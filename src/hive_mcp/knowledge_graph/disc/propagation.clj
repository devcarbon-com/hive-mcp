(ns hive-mcp.knowledge-graph.disc.propagation
  "Certainty event wiring and transitive staleness propagation.

   Manages:
   - Bayesian certainty updates (wiring observation events to disc entities)
   - Re-grounding (verifying disc entities against actual files)
   - Time decay application (periodic certainty degradation)
   - Transitive staleness propagation via KG edges

   Extracted from disc.clj (Sprint 2 decomposition).

   CLARITY-Y: Status codes instead of exceptions for all outcomes."
  (:require [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.queries :as queries]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.disc.hash :as hash]
            [hive-mcp.knowledge-graph.disc.crud :as crud]
            [hive-mcp.knowledge-graph.disc.volatility :as vol]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Forward declaration for mutual reference
(declare propagate-staleness!)

;; =============================================================================
;; Certainty Event Wiring
;; =============================================================================
;;
;; These functions wire observation events into certainty updates automatically.
;; - reground-disc! triggers :read-confirmed or :hash-mismatch
;; - on-git-commit-touched triggers :git-commit-touched

(defn update-disc-certainty!
  "Update certainty for a disc entity based on observation event and persist.

   This is the central function for wiring observation events to certainty updates.
   It handles both the Bayesian update and persistence in one atomic operation.

   Arguments:
     path  - File path of the disc entity
     event - Observation event keyword:
             :read-confirmed, :hash-mismatch, :git-commit-touched, :time-decay

   Returns:
     Updated disc entity map, or nil if disc not found.

   CLARITY-Y: Returns nil instead of throwing for missing discs."
  [path event]
  {:pre [(string? path) (keyword? event)]}
  (when-let [disc (crud/get-disc path)]
    (let [updated (vol/update-certainty disc event)
          now (java.util.Date.)
          certainty-updates {:disc/certainty-alpha (:disc/certainty-alpha updated)
                             :disc/certainty-beta (:disc/certainty-beta updated)
                             :disc/last-observation now}]
      (crud/update-disc! path certainty-updates)
      (log/debug "Updated disc certainty" {:path path
                                           :event event
                                           :new-certainty (vol/current-certainty updated)})
      (crud/get-disc path))))

(defn reground-disc!
  "Re-ground a disc entity by verifying against the actual file on disk.

   Compares stored content-hash with current file hash:
   - If hashes match: triggers :read-confirmed (alpha += 3)
   - If hashes differ: triggers :hash-mismatch (beta += 5) and updates stored hash

   Also updates last-observation timestamp for time decay calculations.

   Arguments:
     path        - File path of the disc entity
     git-commit  - Optional git commit hash for tracking

   Returns:
     {:status    :regrounded|:hash-mismatch|:file-missing|:not-found
      :disc      Updated disc entity (if successful)
      :certainty New certainty value
      :old-hash  Previous stored hash
      :new-hash  Current file hash}

   CLARITY-Y: Status codes instead of exceptions for all outcomes."
  [path & {:keys [git-commit]}]
  {:pre [(string? path)]}
  (let [disc (crud/get-disc path)]
    (cond
      ;; Disc not found
      (nil? disc)
      {:status :not-found :path path}

      ;; Check file
      :else
      (let [{:keys [hash exists?]} (hash/file-content-hash path)
            stored-hash (:disc/content-hash disc)]
        (cond
          ;; File doesn't exist
          (not exists?)
          {:status :file-missing :path path :disc disc}

          ;; Hashes match - content confirmed
          (= hash stored-hash)
          (let [updated-disc (update-disc-certainty! path :read-confirmed)
                ;; Also update analyzed-at
                _ (crud/update-disc! path (merge {:disc/analyzed-at (java.util.Date.)}
                                                 (when git-commit {:disc/git-commit git-commit})))
                ;; Propagate staleness if git-commit provided
                _ (when git-commit (propagate-staleness! path 2.0 :git-commit))]
            {:status :regrounded
             :disc (crud/get-disc path)
             :certainty (vol/current-certainty updated-disc)
             :old-hash stored-hash
             :new-hash hash})

          ;; Hash mismatch - content changed
          :else
          (let [_ (update-disc-certainty! path :hash-mismatch)
                ;; Update with new hash and timestamp
                _ (crud/update-disc! path (merge {:disc/content-hash hash
                                                  :disc/analyzed-at (java.util.Date.)}
                                                 (when git-commit {:disc/git-commit git-commit})))
                _ (propagate-staleness! path 5.0 :hash-mismatch)
                updated-disc (crud/get-disc path)]
            {:status :hash-mismatch
             :disc updated-disc
             :certainty (vol/current-certainty updated-disc)
             :old-hash stored-hash
             :new-hash hash}))))))

(defn on-git-commit-touched
  "Called when a git commit affects a disc's file.

   This is an integration point for git hooks or watchers to notify
   the knowledge graph that a file has been modified by a commit.

   The certainty is updated with :git-commit-touched event (beta += 2),
   signaling moderate evidence that the file content may have changed.

   Arguments:
     path       - File path that was touched by the commit
     git-commit - Git commit hash (optional, for tracking)

   Returns:
     {:status :updated|:not-found|:created
      :disc   Updated/created disc entity
      :certainty New certainty value}

   If the disc doesn't exist yet, creates it with the git commit info."
  [path & {:keys [git-commit]}]
  {:pre [(string? path)]}
  (if-let [_disc (crud/get-disc path)]
    ;; Existing disc - update certainty
    (let [updated-disc (update-disc-certainty! path :git-commit-touched)
          ;; Also update git-commit if provided
          _ (when git-commit
              (crud/update-disc! path {:disc/git-commit git-commit}))
          ;; Propagate staleness from git commit event
          _ (propagate-staleness! path 2.0 :git-commit)]
      {:status :updated
       :disc (crud/get-disc path)
       :certainty (vol/current-certainty updated-disc)})
    ;; No disc yet - create one
    (let [{:keys [hash exists?]} (hash/file-content-hash path)]
      (when exists?
        (crud/add-disc! {:path path
                         :content-hash hash
                         :git-commit (or git-commit "")
                         :project-id "global"})
        ;; Apply git-commit-touched event to the new disc
        (let [updated-disc (update-disc-certainty! path :git-commit-touched)]
          {:status :created
           :disc updated-disc
           :certainty (vol/current-certainty updated-disc)})))))

(defn reground-stale-discs!
  "Re-ground all discs that need verification.

   Iterates through discs where certainty has fallen below threshold
   and triggers reground-disc! for each.

   Arguments:
     threshold  - Certainty threshold (default: 0.7)
     project-id - Optional project filter
     limit      - Max discs to reground (default: 50)

   Returns:
     {:processed int
      :regrounded int
      :mismatches int
      :missing int
      :errors int
      :results [...]}

   CLARITY-A: Batched processing with configurable limits."
  [& {:keys [threshold project-id limit] :or {threshold 0.7 limit 50}}]
  (let [discs (crud/get-all-discs :project-id project-id)
        needs-reground (filter #(vol/needs-read? % threshold) discs)
        to-process (take limit needs-reground)
        results (doall
                 (for [disc to-process]
                   (try
                     (reground-disc! (:disc/path disc))
                     (catch Exception e
                       {:status :error
                        :path (:disc/path disc)
                        :error (.getMessage e)}))))
        by-status (frequencies (map :status results))]
    (log/info "Reground stale discs complete"
              {:processed (count to-process)
               :by-status by-status})
    {:processed (count to-process)
     :regrounded (get by-status :regrounded 0)
     :mismatches (get by-status :hash-mismatch 0)
     :missing (get by-status :file-missing 0)
     :errors (get by-status :error 0)
     :results results}))

;; =============================================================================
;; Time Decay (Batch)
;; =============================================================================

(defn apply-time-decay-to-all-discs!
  "Apply time decay to all disc entities in DataScript.

   Iterates through all discs and applies time-based certainty decay
   based on each disc's volatility class. Persists updates to DataScript.

   Arguments:
     project-id - Optional project filter (nil = all projects)

   Returns:
     {:updated int :skipped int :errors int}

   Use for periodic background maintenance of certainty scores."
  [& {:keys [project-id]}]
  (let [discs (crud/get-all-discs :project-id project-id)
        results (reduce
                 (fn [acc disc]
                   (try
                     (let [path (:disc/path disc)
                           decayed (vol/apply-time-decay disc)
                           ;; Only persist the certainty fields
                           updates {:disc/certainty-beta (:disc/certainty-beta decayed)
                                    :disc/last-observation (:disc/last-observation decayed)}]
                       (crud/update-disc! path updates)
                       (update acc :updated inc))
                     (catch Exception e
                       (log/warn "Failed to apply decay to disc"
                                 {:path (:disc/path disc) :error (.getMessage e)})
                       (update acc :errors inc))))
                 {:updated 0 :skipped 0 :errors 0}
                 discs)]
    (log/info "Time decay applied to discs" results)
    results))

;; =============================================================================
;; L1-P2 Transitive Staleness Propagation
;; =============================================================================

(defn apply-transitive-staleness!
  "Apply staleness to a single Chroma entry with decay based on depth.

   Arguments:
     entry-id         - Chroma entry ID to update
     base-staleness   - Base staleness value before decay
     depth            - Propagation depth (0 = source, 1 = direct dependent, etc.)
     staleness-source - Source event type keyword

   Returns:
     {:status :updated|:skipped|:error
      :entry-id entry-id
      :beta-increment amount added to staleness-beta}

   Skips update if decayed staleness is below staleness-min-threshold."
  [entry-id base-staleness depth staleness-source]
  (let [beta-increment (* base-staleness
                          (Math/pow vol/staleness-decay-factor depth))]
    (if (< beta-increment vol/staleness-min-threshold)
      {:status :skipped :entry-id entry-id :beta-increment 0}
      (try
        (let [current-entry (chroma/get-entry-by-id entry-id)
              current-beta (or (:staleness-beta current-entry) 1.0)
              new-beta (+ current-beta beta-increment)]
          (chroma/update-entry! entry-id
                                {:staleness-beta new-beta
                                 :staleness-depth depth
                                 :staleness-source (name staleness-source)})
          (log/debug "Applied transitive staleness"
                     {:entry-id entry-id :depth depth :beta-increment beta-increment})
          {:status :updated :entry-id entry-id :beta-increment beta-increment})
        (catch Exception e
          (log/warn "Failed to apply staleness to entry"
                    {:entry-id entry-id :error (.getMessage e)})
          {:status :error :entry-id entry-id :error (.getMessage e)})))))

(defn propagate-staleness!
  "Propagate staleness from a disc to dependent labels via KG edges.
   Uses Bayesian beta update with 0.5^depth decay.

   Algorithm:
   1. Query Chroma for entries WHERE grounded-from = disc-path
   2. For each grounded entry, update it at depth 0 (source)
   3. Call impact-analysis to find dependents via propagation-relations
   4. Update direct dependents at depth 1
   5. Update transitive dependents at depth 2 (simplified - actual depth may vary)
   6. Only propagate if beta-increment >= staleness-min-threshold

   Arguments:
     disc-path        - File path of the disc entity triggering staleness
     base-staleness   - Base staleness value (from base-staleness-values map)
     staleness-source - Source event type (e.g., :hash-mismatch, :git-commit, :time-decay)

   Returns:
     {:propagated int :skipped int :errors int :grounded int}"
  [disc-path base-staleness staleness-source]
  (try
    ;; Step 1: Query Chroma for entries grounded from this disc
    (let [grounded-entries (chroma/query-grounded-from disc-path)
          results (atom {:propagated 0 :skipped 0 :errors 0 :grounded 0})]

      (doseq [entry grounded-entries]
        (let [entry-id (:id entry)]
          ;; Step 2: Update the grounded entry itself at depth 0
          (let [result (apply-transitive-staleness! entry-id base-staleness 0 staleness-source)]
            (case (:status result)
              :updated (swap! results update :grounded inc)
              :skipped (swap! results update :skipped inc)
              :error (swap! results update :errors inc)))

          ;; Step 3: Run impact analysis to find dependents
          (try
            (let [{:keys [direct transitive]}
                  (queries/impact-analysis entry-id
                                           {:max-depth vol/staleness-max-depth})]

              ;; Step 4: Update direct dependents at depth 1
              (doseq [dep-id direct]
                (let [result (apply-transitive-staleness! dep-id base-staleness 1 staleness-source)]
                  (case (:status result)
                    :updated (swap! results update :propagated inc)
                    :skipped (swap! results update :skipped inc)
                    :error (swap! results update :errors inc))))

              ;; Step 5: Update transitive dependents at depth 2
              (doseq [trans-id transitive]
                (let [result (apply-transitive-staleness! trans-id base-staleness 2 staleness-source)]
                  (case (:status result)
                    :updated (swap! results update :propagated inc)
                    :skipped (swap! results update :skipped inc)
                    :error (swap! results update :errors inc)))))

            (catch Exception e
              (log/warn "Impact analysis failed for entry"
                        {:entry-id entry-id :error (.getMessage e)})
              (swap! results update :errors inc)))))

      (log/info "Staleness propagation complete"
                {:disc-path disc-path :results @results})
      @results)

    (catch Exception e
      (log/warn "Failed to propagate staleness"
                {:disc-path disc-path :error (.getMessage e)})
      {:propagated 0 :skipped 0 :errors 1 :grounded 0})))

;; =============================================================================
;; L1-P2 Chroma Entry Staleness Report
;; =============================================================================

(defn stale-entries-report
  "Query Chroma for entries with staleness above threshold.

   Arguments:
     threshold  - Minimum staleness score (default: 0.3)
     limit      - Max entries to return (default: 20)
     project-id - Optional project filter

   Returns:
     Vector of {:id :score :source :depth :grounded-from} sorted by score desc"
  [& {:keys [threshold limit project-id] :or {threshold 0.3 limit 20}}]
  (try
    (let [entries (chroma/query-entries :project-id project-id :limit 1000)]
      (->> entries
           (map vol/entry-staleness-report)
           (filter #(> (:score %) threshold))
           (sort-by :score >)
           (take limit)
           vec))
    (catch Exception e
      (log/warn "Failed to generate stale entries report" {:error (.getMessage e)})
      [])))
