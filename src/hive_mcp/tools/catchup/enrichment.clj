(ns hive-mcp.tools.catchup.enrichment
  "Knowledge Graph enrichment functions for catchup workflow.

   SOLID: SRP - Single responsibility for KG context enrichment.
   Extracted from hive-mcp.tools.catchup (Sprint 2 refactoring).

   Contains:
   - Session summary traversal (find related entries via :derived-from)
   - Decision relationship traversal (implements, refines, depends-on)
   - Grounding freshness checks (P1.4)
   - Co-access suggestions (KG co-accessed edge mining)
   - KG relation extraction from node context
   - Entry enrichment with KG relationships
   - High-level KG insight gathering"
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.queries :as kg-queries]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Session & Decision Traversal
;; =============================================================================

(defn find-related-via-session-summaries
  "Find entries related to session summaries via :derived-from traversal.
   Session summaries often derive from decisions/conventions made during session.

   Returns vector of related entry IDs."
  [session-ids _project-id]
  (when (seq session-ids)
    (try
      (->> session-ids
           (mapcat (fn [session-id]
                     ;; Traverse incoming :derived-from edges to find source entries
                     ;; Note: Don't pass scope - catchup should see all related entries
                     (let [results (kg-queries/traverse
                                    session-id
                                    {:direction :incoming
                                     :relations #{:derived-from}
                                     :max-depth 2})]
                       (map :node-id results))))
           (distinct)
           (vec))
      (catch Exception e
        (log/debug "Session traversal failed:" (.getMessage e))
        []))))

(defn find-related-decisions-via-kg
  "Find decisions connected via :implements, :refines, or :depends-on relationships.
   These are decisions that have active dependencies in the knowledge graph.

   Returns vector of related decision IDs."
  [decision-ids _project-id]
  (when (seq decision-ids)
    (try
      (->> decision-ids
           (mapcat (fn [decision-id]
                     ;; Look for entries that implement or refine this decision
                     ;; Note: Don't pass scope - catchup should see all related entries
                     (let [results (kg-queries/traverse
                                    decision-id
                                    {:direction :both
                                     :relations #{:implements :refines :depends-on}
                                     :max-depth 2})]
                       (map :node-id results))))
           (distinct)
           (remove (set decision-ids))  ; Exclude original decisions
           (vec))
      (catch Exception e
        (log/debug "Decision traversal failed:" (.getMessage e))
        []))))

;; =============================================================================
;; Grounding Freshness (P1.4)
;; =============================================================================

(defn entry-grounding-age-days
  "Compute age in days since entry was last grounded.
   Returns nil if entry has no grounded-at metadata, or days as long."
  [entry]
  (let [grounded-at (or (get-in entry [:metadata :grounded-at])
                        (:grounded-at entry))]
    (when (and grounded-at (not (str/blank? (str grounded-at))))
      (try
        (let [grounded-inst (cond
                              (instance? java.util.Date grounded-at)
                              (.toInstant grounded-at)

                              (string? grounded-at)
                              (java.time.Instant/parse grounded-at)

                              :else nil)]
          (when grounded-inst
            (.toDays (java.time.Duration/between grounded-inst (java.time.Instant/now)))))
        (catch Exception _ nil)))))

(defn entry->grounding-warning
  "Analyze a single entry for grounding staleness.
   Returns a warning map if entry needs regrounding, nil otherwise.

   Uses manual age check for string timestamps from Chroma."
  [entry max-age-days]
  (let [entry-id (or (:id entry) "unknown")
        entry-type (name (or (:type entry) "unknown"))
        grounded-at (or (get-in entry [:metadata :grounded-at])
                        (:grounded-at entry))
        age-days (entry-grounding-age-days entry)
        never-grounded? (or (nil? grounded-at)
                            (str/blank? (str grounded-at)))
        stale? (or never-grounded?
                   (and age-days (> age-days max-age-days)))]
    (when stale?
      {:id entry-id
       :type entry-type
       :grounded-at (when-not never-grounded? (str grounded-at))
       :age-days age-days
       :never-grounded? never-grounded?
       :preview (subs (str (:content entry ""))
                      0 (min (count (str (:content entry ""))) 60))})))

(defn check-grounding-freshness
  "Check grounding freshness of top project entries.

   Queries decisions and conventions (bounded to limit entries each),
   checks each for grounding staleness using needs-regrounding? logic,
   and returns structured warnings.

   Arguments:
     project-id    - Project scope for Chroma queries
     opts          - Optional map:
       :max-age-days  - Staleness threshold in days (default: 7)
       :limit         - Max entries to check per type (default: 20)
       :timeout-ms    - Timeout for the check in ms (default: 5000)

   Returns:
     {:total-checked int
      :stale-count int
      :stale-entries [{:id :type :grounded-at :age-days :never-grounded? :preview}]
      :timed-out? boolean}"
  [project-id & [{:keys [max-age-days limit timeout-ms]
                  :or {max-age-days 7 limit 20 timeout-ms 5000}}]]
  (let [result-future
        (future
          (try
            (let [decisions (chroma/query-entries :type "decision"
                                                  :project-id project-id
                                                  :limit limit)
                  conventions (chroma/query-entries :type "convention"
                                                    :project-id project-id
                                                    :limit limit)
                  all-entries (take (* 2 limit) (concat decisions conventions))
                  warnings (->> all-entries
                                (keep #(entry->grounding-warning % max-age-days))
                                (vec))]
              {:total-checked (count all-entries)
               :stale-count (count warnings)
               :stale-entries warnings
               :timed-out? false})
            (catch Exception e
              (log/debug "Grounding freshness check failed:" (.getMessage e))
              {:total-checked 0
               :stale-count 0
               :stale-entries []
               :error (.getMessage e)
               :timed-out? false})))]
    (let [result (deref result-future timeout-ms ::timeout)]
      (if (= result ::timeout)
        (do
          (future-cancel result-future)
          (log/warn "Grounding freshness check timed out after" timeout-ms "ms")
          {:total-checked 0
           :stale-count 0
           :stale-entries []
           :timed-out? true})
        result))))

;; =============================================================================
;; Co-Access Suggestions
;; =============================================================================

(defn find-co-accessed-suggestions
  "Find memory entries frequently co-accessed with the given entries.
   Uses :co-accessed edges in the Knowledge Graph to surface related entries
   that aren't already in the catchup result set.

   Arguments:
     entry-ids       - IDs of entries already surfaced in catchup
     exclude-ids     - IDs to exclude from suggestions (already visible)

   Returns vector of {:entry-id <id> :confidence <score>}"
  [entry-ids exclude-ids]
  (when (seq entry-ids)
    (try
      (let [excluded (set exclude-ids)
            ;; For each entry, find co-accessed entries
            co-accessed (->> entry-ids
                             (mapcat (fn [eid]
                                       (kg-edges/get-co-accessed eid)))
                             ;; Exclude entries already in the result set
                             (remove #(contains? excluded (:entry-id %)))
                             ;; Group by entry-id and take max confidence
                             (group-by :entry-id)
                             (map (fn [[eid entries]]
                                    {:entry-id eid
                                     :confidence (apply max (map :confidence entries))
                                     :co-access-count (count entries)}))
                             ;; Sort by co-access count * confidence for relevance
                             (sort-by (fn [{:keys [confidence co-access-count]}]
                                        (* confidence co-access-count))
                                      >)
                             (take 5)
                             vec)]
        (when (seq co-accessed)
          (log/debug "Found" (count co-accessed) "co-accessed suggestions"))
        co-accessed)
      (catch Exception e
        (log/debug "Co-access suggestions failed:" (.getMessage e))
        []))))

;; =============================================================================
;; KG Relation Extraction
;; =============================================================================

(defn extract-kg-relations
  "Extract meaningful KG relationships from node context.

   Returns map with:
   - :supersedes - entries this replaces (outgoing :supersedes)
   - :superseded-by - entries that replace this (incoming :supersedes)
   - :depends-on - prerequisites (outgoing :depends-on)
   - :depended-by - entries depending on this (incoming :depends-on)
   - :derived-from - source materials (outgoing :derived-from)
   - :contradicts - conflicting knowledge (both directions)"
  [{:keys [incoming outgoing]}]
  (let [;; Helper to extract node IDs by relation from edge list
        extract-by-rel (fn [edges rel from?]
                         (->> (:edges edges)
                              (filter #(= (:kg-edge/relation %) rel))
                              (map #(if from?
                                      (:kg-edge/from %)
                                      (:kg-edge/to %)))
                              (vec)))
        ;; Outgoing relations (this node -> others)
        supersedes (extract-by-rel outgoing :supersedes false)
        depends-on (extract-by-rel outgoing :depends-on false)
        derived-from (extract-by-rel outgoing :derived-from false)
        ;; Incoming relations (others -> this node)
        superseded-by (extract-by-rel incoming :supersedes true)
        depended-by (extract-by-rel incoming :depends-on true)
        ;; Contradictions (both directions)
        contradicts-out (extract-by-rel outgoing :contradicts false)
        contradicts-in (extract-by-rel incoming :contradicts true)
        contradicts (vec (distinct (concat contradicts-out contradicts-in)))]
    ;; Only include non-empty relations
    (cond-> {}
      (seq supersedes) (assoc :supersedes supersedes)
      (seq superseded-by) (assoc :superseded-by superseded-by)
      (seq depends-on) (assoc :depends-on depends-on)
      (seq depended-by) (assoc :depended-by depended-by)
      (seq derived-from) (assoc :derived-from derived-from)
      (seq contradicts) (assoc :contradicts contradicts))))

;; =============================================================================
;; Entry Enrichment
;; =============================================================================

(defn enrich-entry-with-kg
  "Enrich a single entry with its KG relationships.
   Returns entry with :kg key if relationships exist."
  [entry]
  (try
    (when-let [entry-id (:id entry)]
      (let [context (kg-queries/get-node-context entry-id)
            relations (extract-kg-relations context)]
        (if (seq relations)
          (assoc entry :kg relations)
          entry)))
    (catch Exception e
      (log/debug "KG enrichment failed for" (:id entry) ":" (.getMessage e))
      entry)))

(defn enrich-entries-with-kg
  "Enrich a collection of entries with KG context.
   Only entries with KG relationships will have :kg key added.

   Returns {:entries [...] :kg-count n :warnings []}"
  [entries]
  (try
    (let [enriched (mapv enrich-entry-with-kg entries)
          kg-count (count (filter :kg enriched))]
      (when (pos? kg-count)
        (log/debug "Enriched" kg-count "entries with KG context"))
      {:entries enriched
       :kg-count kg-count})
    (catch Exception e
      (log/warn "KG enrichment failed:" (.getMessage e))
      {:entries entries
       :kg-count 0
       :warnings [(.getMessage e)]})))

;; =============================================================================
;; High-Level KG Insights
;; =============================================================================

(defn gather-kg-insights
  "Gather high-level KG insights for catchup summary.

   Returns comprehensive KG context including:
   - :edge-count - total edges in KG
   - :by-relation - breakdown by relation type
   - :contradictions - entries with conflicts that need attention
   - :superseded - entries that have been replaced (may need cleanup)
   - :dependency-chains - count of entries with dependency relationships
   - :related-decisions - decisions connected via KG traversal
   - :grounding-warnings - structured warnings for stale/ungrounded entries (P1.4)"
  [decisions-meta conventions-meta sessions-meta project-id]
  (try
    (let [;; Always query KG stats first - gives overview even without enriched entries
          kg-stats (kg-edges/edge-stats)
          edge-count (:total-edges kg-stats)
          by-relation (:by-relation kg-stats)

          ;; Extract IDs for traversal
          session-ids (mapv :id sessions-meta)
          decision-ids (mapv :id decisions-meta)

          ;; Find related entries via KG traversal
          related-from-sessions (find-related-via-session-summaries session-ids project-id)
          related-decisions (find-related-decisions-via-kg decision-ids project-id)

          ;; Check grounding freshness (bounded, with timeout)
          grounding-check (check-grounding-freshness project-id
                                                     {:max-age-days 7
                                                      :limit 20
                                                      :timeout-ms 5000})

          ;; Find entries with concerning relationships from enriched data
          all-entries (concat decisions-meta conventions-meta)
          contradictions (->> all-entries
                              (filter #(seq (get-in % [:kg :contradicts])))
                              (mapv #(select-keys % [:id :preview :kg])))
          superseded (->> all-entries
                          (filter #(seq (get-in % [:kg :superseded-by])))
                          (mapv #(select-keys % [:id :preview :kg])))
          ;; Find entries with dependencies (decision chains)
          with-deps (->> all-entries
                         (filter #(or (seq (get-in % [:kg :depends-on]))
                                      (seq (get-in % [:kg :depended-by]))))
                         (count))

          ;; L1 Disc: Surface stale files (staleness > 0.5) for catchup awareness
          stale-files (try
                        (kg-disc/top-stale-files :n 10 :project-id project-id :threshold 0.5)
                        (catch Exception e
                          (log/debug "KG insights stale-files query failed:" (.getMessage e))
                          []))]

      ;; Build insights map - always include edge-count for visibility
      (cond-> {:edge-count edge-count}
        (seq by-relation) (assoc :by-relation by-relation)
        (seq contradictions) (assoc :contradictions contradictions)
        (seq superseded) (assoc :superseded superseded)
        (pos? with-deps) (assoc :dependency-chains with-deps)
        (seq related-from-sessions) (assoc :session-derived related-from-sessions)
        (seq related-decisions) (assoc :related-decisions related-decisions)
        ;; P1.4: Replace count-only with rich grounding warnings
        (pos? (:stale-count grounding-check)) (assoc :grounding-warnings grounding-check)
        (seq stale-files) (assoc :stale-files stale-files)))
    (catch Exception e
      (log/warn "KG insights gathering failed:" (.getMessage e))
      {:edge-count 0 :error (.getMessage e)})))
