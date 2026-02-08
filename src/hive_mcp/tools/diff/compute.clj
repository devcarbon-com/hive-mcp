(ns hive-mcp.tools.diff.compute
  "Pure diff computation: hunks, metrics, formatting.

   Token-Efficient Hunk-Based Diff (ADR 20260125002853):
   Uses java-diff-utils for git-style hunks.

   SOLID: SRP - Pure functions, no state or I/O."
  (:require [clojure.string :as str])
  (:import [com.github.difflib DiffUtils]
           [com.github.difflib.patch DeltaType AbstractDelta]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Domain: Diff Operations (Pure Functions)
;; =============================================================================

(defn generate-diff-id
  "Generate a unique diff ID."
  []
  (str "diff-" (System/currentTimeMillis) "-" (rand-int 10000)))

(defn compute-unified-diff
  "Compute a unified diff between old and new content.
   Returns a string in unified diff format.

   DEPRECATED: Use compute-hunks for token-efficient diff storage.
   This naive implementation marks ALL lines as changed."
  [old-content new-content file-path]
  (let [old-lines (str/split-lines old-content)
        new-lines (str/split-lines new-content)
        ;; Simple line-by-line diff (not full Myers algorithm, but sufficient)
        header (str "--- a/" file-path "\n+++ b/" file-path "\n@@ -1,"
                    (count old-lines) " +1," (count new-lines) " @@\n")]
    (str header
         (str/join "\n"
                   (concat
                    (map #(str "-" %) old-lines)
                    (map #(str "+" %) new-lines))))))

;; =============================================================================
;; Token-Efficient Hunk-Based Diff (ADR 20260125002853)
;; =============================================================================

(defn- delta-type->keyword
  "Convert Java DeltaType enum to Clojure keyword."
  [^DeltaType dt]
  (condp = dt
    DeltaType/CHANGE :change
    DeltaType/DELETE :delete
    DeltaType/INSERT :insert
    DeltaType/EQUAL  :equal
    :unknown))

(defn- extract-context
  "Extract context lines around a position.
   Returns {:before [lines...] :after [lines...]}."
  [lines position length context-size]
  (let [before-start (max 0 (- position context-size))
        after-end (min (count lines) (+ position length context-size))]
    {:before (vec (subvec (vec lines) before-start position))
     :after (vec (subvec (vec lines) (+ position length) after-end))}))

(defn- delta->hunk
  "Convert a java-diff-utils Delta to a hunk map with context."
  [^AbstractDelta delta old-lines _new-lines context-size]
  (let [source (.getSource delta)
        target (.getTarget delta)
        old-pos (.getPosition source)
        new-pos (.getPosition target)
        old-chunk-lines (vec (.getLines source))
        new-chunk-lines (vec (.getLines target))
        context (extract-context old-lines old-pos (count old-chunk-lines) context-size)]
    {:type (delta-type->keyword (.getType delta))
     :start-old (inc old-pos)  ; 1-indexed for display
     :start-new (inc new-pos)
     :old-lines (count old-chunk-lines)
     :new-lines (count new-chunk-lines)
     :context-before (:before context)
     :removed old-chunk-lines
     :added new-chunk-lines
     :context-after (:after context)}))

(defn compute-hunks
  "Compute git-style hunks using java-diff-utils.
   Returns vector of hunk maps with context.

   Each hunk contains:
   - :type - :change, :delete, :insert
   - :start-old, :start-new - 1-indexed line positions
   - :old-lines, :new-lines - count of lines in each version
   - :context-before, :context-after - surrounding lines
   - :removed, :added - actual changed lines

   Options:
   - :context-lines (default 3) - lines of context around changes"
  [old-content new-content & [{:keys [context-lines] :or {context-lines 3}}]]
  (let [old-lines (if (str/blank? old-content) [] (str/split-lines old-content))
        new-lines (if (str/blank? new-content) [] (str/split-lines new-content))
        patch (DiffUtils/diff old-lines new-lines)
        deltas (.getDeltas patch)]
    (mapv #(delta->hunk % old-lines new-lines context-lines) deltas)))

(defn compute-metrics
  "Compute diff metrics from hunks.
   Returns map with :lines-added, :lines-removed, :net-change, :hunks-count."
  [hunks]
  (let [added (reduce + (map :new-lines hunks))
        removed (reduce + (map :old-lines hunks))]
    {:lines-added added
     :lines-removed removed
     :net-change (- added removed)
     :hunks-count (count hunks)}))

(defn format-hunk-as-unified
  "Format a single hunk as unified diff text."
  [{:keys [start-old start-new old-lines new-lines
           context-before removed added context-after]}]
  (let [;; Adjust positions for context
        ctx-before-count (count context-before)
        display-start-old (max 1 (- start-old ctx-before-count))
        display-start-new (max 1 (- start-new ctx-before-count))
        total-old (+ ctx-before-count old-lines (count context-after))
        total-new (+ ctx-before-count new-lines (count context-after))
        header (format "@@ -%d,%d +%d,%d @@"
                       display-start-old total-old
                       display-start-new total-new)]
    (str/join "\n"
              (concat
               [header]
               (map #(str " " %) context-before)
               (map #(str "-" %) removed)
               (map #(str "+" %) added)
               (map #(str " " %) context-after)))))

(defn format-hunks-as-unified
  "Format hunks as human-readable unified diff string.
   Generates proper unified diff with file headers."
  [hunks file-path]
  (if (empty? hunks)
    (str "--- a/" file-path "\n+++ b/" file-path "\n(no changes)")
    (str "--- a/" file-path "\n"
         "+++ b/" file-path "\n"
         (str/join "\n" (map format-hunk-as-unified hunks)))))

(defn create-diff-proposal
  "Create a diff proposal map from input parameters.
   Optionally includes wave-id for batch review tracking.

   Token-Efficient Storage (ADR 20260125002853):
   - Computes and stores :hunks for tier-2 retrieval
   - Computes and stores :metrics for tier-1 listing
   - Stores :old-content/:new-content for apply_diff (tier-3, never returned)
   - :unified-diff removed (replaced by on-demand hunk formatting)"
  [{:keys [file_path old_content new_content description drone_id wave_id]}]
  (let [diff-id (generate-diff-id)
        hunks (compute-hunks old_content new_content)
        metrics (compute-metrics hunks)]
    (cond-> {:id diff-id
             :file-path file_path
             :old-content old_content   ; Tier 3: stored, never returned
             :new-content new_content   ; Tier 3: stored, never returned
             :description (or description "No description provided")
             :drone-id (or drone_id "unknown")
             :hunks hunks               ; Tier 2: returned by get_diff_details
             :metrics metrics           ; Tier 1: returned by list_proposed_diffs
             :tdd-status nil            ; Populated by drone after tests/lint
             :status "pending"
             :created-at (java.time.Instant/now)}
      wave_id (assoc :wave-id wave_id))))
