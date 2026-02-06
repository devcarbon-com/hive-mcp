(ns hive-mcp.knowledge-graph.disc.staleness
  "Staleness surfacing and KG-first context for disc entities.

   Contains impure wrappers around pure volatility scoring that
   perform file I/O, plus proactive staleness reporting and
   KG-first context classification.

   Extracted from disc.clj (Sprint 2 decomposition).

   CLARITY-Y: Graceful failure with status codes."
  (:require [hive-mcp.knowledge-graph.disc.hash :as hash]
            [hive-mcp.knowledge-graph.disc.crud :as crud]
            [hive-mcp.knowledge-graph.disc.volatility :as vol]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Impure Staleness Wrappers (perform file I/O)
;; =============================================================================

(defn staleness-score
  "Compute staleness score for a disc entity.
   Score ranges from 0.0 (fresh) to 1.0 (very stale).

   This is the impure wrapper that reads the file to check hash.
   For the pure version, use volatility/staleness-score with a pre-computed hash-result.

   Arguments:
     disc - Disc entity map

   Returns:
     Float score 0.0-1.0"
  [disc]
  (let [path (:disc/path disc)
        hash-result (when path (hash/file-content-hash path))]
    (vol/staleness-score disc hash-result)))

(defn staleness-report
  "Compute staleness score and diagnostic info for a disc entity in one pass.
   Avoids redundant file I/O compared to calling staleness-score separately.

   Returns {:score :days-since-read :hash-mismatch? :never-analyzed?}"
  [disc]
  (let [path (:disc/path disc)
        hash-result (when path (hash/file-content-hash path))]
    (vol/staleness-report disc hash-result)))

;; =============================================================================
;; L1 Disc Surfacing — Proactive Staleness Reporting
;; =============================================================================

(defn staleness-warnings
  "Generate staleness warnings for a collection of file paths.
   Only returns warnings for files with existing disc entities that are stale.
   Fresh files and files without disc entities produce no output (zero noise).

   Arguments:
     paths - Collection of file path strings

   Returns:
     Vector of {:path :staleness :message} for stale files only (staleness > 0.3)"
  [paths]
  (when (seq paths)
    (->> paths
         (keep (fn [path]
                 (when-let [disc (crud/get-disc path)]
                   (let [{:keys [score days-since-read hash-mismatch?]}
                         (staleness-report disc)]
                     (when (> score 0.3)
                       {:path path
                        :staleness score
                        :message (format "NOTE: file %s is stale (staleness: %.1f%s%s). Re-read carefully."
                                         path
                                         (float score)
                                         (if days-since-read
                                           (format ", last read %dd ago" days-since-read)
                                           ", never read")
                                         (if hash-mismatch?
                                           ", hash mismatch"
                                           ""))})))))
         vec)))

(defn top-stale-files
  "Query top-N most stale disc entities.
   Returns vector of {:path :score :days-since-read :hash-mismatch?}
   sorted by staleness score descending.

   Arguments:
     n          - Max entries to return (default: 5)
     project-id - Optional project filter
     threshold  - Minimum staleness score (default: 0.5)"
  [& {:keys [n project-id threshold] :or {n 5 threshold 0.5}}]
  (let [discs (crud/get-all-discs :project-id project-id)]
    (->> discs
         (map (fn [disc]
                (let [report (staleness-report disc)]
                  (assoc report :path (:disc/path disc)))))
         (filter #(> (:score %) threshold))
         (sort-by :score >)
         (take n)
         vec)))

;; =============================================================================
;; KG-First Context — Read the Map Before the Territory
;; =============================================================================

(defn- classify-disc
  "Classify a single file path's KG status.
   Returns a map with :path, :disc, :status (:fresh | :stale | :missing),
   and diagnostic fields.

   Staleness threshold: files with score <= threshold are considered fresh.
   Default threshold: 0.3 (matches staleness-warnings threshold)."
  [path staleness-threshold]
  (try
    (if-let [disc (crud/get-disc path)]
      (let [{:keys [score days-since-read hash-mismatch? never-analyzed?]}
            (staleness-report disc)]
        {:path path
         :disc disc
         :status (if (<= score staleness-threshold) :fresh :stale)
         :staleness-score score
         :days-since-read days-since-read
         :hash-mismatch? hash-mismatch?
         :never-analyzed? never-analyzed?
         :read-count (or (:disc/read-count disc) 0)
         :last-read-at (:disc/last-read-at disc)})
      ;; No disc entity — file has never been tracked
      {:path path
       :disc nil
       :status :missing
       :staleness-score 1.0
       :days-since-read nil
       :hash-mismatch? false
       :never-analyzed? true
       :read-count 0
       :last-read-at nil})
    (catch Exception e
      (log/warn "KG classify-disc failed, treating as missing"
                {:path path :error (.getMessage e)})
      {:path path
       :disc nil
       :status :missing
       :staleness-score 1.0
       :days-since-read nil
       :hash-mismatch? false
       :never-analyzed? true
       :read-count 0
       :last-read-at nil})))

(defn kg-first-context
  "Consult the KG before file reads — the Structural Differential principle.
   Labels (KG) before disc (files).

   Takes a collection of file paths and classifies each based on KG freshness:
   - :kg-known  — Files with fresh KG data (skip file read, use KG knowledge)
   - :needs-read — Files with no KG data (must read from disc)
   - :stale     — Files with stale KG data (should re-read from disc)

   Arguments:
     paths - Collection of file path strings
     opts  - Optional map:
       :staleness-threshold - Score cutoff for fresh vs stale (default: 0.3)
                              0.0 = only perfectly fresh, 1.0 = everything is fresh

   Returns:
     {:kg-known   {path {:disc ... :staleness-score ... :read-count ...} ...}
      :needs-read [path ...]
      :stale      [path ...]
      :summary    {:total N :known N :needs-read N :stale N}}

   CLARITY-Y: Individual file failures are logged and treated as :needs-read.
   CLARITY-A: Single-pass classification, no redundant file I/O."
  [paths & [{:keys [staleness-threshold] :or {staleness-threshold 0.3}}]]
  (let [unique-paths (distinct (filter (every-pred string? seq) paths))
        classified (mapv #(classify-disc % staleness-threshold) unique-paths)
        grouped (group-by :status classified)
        ;; Build the result maps
        kg-known (->> (:fresh grouped)
                      (reduce (fn [m entry]
                                (assoc m (:path entry)
                                       (dissoc entry :path :status)))
                              {}))
        needs-read (mapv :path (:missing grouped))
        stale (mapv :path (:stale grouped))]
    {:kg-known kg-known
     :needs-read needs-read
     :stale stale
     :summary {:total (count unique-paths)
               :known (count kg-known)
               :needs-read (count needs-read)
               :stale (count stale)}}))
