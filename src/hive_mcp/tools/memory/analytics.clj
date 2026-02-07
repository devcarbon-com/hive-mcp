(ns hive-mcp.tools.memory.analytics
  "Analytics handlers for memory feedback and usage tracking.

   SOLID: SRP - Single responsibility for usage analytics.
   CLARITY: T - Telemetry first with access tracking.

   Handlers:
   - log-access: Track when entries are accessed (with cross-project detection W5)
   - feedback: Record helpfulness feedback
   - helpfulness-ratio: Calculate entry usefulness ratio"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.chroma :as chroma]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.time ZonedDateTime]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Access Tracking Handler
;; ============================================================

(defn- xpoll-tag
  "Create a cross-pollination tag for a project-id.
   Format: xpoll:project:<project-id>"
  [project-id]
  (str "xpoll:project:" project-id))

(defn- has-xpoll-tag?
  "Check if entry already has cross-pollination tag for given project."
  [entry project-id]
  (let [tag (xpoll-tag project-id)]
    (some #(= % tag) (or (:tags entry) []))))

(defn- detect-cross-project-access
  "Detect if this access is from a different project than the entry's origin.

   Returns the accessing project-id if cross-project, nil otherwise."
  [entry accessing-project-id]
  (when (and accessing-project-id
             (not (str/blank? accessing-project-id))
             (not= accessing-project-id "global")
             (not= accessing-project-id (:project-id entry)))
    accessing-project-id))

(defn handle-log-access
  "Log access to a memory entry (Chroma-only).
   Increments access-count and updates last-accessed timestamp.

   W5 Cross-Pollination: When directory is provided, detects cross-project access
   and adds xpoll:project:<id> tag to track which projects use this entry.
   This feeds into cross-pollination auto-promotion scoring in crystal/core.clj."
  [{:keys [id directory]}]
  (log/info "mcp-memory-log-access:" id "directory:" directory)
  (with-chroma
    (if-let [entry (chroma/get-entry-by-id id)]
      (let [new-count (inc (or (:access-count entry) 0))
            accessing-project (when directory
                                (scope/get-current-project-id directory))
            cross-project-id (detect-cross-project-access entry accessing-project)
            ;; Base updates: access count + timestamp
            base-updates {:access-count new-count
                          :last-accessed (str (ZonedDateTime/now))}
            ;; W5: Add xpoll tag if cross-project and not already tagged
            updates (if (and cross-project-id
                             (not (has-xpoll-tag? entry cross-project-id)))
                      (let [new-tags (conj (vec (or (:tags entry) []))
                                           (xpoll-tag cross-project-id))
                            tag-str (str/join "," new-tags)]
                        (log/info "Cross-pollination detected: entry" id
                                  "accessed from project" cross-project-id
                                  "(origin:" (:project-id entry) ")")
                        (assoc base-updates :tags tag-str))
                      base-updates)
            updated (chroma/update-entry! id updates)]
        (mcp-json (merge (fmt/entry->json-alist updated)
                         (when cross-project-id
                           {:cross_pollination {:detected true
                                                :accessing_project cross-project-id
                                                :origin_project (:project-id entry)}}))))
      (mcp-json {:error "Entry not found"}))))

;; ============================================================
;; Feedback Handler
;; ============================================================

(defn handle-feedback
  "Submit helpfulness feedback for a memory entry (Chroma-only).
   feedback should be 'helpful' or 'unhelpful'."
  [{:keys [id feedback]}]
  (log/info "mcp-memory-feedback:" id feedback)
  (with-chroma
    (if-let [entry (chroma/get-entry-by-id id)]
      (let [updates (case feedback
                      "helpful" {:helpful-count (inc (or (:helpful-count entry) 0))}
                      "unhelpful" {:unhelpful-count (inc (or (:unhelpful-count entry) 0))}
                      (throw (ex-info "Invalid feedback type" {:feedback feedback})))
            updated (chroma/update-entry! id updates)]
        (mcp-json (fmt/entry->json-alist updated)))
      (mcp-json {:error "Entry not found"}))))

;; ============================================================
;; Helpfulness Ratio Handler
;; ============================================================

(defn handle-helpfulness-ratio
  "Get helpfulness ratio for a memory entry (Chroma-only).
   Returns helpful/(helpful+unhelpful) or null if no feedback."
  [{:keys [id]}]
  (log/info "mcp-memory-helpfulness-ratio:" id)
  (with-chroma
    (if-let [entry (chroma/get-entry-by-id id)]
      (let [helpful (or (:helpful-count entry) 0)
            unhelpful (or (:unhelpful-count entry) 0)
            total (+ helpful unhelpful)
            ratio (when (pos? total) (/ (double helpful) total))]
        (mcp-json {:ratio ratio
                   :helpful helpful
                   :unhelpful unhelpful}))
      (mcp-json {:error "Entry not found"}))))
