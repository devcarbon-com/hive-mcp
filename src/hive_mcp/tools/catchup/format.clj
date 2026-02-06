(ns hive-mcp.tools.catchup.format
  "Formatting and rendering functions for catchup workflow.

   SOLID: SRP - Single responsibility for output formatting.
   Extracted from hive-mcp.tools.catchup (Sprint 1 refactoring).

   Contains:
   - Entry metadata transforms (entry->*-meta)
   - Scope display builders
   - Response structure builders (JSON)
   - Spawn context markdown formatters
   - Error response helpers"
  (:require [hive-mcp.knowledge-graph.scope :as kg-scope]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Entry Metadata Transforms
;; =============================================================================

(defn entry->catchup-meta
  "Convert a Chroma entry to catchup metadata format.
   Returns map with :id, :type, :preview, :tags."
  [entry preview-len]
  (let [content (:content entry)
        content-str (if (string? content)
                      content
                      (str content))
        preview (subs content-str 0 (min (count content-str) (or preview-len 80)))]
    {:id (:id entry)
     :type (name (or (:type entry) "note"))
     :preview preview
     :tags (vec (or (:tags entry) []))}))

(defn entry->axiom-meta
  "Convert entry to axiom metadata with full content."
  [entry]
  {:id (:id entry)
   :type "axiom"
   :tags (vec (or (:tags entry) []))
   :content (:content entry)
   :severity "INVIOLABLE"})

(defn entry->priority-meta
  "Convert entry to priority convention metadata with full content."
  [entry]
  {:id (:id entry)
   :type "convention"
   :tags (vec (or (:tags entry) []))
   :content (:content entry)})

;; =============================================================================
;; Scope Display
;; =============================================================================

(defn build-scopes
  "Build scope list for display.
   HCR Wave 4: Includes descendant scope info for visibility.
   FIX: Only includes scope:global when actually in global context,
   not when in a project context (prevents misleading scope display)."
  [project-name project-id]
  (let [in-project? (and project-id (not= project-id "global"))
        base (cond-> (if in-project? [] ["scope:global"])
               project-name (conj (str "scope:project:" project-name))
               (and project-id (not= project-id project-name) (not= project-id "global"))
               (conj (str "scope:project:" project-id)))
        ;; HCR Wave 4: Show descendant scopes in display
        descendants (when (and project-id in-project?)
                      (kg-scope/descendant-scopes project-id))]
    (if (seq descendants)
      (into base (map #(str "scope:project:" %) descendants))
      base)))

;; =============================================================================
;; Response Builders
;; =============================================================================

(defn build-catchup-response
  "Build the final catchup response structure."
  [{:keys [project-name project-id scopes git-info permeation
           axioms-meta priority-meta sessions-meta decisions-meta
           conventions-meta snippets-meta expiring-meta kg-insights
           project-tree-scan disc-decay]}]
  {:type "text"
   :text (json/write-str
          {:success true
           :project (or project-name project-id "global")
           :scopes scopes
           :git git-info
           :permeation permeation  ;; Auto-permeated ling wraps
           :counts {:axioms (count axioms-meta)
                    :priority-conventions (count priority-meta)
                    :sessions (count sessions-meta)
                    :decisions (count decisions-meta)
                    :conventions (count conventions-meta)
                    :snippets (count snippets-meta)
                    :expiring (count expiring-meta)}
           :axioms axioms-meta
           :priority-conventions priority-meta
           :context {:sessions sessions-meta
                     :decisions decisions-meta
                     :conventions conventions-meta
                     :snippets snippets-meta
                     :expiring expiring-meta}
           ;; KG insights surface contradictions and superseded entries
           :kg-insights kg-insights
           ;; HCR Wave 2: Project tree staleness scan result
           :project-tree project-tree-scan
           ;; P0.2: Disc certainty decay results
           :disc-decay disc-decay
           :hint "AXIOMS are INVIOLABLE - follow them word-for-word. Priority conventions and axioms loaded with full content. Entries with :kg key have Knowledge Graph relationships. Use mcp_memory_get_full for other entries."})})

(defn chroma-not-configured-error
  "Return error response when Chroma is not configured."
  []
  {:type "text"
   :text (json/write-str {:success false
                          :error "Chroma not configured"
                          :message "Memory query requires Chroma with embedding provider"})
   :isError true})

(defn catchup-error
  "Return error response for catchup failures."
  [e]
  (log/error e "native-catchup failed")
  {:type "text"
   :text (json/write-str {:success false :error (.getMessage e)})
   :isError true})

;; =============================================================================
;; Spawn Context Markdown Formatters
;; =============================================================================

(defn format-spawn-axioms
  "Format axioms section for spawn context markdown."
  [axioms]
  (when (seq axioms)
    (let [lines (map-indexed
                 (fn [idx ax]
                   (format "%d. %s" (inc idx) (:content ax)))
                 axioms)]
      (str "### Axioms (INVIOLABLE \u2014 follow word-for-word)\n\n"
           (str/join "\n\n" lines)
           "\n\n"))))

(defn format-spawn-priorities
  "Format priority conventions section for spawn context markdown."
  [conventions]
  (when (seq conventions)
    (let [lines (map-indexed
                 (fn [idx conv]
                   (format "%d. %s" (inc idx) (:content conv)))
                 conventions)]
      (str "### Priority Conventions\n\n"
           (str/join "\n\n" lines)
           "\n\n"))))

(defn format-spawn-decisions
  "Format active decisions section for spawn context markdown."
  [decisions]
  (when (seq decisions)
    (let [lines (map (fn [d] (format "- %s" (:preview d))) decisions)]
      (str "### Active Decisions\n\n"
           (str/join "\n" lines)
           "\n\n"))))

(defn format-spawn-git
  "Format git status section for spawn context markdown."
  [git-info]
  (when git-info
    (str "### Git Status\n\n"
         (format "- **Branch**: %s\n" (or (:branch git-info) "unknown"))
         (when (:uncommitted git-info)
           "- **Uncommitted changes**: yes\n")
         (format "- **Last commit**: %s\n" (or (:last-commit git-info) "unknown")))))

(defn format-spawn-stale-files
  "Format stale files section for spawn context markdown.
   Surfaces top-N most stale disc entities as files needing re-grounding."
  [stale-files]
  (when (seq stale-files)
    (let [lines (map (fn [{:keys [path score days-since-read hash-mismatch?]}]
                       (format "- `%s` (staleness: %.1f%s%s)"
                               path
                               (float score)
                               (if days-since-read
                                 (format ", last read %dd ago" days-since-read)
                                 ", never read")
                               (if hash-mismatch?
                                 ", content changed"
                                 "")))
                     stale-files)]
      (str "### Files Needing Re-Grounding (L1 Disc)\n\n"
           (str/join "\n" lines)
           "\n\n"))))

;; =============================================================================
;; Spawn Context Serialization
;; =============================================================================

(def max-spawn-context-chars
  "Maximum characters for spawn context injection (~3K tokens)."
  12000)

(defn serialize-spawn-context
  "Serialize spawn context data to markdown string.
   Formats as a '## Project Context (Auto-Injected)' section."
  [{:keys [axioms priority-conventions decisions git-info project-name stale-files]}]
  (str "## Project Context (Auto-Injected)\n\n"
       (format "**Project**: %s\n\n" (or project-name "unknown"))
       (format-spawn-axioms axioms)
       (format-spawn-priorities priority-conventions)
       (format-spawn-decisions decisions)
       (format-spawn-stale-files stale-files)
       (format-spawn-git git-info)))
