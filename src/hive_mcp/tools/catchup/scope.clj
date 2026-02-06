(ns hive-mcp.tools.catchup.scope
  "Scope resolution and query functions for catchup workflow.

   SOLID: SRP - Single responsibility for scope-filtered memory queries.
   Extracted from hive-mcp.tools.catchup (Sprint 1 refactoring).

   Contains:
   - distinct-by utility (used by scope-piercing logic)
   - Project name lookup (Emacs integration)
   - Tag filtering
   - Scope-filtered Chroma queries (HCR Wave 4, scope-piercing)
   - Expiring entry detection
   - Composed query helpers (axioms, regular conventions)"
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [clojure.string :as str]
            [clojure.set :as set]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Utility Helpers
;; =============================================================================

(defn distinct-by
  "Return distinct elements from coll by the value of (f item).
   Keeps the first occurrence of each unique (f item) value."
  [f coll]
  (let [seen (volatile! #{})]
    (filterv (fn [item]
               (let [key (f item)]
                 (if (contains? @seen key)
                   false
                   (do (vswap! seen conj key) true))))
             coll)))

(defn- newest-first
  "Sort entries by :created timestamp, newest first.
   SLAP helper — keeps pipelines at a single abstraction level."
  [entries]
  (sort-by :created #(compare %2 %1) entries))

;; =============================================================================
;; Project Context Helpers
;; =============================================================================

;; NOTE: get-current-project-id is provided by scope/get-current-project-id
;; which implements the Go Context Pattern: context flows down explicitly,
;; never derived from ambient Emacs state.

(defn get-current-project-name
  "Get current project name from Emacs.
   When directory is provided, uses that path to determine project context."
  ([] (get-current-project-name nil))
  ([directory]
   (try
     (let [elisp (if directory
                   (format "(hive-mcp-memory--get-project-name %s)" (pr-str directory))
                   "(hive-mcp-memory--get-project-name)")
           {:keys [success result timed-out]} (ec/eval-elisp-with-timeout elisp 10000)]
       (if (and success result (not= result "nil") (not timed-out))
         (str/replace result #"\"" "")
         nil))
     (catch Exception _
       nil))))

;; =============================================================================
;; Tag Filtering
;; =============================================================================

(defn filter-by-tags
  "Filter entries to only those containing all specified tags."
  [entries tags]
  (if (seq tags)
    (filter (fn [entry]
              (let [entry-tags (set (:tags entry))]
                (every? #(contains? entry-tags %) tags)))
            entries)
    entries))

;; =============================================================================
;; Scope-Filtered Queries
;; =============================================================================

(defn query-scoped-entries
  "Query Chroma entries filtered by project scope.
   Uses project-id metadata filtering AND hierarchical scope matching.
   Aligned with crud.clj approach for consistent behavior.

   HCR Wave 4: Catchup uses full-hierarchy-scope-tags by default, so
   coordinator gets visibility into child project memories too. This is
   the key integration point - catchup surfaces the full project tree.

   SCOPE LEAK FIX: When in project context, excludes scope:global from
   matching to prevent global memories from leaking into project catchup.
   Global entries are only included when project-id IS 'global'."
  [entry-type tags project-id limit]
  (when (chroma/embedding-configured?)
    (let [limit-val (or limit 20)
          in-project? (and project-id (not= project-id "global"))
          ;; HCR Wave 4: Don't filter by project-id at DB level - we want entries
          ;; from child projects too. Over-fetch more for hierarchy filtering.
          entries (chroma/query-entries :type entry-type
                                        :limit (min (* limit-val 8) 500))
          ;; HCR Wave 4: Use full hierarchy scope (ancestors + self + descendants)
          ;; FIX: Exclude scope:global when in project context to prevent leak
          full-scope-tags (cond-> (kg-scope/full-hierarchy-scope-tags project-id)
                            in-project? (disj "scope:global"))
          ;; Build set of all visible project IDs (self + ancestors + descendants)
          ;; FIX: Exclude "global" from visible IDs when in project context
          all-visible-ids (cond-> (set (into (vec (kg-scope/visible-scopes project-id))
                                             (kg-scope/descendant-scopes project-id)))
                            in-project? (disj "global"))
          scoped (filter (fn [entry]
                           (let [entry-tags (set (or (:tags entry) []))]
                             (or
                              ;; Match any full hierarchy scope tag
                              (some entry-tags full-scope-tags)
                              ;; Also match by project-id metadata for backward compat
                              (contains? all-visible-ids (:project-id entry)))))
                         entries)
          ;; Axioms and catchup-priority ALWAYS pierce scope boundaries.
          ;; The scope leak fix (disj "scope:global") is correct for general entries,
          ;; but axioms are INVIOLABLE by definition and must always be visible.
          ;; Similarly, catchup-priority conventions are explicitly marked for visibility.
          ;; NOTE: Some axioms have project-id "global" but scope:project:hive-mcp tags —
          ;; this inconsistency is handled by this scope-piercing pass.
          scope-piercing (when in-project?
                           (filter (fn [entry]
                                     (let [entry-tags (set (or (:tags entry) []))
                                           entry-type (str (or (:type entry) ""))]
                                       (or (= entry-type "axiom")
                                           (contains? entry-tags "catchup-priority"))))
                                   entries))
          scoped (distinct-by :id (concat scoped scope-piercing))
          ;; NOTE: filter-by-tags has signature [entries tags], so we can't use ->>
          filtered (filter-by-tags scoped tags)]
      (->> filtered
           newest-first
           (take limit-val)))))

;; =============================================================================
;; Expiring Entries
;; =============================================================================

(defn entry-expiring-soon?
  "Check if entry expires within 7 days."
  [entry]
  (when-let [exp (:expires entry)]
    (try
      (let [exp-time (java.time.ZonedDateTime/parse exp)
            now (java.time.ZonedDateTime/now)
            week-later (.plusDays now 7)]
        (.isBefore exp-time week-later))
      (catch Exception _ false))))

(defn query-expiring-entries
  "Query entries expiring within 7 days, scoped to project.
   HCR Wave 4: Uses full hierarchy scope to surface expiring entries from
   child projects too.
   Scope-piercing: Axioms and catchup-priority entries always included
   regardless of scope, since expiring axioms need urgent attention."
  [project-id limit]
  (let [in-project? (and project-id (not= project-id "global"))
        ;; HCR Wave 4: Don't filter by project-id at DB level
        entries (chroma/query-entries :limit 100)
        ;; Use full hierarchy scope (ancestors + self + descendants)
        ;; FIX: Exclude scope:global when in project context (matches query-scoped-entries)
        full-scope-tags (cond-> (kg-scope/full-hierarchy-scope-tags project-id)
                          in-project? (disj "scope:global"))
        all-visible-ids (cond-> (set (into (vec (kg-scope/visible-scopes project-id))
                                           (kg-scope/descendant-scopes project-id)))
                          in-project? (disj "global"))
        scoped (filter (fn [entry]
                         (let [entry-tags (set (or (:tags entry) []))]
                           (or (some entry-tags full-scope-tags)
                               (contains? all-visible-ids (:project-id entry)))))
                       entries)
        ;; Scope-piercing: axioms and catchup-priority always visible for expiring check
        scope-piercing (when in-project?
                         (filter (fn [entry]
                                   (let [entry-tags (set (or (:tags entry) []))
                                         entry-type (str (or (:type entry) ""))]
                                     (or (= entry-type "axiom")
                                         (contains? entry-tags "catchup-priority"))))
                                 entries))
        scoped (distinct-by :id (concat scoped scope-piercing))]
    (->> scoped
         (filter entry-expiring-soon?)
         newest-first
         (take (or limit 20)))))

;; =============================================================================
;; Composed Query Helpers
;; =============================================================================

(defn query-axioms
  "Query axiom entries (both formal type and legacy tagged conventions)."
  [project-id]
  (let [formal (query-scoped-entries "axiom" nil project-id 100)
        legacy (query-scoped-entries "convention" ["axiom"] project-id 100)]
    (distinct-by :id (concat formal legacy))))

(defn query-regular-conventions
  "Query conventions excluding axioms and priority ones."
  [project-id axiom-ids priority-ids]
  (let [all-conventions (query-scoped-entries "convention" nil project-id 50)
        excluded-ids (set/union axiom-ids priority-ids)]
    (remove #(contains? excluded-ids (:id %)) all-conventions)))
