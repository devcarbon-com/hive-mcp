(ns hive-mcp.tools.memory.crud.query
  "Query operations for memory: filtered retrieval with scope hierarchy.

   SOLID: SRP - Handles only query/read operations with scope filtering.
   CLARITY: L - Layers stay pure with clear domain separation.

   Handlers:
   - handle-query: Query entries with type/tag/duration/scope filtering
   - handle-query-metadata: Backward-compatible metadata-only query

   Scope Hierarchy (HCR):
   - Auto mode: current project + ancestors (upward traversal)
   - HCR Wave 4: include_descendants for downward traversal
   - Scope leak fix: global excluded when in project context

   Co-access tracking: Records co-access edges in KG (async, non-blocking)."
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.core :refer [mcp-json mcp-error coerce-int!]]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.plans :as plans]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.agent.context :as ctx]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Query Filters
;; ============================================================

(defn- apply-tag-filter
  "Filter entries by required tags."
  [entries tags]
  (if (seq tags)
    (filter (fn [entry]
              (let [entry-tags (set (:tags entry))]
                (every? #(contains? entry-tags %) tags)))
            entries)
    entries))

(defn- apply-duration-filter
  "Filter entries by duration."
  [entries duration]
  (if duration
    (filter #(= (:duration %) duration) entries)
    entries))

(defn- record-batch-co-access!
  "Record co-access pattern for batch query results.
   Non-blocking and fail-safe (CLARITY-Y).
   Only records when 2+ entries are returned."
  [result-ids scope]
  (when (>= (count result-ids) 2)
    (future
      (try
        (kg-edges/record-co-access!
         result-ids
         {:scope scope :created-by "system:batch-recall"})
        (catch Exception e
          (log/debug "Co-access recording failed (non-fatal):" (.getMessage e)))))))

;; ============================================================
;; Scope Filtering
;; ============================================================

(defn apply-auto-scope-filter
  "Filter entries for auto-scope mode (nil scope).
   Uses hierarchical scope resolution - includes entries from:
   - Current project scope
   - All ancestor scopes (via :parent-id chain in .hive-project.edn)

   SCOPE LEAK FIX: Global-scoped entries are NO LONGER included when in
   project context. Previously scope:global was always in the match set,
   causing global memories to leak into project queries.
   Global entries are only included when project-id IS 'global'.

   HCR Wave 1: Now walks the parent-id hierarchy for full ancestry visibility.
   HCR Wave 4: When include-descendants? is true, also includes entries from
   child projects (downward hierarchy traversal via full-hierarchy-scope-tags)."
  ([entries project-id]
   (apply-auto-scope-filter entries project-id false))
  ([entries project-id include-descendants?]
   (let [in-project? (and project-id (not= project-id "global"))
         ;; HCR Wave 4: Get scope tags based on direction
         ;; FIX: Exclude scope:global when in project context
         scope-tags (cond-> (if include-descendants?
                              ;; Full bidirectional: ancestors + self + descendants
                              (kg-scope/full-hierarchy-scope-tags project-id)
                              ;; Default: ancestors + self (upward only)
                              (kg-scope/visible-scope-tags project-id))
                      in-project? (disj "scope:global"))
         ;; Also include project-id metadata matches for backward compat
         ;; FIX: Exclude "global" from visible IDs when in project context
         visible-ids (cond-> (set (if include-descendants?
                                    (into (vec (kg-scope/visible-scopes project-id))
                                          (kg-scope/descendant-scopes project-id))
                                    (kg-scope/visible-scopes project-id)))
                       in-project? (disj "global"))]
     (filter (fn [entry]
               (let [tags (set (or (:tags entry) []))]
                 (or
                   ;; Match any visible scope tag
                  (some tags scope-tags)
                   ;; Also match entries with matching project-id metadata
                  (contains? visible-ids (:project-id entry)))))
             entries))))

;; ============================================================
;; Query Handler
;; ============================================================

(defn handle-query
  "Query project memory by type with scope filtering (Chroma-only).
   SCOPE controls which memories are returned:
     - nil/omitted: auto-filter by current project + global
     - \"all\": return all entries regardless of scope
     - \"global\": return only scope:global entries
     - specific scope tag: filter by that scope

   VERBOSITY controls output detail level:
     - \"full\" (default): return complete entries via entry->json-alist
     - \"metadata\": return only id, type, preview, tags, created (~10x fewer tokens).
       Follow up with get-full to fetch specific entries.

   HCR Wave 4: include_descendants parameter.
   When true, auto-scope mode also includes entries from child projects
   (downward hierarchy traversal). Useful for coordinators that need
   visibility into sub-project memories. Default: false (backward compat).

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer (fixes /wrap scoping issue).

   Co-access tracking: When multiple entries are returned, records co-access
   edges between them in the Knowledge Graph (async, non-blocking).

   SCOPE ISOLATION: Global-scoped memories are only returned when in global
   context or when scope is explicitly set to 'global'. Project queries
   only see project + ancestor + (optionally) descendant entries."
  [{:keys [type tags limit duration scope directory include_descendants verbosity]}]
  (let [directory (or directory (ctx/current-directory))
        include-descendants? (boolean include_descendants)
        metadata-only? (= verbosity "metadata")]
    (log/info "mcp-memory-query:" type "scope:" scope "directory:" directory
              "include_descendants:" include-descendants? "verbosity:" (or verbosity "full"))
    (try
      (let [limit-val (coerce-int! limit :limit 20)]
        (with-chroma
          (let [project-id (scope/get-current-project-id directory)
                in-project? (and project-id (not= project-id "global"))
                ;; Compute visible project-ids for DB-level $in filtering
                ;; Pushes scope filtering to Chroma, replacing over-fetch-all strategy
                project-ids-for-db
                (cond
                  ;; Auto mode: visible scopes for current project
                  (nil? scope)
                  (let [visible (kg-scope/visible-scopes project-id)
                        ;; Exclude "global" when in project context (scope leak fix)
                        filtered (if in-project?
                                   (vec (remove #(= "global" %) visible))
                                   visible)
                        ;; Include descendants if requested (HCR Wave 4)
                        descendants (when include-descendants?
                                      (kg-scope/descendant-scopes project-id))]
                    (vec (distinct (concat filtered descendants))))

                  ;; "all" mode: no DB-level filter
                  (= scope "all")
                  nil

                  ;; "global" mode: only global entries
                  (= scope "global")
                  ["global"]

                  ;; Specific scope: visible scopes from that scope
                  :else
                  (let [visible (kg-scope/visible-scopes scope)
                        descendants (when include-descendants?
                                      (kg-scope/descendant-scopes scope))]
                    (vec (distinct (concat visible descendants)))))
                ;; Route type=plan to plans collection
                plan? (= type "plan")
                ;; Reduced over-fetch: DB-level $in filtering means less waste
                over-fetch-factor (if include-descendants? 4 3)
                entries (if plan?
                          (plans/query-plans :project-id (first project-ids-for-db)
                                             :limit (* limit-val over-fetch-factor)
                                             :tags tags)
                          (chroma/query-entries :type type
                                                :project-ids project-ids-for-db
                                                :limit (* limit-val over-fetch-factor)))
                ;; Safety net: apply in-memory scope filter (belt-and-suspenders)
                scope-filtered (cond
                                 ;; Auto mode (nil scope): include project + ancestors
                                 ;; HCR Wave 4: pass include-descendants? flag
                                 (nil? scope)
                                 (apply-auto-scope-filter entries project-id include-descendants?)

                                 ;; "all" mode: no filtering
                                 (= scope "all")
                                 entries

                                 ;; Specific scope: use hierarchical filter
                                 ;; HCR Wave 4: when include_descendants, use full hierarchy
                                 :else
                                 (let [scope-filter (if include-descendants?
                                                      (kg-scope/full-hierarchy-scope-tags scope)
                                                      (scope/derive-hierarchy-scope-filter scope))]
                                   (if scope-filter
                                     (filter #(scope/matches-hierarchy-scopes? % scope-filter) entries)
                                     entries)))
                ;; Apply tag filter
                tag-filtered (apply-tag-filter scope-filtered tags)
                ;; Apply duration filter
                dur-filtered (apply-duration-filter tag-filtered duration)
                ;; Apply limit
                results (take limit-val dur-filtered)]
            ;; Record co-access pattern asynchronously (CLARITY-Y: non-blocking)
            (record-batch-co-access! (mapv :id results) project-id)
            ;; Format based on verbosity
            (if metadata-only?
              (mcp-json (mapv fmt/entry->metadata results))
              (mcp-json (mapv fmt/entry->json-alist results))))))
      (catch clojure.lang.ExceptionInfo e
        (if (= :coercion-error (:type (ex-data e)))
          (mcp-error (.getMessage e))
          (throw e))))))

;; ============================================================
;; Query Metadata Handler
;; ============================================================

(defn handle-query-metadata
  "Backward-compatible alias: delegates to handle-query with verbosity=metadata.
   Returns id, type, preview, tags, created - ~10x fewer tokens than full query.
   Follow up with mcp_memory_get_full to fetch specific entries.

   DEPRECATED: Use query command with verbosity='metadata' instead."
  [params]
  (handle-query (assoc params :verbosity "metadata")))
