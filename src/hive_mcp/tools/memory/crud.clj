(ns hive-mcp.tools.memory.crud
  "CRUD handlers for memory operations.

   SOLID: SRP - Single responsibility for create/read/update/delete.
   CLARITY: L - Layers stay pure with clear domain separation.

   Handlers:
   - add: Create new memory entry (with optional KG edge creation)
   - query: Query entries with filtering
   - query-metadata: Query returning metadata only
   - get-full: Get full entry by ID (with optional KG edges)
   - check-duplicate: Check for existing content

   Knowledge Graph Integration:
   - add supports kg_implements, kg_supersedes, kg_depends_on, kg_refines
   - get-full returns kg_outgoing and kg_incoming edges when present"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.core :refer [mcp-json mcp-error coerce-int! coerce-vec!]]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.schema :as kg-schema]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Add Handler
;; ============================================================

;; ============================================================
;; Knowledge Graph Edge Creation
;; ============================================================

(defn- update-target-incoming!
  "Update target entry's kg-incoming field with the new edge ID.
   Appends to existing incoming edges if present."
  [target-id edge-id]
  (when-let [target-entry (chroma/get-entry-by-id target-id)]
    (let [existing-incoming (or (:kg-incoming target-entry) [])
          updated-incoming (conj existing-incoming edge-id)]
      (chroma/update-entry! target-id {:kg-incoming updated-incoming}))))

(defn- create-kg-edges!
  "Create KG edges for the given relationships.

   Arguments:
     entry-id    - The newly created memory entry ID
     kg-params   - Map with :kg_implements, :kg_supersedes, :kg_depends_on, :kg_refines
     project-id  - Project scope for edge attribution
     agent-id    - Agent creating the edges (for attribution)

   Returns:
     Vector of created edge IDs

   Side effects:
     - Creates edges in DataScript KG store
     - Updates target entries' kg-incoming field in Chroma (bidirectional lookup)"
  [entry-id {:keys [kg_implements kg_supersedes kg_depends_on kg_refines]} project-id agent-id]
  (let [created-by (when agent-id (str "agent:" agent-id))
        create-edges (fn [targets relation]
                       (when (seq targets)
                         (mapv (fn [target-id]
                                 (let [edge-id (kg-edges/add-edge!
                                                {:from entry-id
                                                 :to target-id
                                                 :relation relation
                                                 :scope project-id
                                                 :confidence 1.0
                                                 :created-by created-by})]
                                   ;; Update target's kg-incoming for bidirectional lookup
                                   (update-target-incoming! target-id edge-id)
                                   edge-id))
                               targets)))]
    (vec (concat
          (create-edges kg_implements :implements)
          (create-edges kg_supersedes :supersedes)
          (create-edges kg_depends_on :depends-on)
          (create-edges kg_refines :refines)))))

(defn- default-abstraction-level
  "Return default abstraction level for entry type.
   Levels: 4=axiom/decision, 3=convention, 2=snippet/note."
  [type]
  (case type
    ("axiom" "decision") 4
    "convention" 3
    ("snippet" "note") 2
    2)) ; fallback

(defn handle-add
  "Add an entry to project memory (Chroma-only storage).
   Stores full entry in Chroma with content, metadata, and embedding.

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer (fixes /wrap scoping issue).

   When agent_id is provided (or CLAUDE_SWARM_SLAVE_ID env var is set),
   adds an 'agent:{id}' tag for tracking which ling created the entry.

   Knowledge Graph Integration:
   When kg_* parameters are provided, creates corresponding KG edges:
     - kg_implements: List of entry IDs this implements
     - kg_supersedes: List of entry IDs this supersedes
     - kg_depends_on: List of entry IDs this depends on
     - kg_refines: List of entry IDs this refines

   Edges are stored in DataScript with full provenance (scope, agent, timestamp).

   ELM Principle: Array parameters are coerced with helpful error messages."
  [{:keys [type content tags duration directory agent_id
           kg_implements kg_supersedes kg_depends_on kg_refines abstraction_level]}]
  (try
    (if (and abstraction_level (not (kg-schema/valid-abstraction-level? abstraction_level)))
      (mcp-error (str "Invalid abstraction_level: " abstraction_level))
      (let [;; ELM Principle: Coerce array parameters with helpful error messages
            tags-vec (coerce-vec! tags :tags [])
            kg-implements-vec (coerce-vec! kg_implements :kg_implements [])
            kg-supersedes-vec (coerce-vec! kg_supersedes :kg_supersedes [])
            kg-depends-on-vec (coerce-vec! kg_depends_on :kg_depends_on [])
            kg-refines-vec (coerce-vec! kg_refines :kg_refines [])
            directory (or directory (ctx/current-directory))
            abstraction-level (or abstraction_level (default-abstraction-level type))]
        (log/info "mcp-memory-add:" type "directory:" directory "agent_id:" agent_id)
        (with-chroma
          (let [project-id (scope/get-current-project-id directory)
                agent-id (or agent_id
                             (ctx/current-agent-id)
                             (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
                agent-tag (when agent-id (str "agent:" agent-id))
                base-tags tags-vec
                tags-with-agent (if agent-tag (conj base-tags agent-tag) base-tags)
                kg-tags (cond-> []
                          (seq kg-implements-vec) (conj "kg:has-implements")
                          (seq kg-supersedes-vec) (conj "kg:has-supersedes")
                          (seq kg-depends-on-vec) (conj "kg:has-depends-on")
                          (seq kg-refines-vec) (conj "kg:has-refines"))
                tags-with-kg (into tags-with-agent kg-tags)
                tags-with-scope (scope/inject-project-scope tags-with-kg project-id)
                content-hash (chroma/content-hash content)
                duration-str (or duration "long")
                expires (dur/calculate-expires duration-str)
                existing (chroma/find-duplicate type content-hash :project-id project-id)]
            (if existing
              (let [merged-tags (distinct (concat (:tags existing) tags-with-scope))
                    updated (chroma/update-entry! (:id existing) {:tags merged-tags})]
                (log/info "Duplicate found, merged tags:" (:id existing))
                (mcp-json (fmt/entry->json-alist updated)))
              (let [entry-id (chroma/index-memory-entry!
                              {:type type
                               :content content
                               :tags tags-with-scope
                               :content-hash content-hash
                               :duration duration-str
                               :expires (or expires "")
                               :project-id project-id
                               :abstraction-level abstraction-level})
                    kg-params {:kg_implements kg-implements-vec
                               :kg_supersedes kg-supersedes-vec
                               :kg_depends_on kg-depends-on-vec
                               :kg_refines kg-refines-vec}
                    edge-ids (create-kg-edges! entry-id kg-params project-id agent-id)
                    _ (when (seq edge-ids)
                        (chroma/update-entry! entry-id {:kg-outgoing edge-ids}))
                    created (chroma/get-entry-by-id entry-id)]
                (log/info "Created memory entry:" entry-id
                          (when (seq edge-ids) (str " with " (count edge-ids) " KG edges")))
                ;; Notify Olympus Web UI of new memory entry
                ;; Uses channel/publish! so olympus.clj subscription picks it up
                (try
                  (when-let [publish-fn (requiring-resolve 'hive-mcp.channel/publish!)]
                    (publish-fn {:type :memory-added
                                 :id entry-id
                                 :memory-type type
                                 :tags tags-with-scope
                                 :project-id project-id}))
                  (catch Exception _ nil))
                (mcp-json (cond-> (fmt/entry->json-alist created)
                            (seq edge-ids) (assoc :kg_edges_created edge-ids)))))))))
    (catch clojure.lang.ExceptionInfo e
      (if (#{:coercion-error :embedding-too-long} (:type (ex-data e)))
        (mcp-error (.getMessage e))
        (throw e)))))

;; ============================================================
;; Query Handler
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

(defn- apply-auto-scope-filter
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

(defn handle-query
  "Query project memory by type with scope filtering (Chroma-only).
   SCOPE controls which memories are returned:
     - nil/omitted: auto-filter by current project + global
     - \"all\": return all entries regardless of scope
     - \"global\": return only scope:global entries
     - specific scope tag: filter by that scope

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
  [{:keys [type tags limit duration scope directory include_descendants]}]
  (let [directory (or directory (ctx/current-directory))
        include-descendants? (boolean include_descendants)]
    (log/info "mcp-memory-query:" type "scope:" scope "directory:" directory
              "include_descendants:" include-descendants?)
    (try
      (let [limit-val (coerce-int! limit :limit 20)]
        (with-chroma
          (let [project-id (scope/get-current-project-id directory)
                ;; HCR Wave 4: Over-fetch more when including descendants
                over-fetch-factor (if include-descendants? 8 5)
                ;; Query from Chroma - DON'T filter by project-id at DB level when scope is nil
                ;; This allows global entries to be retrieved (they have project-id: "global")
                ;; Filtering happens in memory to include BOTH project AND global entries
                entries (chroma/query-entries :type type
                                              :limit (* limit-val over-fetch-factor))
                ;; Apply scope filtering based on mode
                scope-filtered (cond
                                 ;; Auto mode (nil scope): include project + global
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
            (mcp-json (mapv fmt/entry->json-alist results)))))
      (catch clojure.lang.ExceptionInfo e
        (if (= :coercion-error (:type (ex-data e)))
          (mcp-error (.getMessage e))
          (throw e))))))

;; ============================================================
;; Query Metadata Handler
;; ============================================================

(defn handle-query-metadata
  "Query project memory by type, returning only metadata (Chroma-only).
   Returns id, type, preview, tags, created - ~10x fewer tokens than full query.
   Follow up with mcp_memory_get_full to fetch specific entries.

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer (fixes /wrap scoping issue)."
  [{:keys [type tags limit scope directory]}]
  (let [directory (or directory (ctx/current-directory))]
    (log/info "mcp-memory-query-metadata:" type "scope:" scope "directory:" directory)
    (with-chroma
      (let [;; Reuse query logic - pass directory through
            {:keys [text isError]} (handle-query
                                    {:type type :tags tags :limit limit :scope scope :directory directory})]
        (if isError
          {:type "text" :text text :isError true}
          (let [entries (json/read-str text :key-fn keyword)
                metadata (mapv fmt/entry->metadata entries)]
            (mcp-json metadata)))))))

;; ============================================================
;; Get Full Handler (with KG Edge Inclusion)
;; ============================================================

(defn- edge->json-map
  "Convert KG edge to JSON-safe map format."
  [edge]
  (cond-> {:id (:kg-edge/id edge)
           :from (:kg-edge/from edge)
           :to (:kg-edge/to edge)
           :relation (name (:kg-edge/relation edge))
           :confidence (:kg-edge/confidence edge)
           :scope (:kg-edge/scope edge)
           :created_by (:kg-edge/created-by edge)
           :created_at (str (:kg-edge/created-at edge))}
    (:kg-edge/last-verified edge) (assoc :last_verified (str (:kg-edge/last-verified edge)))
    (:kg-edge/source-type edge) (assoc :source_type (name (:kg-edge/source-type edge)))))

(defn- get-kg-edges-for-entry
  "Get KG edges for a memory entry.
   Returns map with :outgoing and :incoming edge lists."
  [entry-id]
  (let [outgoing (kg-edges/get-edges-from entry-id)
        incoming (kg-edges/get-edges-to entry-id)]
    {:outgoing (mapv edge->json-map outgoing)
     :incoming (mapv edge->json-map incoming)}))

(defn handle-get-full
  "Get full content of a memory entry by ID (Chroma-only).
   Use after mcp_memory_query_metadata to fetch specific entries.

   Knowledge Graph Integration:
   Automatically includes KG edges when present:
     - kg_outgoing: Edges where this entry is the source
     - kg_incoming: Edges where this entry is the target"
  [{:keys [id]}]
  (log/info "mcp-memory-get-full:" id)
  (with-chroma
    (if-let [entry (chroma/get-entry-by-id id)]
      (let [base-result (fmt/entry->json-alist entry)
            ;; Include KG edges
            {:keys [outgoing incoming]} (get-kg-edges-for-entry id)
            result (cond-> base-result
                     (seq outgoing) (assoc :kg_outgoing outgoing)
                     (seq incoming) (assoc :kg_incoming incoming))]
        (mcp-json result))
      (mcp-json {:error "Entry not found" :id id}))))

;; ============================================================
;; Check Duplicate Handler
;; ============================================================

(defn handle-check-duplicate
  "Check if content already exists in memory (Chroma-only).

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer."
  [{:keys [type content directory]}]
  (log/info "mcp-memory-check-duplicate:" type "directory:" directory)
  (with-chroma
    (let [project-id (scope/get-current-project-id directory)
          hash (chroma/content-hash content)
          existing (chroma/find-duplicate type hash :project-id project-id)]
      (mcp-json {:exists (some? existing)
                 :entry (when existing (fmt/entry->json-alist existing))
                 :content_hash hash}))))

;; ============================================================
;; Update Tags Handler
;; ============================================================

(defn handle-update-tags
  "Update tags on an existing memory entry (Chroma-only).
   Replaces existing tags with the new tags list.
   Returns the updated entry or error if not found."
  [{:keys [id tags]}]
  (log/info "mcp-memory-update-tags:" id "tags:" tags)
  (with-chroma
    (if-let [_existing (chroma/get-entry-by-id id)]
      (let [updated (chroma/update-entry! id {:tags (or tags [])})]
        (log/info "Updated tags for entry:" id)
        (mcp-json (fmt/entry->json-alist updated)))
      (mcp-json {:error "Entry not found" :id id}))))
