(ns hive-mcp.agent.hints
  "Memory hints for ling priming — structured pointers instead of full text injection.

   Three-level hint system:
   - L1: Memory ID references (ling fetches via `memory get`)
   - L2: Semantic queries (ling searches via `memory search`)
   - L3: KG traversal seeds (ling walks graph neighborhood)

   Replaces ~15K token spawn-context with ~500 token hint block.
   Feature-flagged, backward compatible (full text = default).

   Architecture > LLM behavior: hints define the ling's Silence phase spec.
   Korzybski: pass pointers to territory, not pre-digested maps.

   Self-contained: queries Chroma directly (no dependency on catchup internals).

   See decision: 20260205213357-62f62a93 (Memory Hints Replace Text Injection)
   See convention: 20260205213853-6532e4b9 (Pass by Reference)"
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.queries :as kg-queries]
            [clojure.string :as str]
            [clojure.set :as set]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Scope-Aware Query Helpers (self-contained, mirrors catchup.clj logic)
;; =============================================================================

(defn- query-scoped-entries
  "Query Chroma entries filtered by project scope.
   Self-contained version that mirrors catchup.clj's scoping logic.
   HCR Wave 4: Uses full hierarchy scope for coordinator visibility."
  [entry-type tags project-id limit]
  (when (chroma/embedding-configured?)
    (let [limit-val (or limit 20)
          in-project? (and project-id (not= project-id "global"))
          entries (chroma/query-entries :type entry-type
                                        :limit (min (* limit-val 8) 500))
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
          filtered (if (seq tags)
                     (filter (fn [entry]
                               (let [entry-tags (set (:tags entry))]
                                 (every? #(contains? entry-tags %) tags)))
                             scoped)
                     scoped)]
      (take limit-val filtered))))

(defn- query-axioms
  "Query axiom entries (both formal type and legacy tagged conventions)."
  [project-id]
  (let [seen (volatile! #{})
        distinct-by-id (fn [coll]
                         (filterv (fn [item]
                                    (let [k (:id item)]
                                      (if (contains? @seen k) false
                                          (do (vswap! seen conj k) true))))
                                  coll))
        formal (query-scoped-entries "axiom" nil project-id 100)
        legacy (query-scoped-entries "convention" ["axiom"] project-id 100)]
    (distinct-by-id (concat formal legacy))))

;; =============================================================================
;; Hint Generation (spawn-time: coordinator → ling)
;; =============================================================================

(defn- axiom-ids
  "Extract IDs of all axioms visible to this project scope.
   Axioms are always needed (INVIOLABLE), so we pass IDs eagerly."
  [project-id]
  (try
    (let [axioms (query-axioms project-id)]
      (mapv :id axioms))
    (catch Exception e
      (log/debug "axiom-ids failed:" (.getMessage e))
      [])))

(defn- priority-convention-ids
  "Extract IDs of catchup-priority conventions.
   These are high-signal conventions worth reading in full."
  [project-id]
  (try
    (let [conventions (query-scoped-entries
                       "convention" ["catchup-priority"] project-id 50)]
      (mapv :id conventions))
    (catch Exception e
      (log/debug "priority-convention-ids failed:" (.getMessage e))
      [])))

(defn- decision-ids
  "Extract IDs of active decisions for this project."
  [project-id]
  (try
    (let [decisions (query-scoped-entries "decision" nil project-id 20)]
      (mapv :id decisions))
    (catch Exception e
      (log/debug "decision-ids failed:" (.getMessage e))
      [])))

(defn- task-semantic-queries
  "Generate semantic search queries from a task description.
   Extracts key terms the ling should search for during Silence phase."
  [task-description]
  (when (and task-description (not (str/blank? task-description)))
    ;; Extract meaningful phrases from the task
    ;; The ling will run these as Chroma semantic searches
    [(str "conventions for: " (subs task-description 0 (min 100 (count task-description))))]))

(defn- kg-seed-nodes
  "Find KG seed nodes relevant to this project for graph exploration.
   Seeds are high-connectivity nodes the ling can traverse from."
  [project-id]
  (try
    (let [stats (kg-edges/edge-stats)
          total-edges (:total-edges stats)]
      (when (> total-edges 5)
        ;; Find nodes with highest connectivity as traversal seeds
        ;; Use decisions as seeds since they tend to be central KG nodes
        (let [decisions (query-scoped-entries "decision" nil project-id 5)]
          (->> decisions
               (mapv :id)
               (take 3)
               vec))))
    (catch Exception e
      (log/debug "kg-seed-nodes failed:" (.getMessage e))
      [])))

;; =============================================================================
;; Hint Schema
;; =============================================================================

(defn generate-hints
  "Generate structured memory hints for a ling spawn.

   Arguments:
     project-id - Project scope for queries
     opts       - Optional map:
                  :task - Task description (for semantic query generation)
                  :extra-ids - Additional memory IDs to include
                  :extra-queries - Additional semantic queries
                  :extra-tags - Additional tag queries [[tag1 tag2] ...]

   Returns:
     Map with hint structure:
     {:memory-hints
       {:axiom-ids [...]       ; L1: always fetch all axioms
        :read-ids [...]        ; L1: specific memory IDs to fetch
        :queries [...]         ; L2: semantic search queries
        :tags [...]            ; L2: tag-filtered queries
        :kg-seeds [...]        ; L3: KG traversal seed nodes
        :kg-depth 2}}          ; L3: max traversal depth"
  ([project-id] (generate-hints project-id {}))
  ([project-id {:keys [task extra-ids extra-queries extra-tags]}]
   (let [axioms (axiom-ids project-id)
         priorities (priority-convention-ids project-id)
         decisions (decision-ids project-id)
         task-queries (task-semantic-queries task)
         seeds (kg-seed-nodes project-id)

         ;; Compose read-ids: priority conventions + decisions + extras
         read-ids (vec (distinct (concat priorities decisions (or extra-ids []))))

         ;; Compose queries: task-derived + extras
         queries (vec (distinct (concat (or task-queries []) (or extra-queries []))))

         ;; Compose tag queries
         tags (vec (or extra-tags []))]

     {:memory-hints
      (cond-> {:axiom-ids axioms}
        (seq read-ids) (assoc :read-ids read-ids)
        (seq queries) (assoc :queries queries)
        (seq tags) (assoc :tags tags)
        (seq seeds) (assoc :kg-seeds seeds :kg-depth 2))})))

;; =============================================================================
;; Task-Specific Hint Generation (coordinator-side, KG-driven)
;; =============================================================================

(def ^:const max-l1-ids
  "Maximum L1 memory IDs from KG traversal to avoid token bloat."
  20)

(def ^:const max-l2-queries
  "Maximum L2 semantic queries derived from KG neighborhood."
  5)

(def ^:const max-l3-seeds
  "Maximum L3 KG seed nodes for ling-side traversal."
  3)

(defn- extract-semantic-themes
  "Extract L2 semantic query themes from KG traversal results.
   Uses node tags, relation types, and entry types to generate search queries.

   Pure function — no side effects."
  [traversal-results entry]
  (let [;; Collect relation types as themes
        relation-themes (->> traversal-results
                             (map (comp name :kg-edge/relation :edge))
                             (remove nil?)
                             distinct)
        ;; Collect unique tags from source entry
        entry-tags (set (or (:tags entry) []))
        ;; Remove boilerplate tags
        meaningful-tags (remove #(or (str/starts-with? % "scope:")
                                     (str/starts-with? % "agent:")
                                     (= % "catchup-priority"))
                                entry-tags)
        ;; Build queries from tags
        tag-queries (when (seq meaningful-tags)
                      [(str "conventions for: " (str/join ", " (take 3 meaningful-tags)))])
        ;; Build queries from relations
        relation-queries (when (seq relation-themes)
                           [(str "entries related via: " (str/join ", " (take 3 relation-themes)))])
        ;; Build query from entry type
        type-query (when-let [t (:type entry)]
                     [(str (name t) " entries in this project")])]
    (->> (concat tag-queries relation-queries type-query)
         (remove nil?)
         (take max-l2-queries)
         vec)))

(defn- fallback-tag-search
  "Fallback when KG has no edges for the node: use entry tags for L2 queries.
   Returns hint structure with tag-derived queries only."
  [entry]
  (let [tags (or (:tags entry) [])
        meaningful (remove #(or (str/starts-with? % "scope:")
                                (str/starts-with? % "agent:"))
                           tags)]
    {:l1-ids []
     :l2-queries (if (seq meaningful)
                   [(str "conventions for: " (str/join ", " (take 4 meaningful)))]
                   [])
     :l3-seeds []}))

(defn generate-task-hints
  "Generate KG-driven hints for a specific task/memory node.

   Called by coordinator at dispatch time to prime a ling with
   task-relevant context from the Knowledge Graph.

   Arguments:
     opts - Map with:
            :task-id or :memory-id - Memory entry ID (kanban tasks are memory entries)
            :depth                 - KG traversal depth (default: 2)

   Returns:
     {:l1-ids    [...]  ; Direct memory IDs from KG neighborhood (max 20)
      :l2-queries [...]  ; Semantic search themes from tags/relations (max 5)
      :l3-seeds  [{:id \"...\" :depth 2}]  ; High-connectivity nodes for ling traversal (max 3)}

   Falls back to tag-based search when KG has no edges for the node.
   All outputs are bounded to prevent token bloat.

   See decision: 20260205213357-62f62a93 (Memory Hints Replace Text Injection)"
  [{:keys [task-id memory-id depth] :or {depth 2}}]
  (let [node-id (or task-id memory-id)]
    (when node-id
      (try
        ;; Fetch the source entry from Chroma for tag/type info
        (let [entry (when (chroma/embedding-configured?)
                      (chroma/get-entry-by-id node-id))
              ;; Traverse KG from this node
              traversal (try
                          (kg-queries/traverse node-id
                                               {:direction :both
                                                :max-depth depth})
                          (catch Exception e
                            (log/debug "generate-task-hints KG traverse failed:" (.getMessage e))
                            []))]
          (if (empty? traversal)
            ;; Fallback: no KG edges, use tags
            (fallback-tag-search (or entry {}))
            ;; KG has edges — extract structured hints
            (let [;; L1: Collect discovered node IDs
                  discovered-ids (->> traversal
                                      (map :node-id)
                                      distinct
                                      (take max-l1-ids)
                                      vec)
                  ;; L2: Extract semantic themes from traversal + entry
                  l2-queries (extract-semantic-themes traversal (or entry {}))
                  ;; L3: Pick high-connectivity nodes as seeds for ling-side traversal
                  ;; Nodes that appear at depth 1 are direct neighbors — good seeds
                  depth-1-nodes (->> traversal
                                     (filter #(= 1 (:depth %)))
                                     (map :node-id)
                                     distinct)
                  l3-seeds (->> depth-1-nodes
                                (take max-l3-seeds)
                                (mapv (fn [id] {:id id :depth depth})))]
              {:l1-ids discovered-ids
               :l2-queries l2-queries
               :l3-seeds l3-seeds})))
        (catch Exception e
          (log/warn "generate-task-hints failed, returning empty:" (.getMessage e))
          {:l1-ids [] :l2-queries [] :l3-seeds []})))))

;; =============================================================================
;; Hint Serialization (spawn-time: Clojure → markdown block)
;; =============================================================================

(defn serialize-hints
  "Serialize memory hints to a compact markdown block for spawn injection.

   The hints block is treated as opaque injected-context by elisp.
   The ling's Silence phase parses and executes the hints.

   Returns markdown string (~500 tokens)."
  [{:keys [memory-hints]} & {:keys [project-name git-info]}]
  (let [{:keys [axiom-ids read-ids queries tags kg-seeds kg-depth]} memory-hints
        sections (atom [])]

    ;; Header
    (swap! sections conj "## Memory Hints (Auto-Injected)")
    (swap! sections conj "")
    (when project-name
      (swap! sections conj (str "**Project**: " project-name)))
    (swap! sections conj "")
    (swap! sections conj "**Mode**: hints (structured pointers, not full text)")
    (swap! sections conj "**Action**: Process these hints during your Silence phase.")
    (swap! sections conj "")

    ;; L1: Axiom IDs (always needed)
    (when (seq axiom-ids)
      (swap! sections conj "### L1: Axiom IDs (INVIOLABLE — fetch ALL)")
      (swap! sections conj "```")
      (swap! sections conj (str "memory get " (str/join " " (take 10 axiom-ids))))
      (swap! sections conj "```")
      (swap! sections conj (str "Count: " (count axiom-ids)))
      (swap! sections conj ""))

    ;; L1: Read IDs (specific entries)
    (when (seq read-ids)
      (swap! sections conj "### L1: Priority Memory IDs (fetch during Silence)")
      (swap! sections conj "```")
      (doseq [batch (partition-all 5 read-ids)]
        (swap! sections conj (str "memory get " (str/join " " batch))))
      (swap! sections conj "```")
      (swap! sections conj (str "Count: " (count read-ids)))
      (swap! sections conj ""))

    ;; L2: Semantic queries
    (when (seq queries)
      (swap! sections conj "### L2: Semantic Queries (search during Silence)")
      (doseq [q queries]
        (swap! sections conj (str "- `memory search \"" q "\"`")))
      (swap! sections conj ""))

    ;; L2: Tag queries
    (when (seq tags)
      (swap! sections conj "### L2: Tag Queries")
      (doseq [tag-set tags]
        (swap! sections conj (str "- `memory query --tags " (str/join "," tag-set) "`")))
      (swap! sections conj ""))

    ;; L3: KG seeds
    (when (seq kg-seeds)
      (swap! sections conj (str "### L3: Knowledge Graph Seeds (traverse depth "
                                (or kg-depth 2) ")"))
      (doseq [seed kg-seeds]
        (swap! sections conj (str "- `kg traverse --start " seed " --depth "
                                  (or kg-depth 2) "`")))
      (swap! sections conj ""))

    ;; Git info (always useful, minimal cost)
    (when git-info
      (swap! sections conj "### Git Status")
      (swap! sections conj (str "- **Branch**: " (or (:branch git-info) "unknown")))
      (when (:uncommitted git-info)
        (swap! sections conj "- **Uncommitted changes**: yes"))
      (swap! sections conj (str "- **Last commit**: "
                                (or (:last-commit git-info) "unknown")))
      (swap! sections conj ""))

    (str/join "\n" @sections)))

;; =============================================================================
;; Hint Deserialization (ling-side: parse hints from injected context)
;; =============================================================================

(defn parse-hints-from-context
  "Parse memory hints from an injected context string.

   Looks for the '## Memory Hints' section and extracts structured data.
   Used by lings during their Silence phase to discover what to fetch.

   Returns:
     {:axiom-ids [...] :read-ids [...] :queries [...] :tags [...] :kg-seeds [...]}
     or nil if no hints found."
  [context-str]
  (when (and context-str (str/includes? context-str "## Memory Hints"))
    (let [hints-section (second (str/split context-str #"## Memory Hints" 2))
          ;; Extract memory IDs from code blocks after L1 headers
          id-pattern #"memory get\s+([^\n`]+)"
          ids (->> (re-seq id-pattern (or hints-section ""))
                   (mapcat (fn [[_ ids-str]] (str/split (str/trim ids-str) #"\s+")))
                   (remove str/blank?)
                   vec)
          ;; Extract semantic queries
          query-pattern #"memory search \"([^\"]+)\""
          queries (->> (re-seq query-pattern (or hints-section ""))
                       (mapv second))
          ;; Extract tag queries
          tag-pattern #"memory query --tags\s+([^\n`]+)"
          tags (->> (re-seq tag-pattern (or hints-section ""))
                    (mapv (fn [[_ tags-str]]
                            (str/split (str/trim tags-str) #","))))
          ;; Extract KG seeds
          kg-pattern #"kg traverse --start\s+(\S+)\s+--depth\s+(\d+)"
          kg-matches (re-seq kg-pattern (or hints-section ""))
          kg-seeds (mapv second kg-matches)
          kg-depth (when (seq kg-matches)
                     (Integer/parseInt (nth (first kg-matches) 2)))
          ;; Separate axiom IDs from read IDs
          ;; Axiom IDs appear after "Axiom IDs" header
          axiom-section (when hints-section
                          (second (re-find #"(?s)Axiom IDs.*?```\n(.*?)```"
                                           hints-section)))
          axiom-ids (when axiom-section
                      (->> (re-seq id-pattern axiom-section)
                           (mapcat (fn [[_ ids-str]]
                                     (str/split (str/trim ids-str) #"\s+")))
                           (remove str/blank?)
                           vec))
          ;; Read IDs = all IDs minus axiom IDs
          axiom-set (set (or axiom-ids []))
          read-ids (vec (remove axiom-set ids))]

      (when (or (seq axiom-ids) (seq read-ids) (seq queries) (seq kg-seeds))
        (cond-> {}
          (seq axiom-ids) (assoc :axiom-ids axiom-ids)
          (seq read-ids) (assoc :read-ids read-ids)
          (seq queries) (assoc :queries queries)
          (seq tags) (assoc :tags tags)
          (seq kg-seeds) (assoc :kg-seeds kg-seeds)
          kg-depth (assoc :kg-depth kg-depth))))))

;; =============================================================================
;; Hint Execution Helpers (ling-side: execute hints during Silence)
;; =============================================================================

(defn execute-hint-ids
  "Execute L1 hints: fetch memory entries by ID.

   Arguments:
     ids - Vector of memory entry IDs

   Returns:
     Vector of {:id :content :type :tags} maps"
  [ids]
  (when (seq ids)
    (->> ids
         (mapv (fn [id]
                 (try
                   (when-let [entry (chroma/get-entry-by-id id)]
                     {:id id
                      :content (:content entry)
                      :type (name (or (:type entry) "note"))
                      :tags (vec (or (:tags entry) []))})
                   (catch Exception e
                     (log/debug "Failed to fetch hint ID:" id (.getMessage e))
                     nil))))
         (filterv some?))))

(defn execute-hint-queries
  "Execute L2 hints: run semantic searches.

   Arguments:
     queries - Vector of search query strings

   Returns:
     Vector of {:query :results [{:id :preview :type}]}"
  [queries]
  (when (seq queries)
    (->> queries
         (mapv (fn [q]
                 (try
                   (let [results (chroma/search-similar q :limit 5)]
                     {:query q
                      :results (mapv (fn [r]
                                       {:id (:id r)
                                        :preview (subs (or (:content r) "") 0
                                                       (min (count (or (:content r) "")) 200))
                                        :type (name (or (:type r) "note"))})
                                     results)})
                   (catch Exception e
                     (log/debug "Failed to execute hint query:" q (.getMessage e))
                     {:query q :results [] :error (.getMessage e)}))))
         vec)))

(defn execute-hint-kg-traversal
  "Execute L3 hints: traverse KG from seed nodes.

   Arguments:
     seeds - Vector of KG node IDs
     depth - Max traversal depth (default: 2)

   Returns:
     Vector of {:seed :related [{:node-id :relation :confidence}]}"
  [seeds depth]
  (when (seq seeds)
    (->> seeds
         (mapv (fn [seed]
                 (try
                   (let [results (kg-queries/traverse
                                  seed
                                  {:direction :both
                                   :relations #{:implements :refines :depends-on
                                                :supersedes :derived-from}
                                   :max-depth (or depth 2)})]
                     {:seed seed
                      :related (mapv #(select-keys % [:node-id :relation :confidence])
                                     results)})
                   (catch Exception e
                     (log/debug "Failed KG traversal from seed:" seed (.getMessage e))
                     {:seed seed :related [] :error (.getMessage e)}))))
         vec)))

(defn execute-all-hints
  "Execute all hint levels in sequence.

   Designed for ling Silence phase: fetch IDs → run queries → traverse KG.

   Arguments:
     hints - Parsed hints map (from parse-hints-from-context or generate-hints)

   Returns:
     {:axioms [...] :entries [...] :search-results [...] :kg-graph [...]
      :stats {:axioms-fetched N :entries-fetched N :queries-run N :kg-seeds-traversed N}}"
  [hints]
  (let [axiom-entries (execute-hint-ids (:axiom-ids hints))
        read-entries (execute-hint-ids (:read-ids hints))
        search-results (execute-hint-queries (:queries hints))
        kg-results (execute-hint-kg-traversal (:kg-seeds hints) (:kg-depth hints))]
    {:axioms axiom-entries
     :entries read-entries
     :search-results search-results
     :kg-graph kg-results
     :stats {:axioms-fetched (count axiom-entries)
             :entries-fetched (count read-entries)
             :queries-run (count (or (:queries hints) []))
             :kg-seeds-traversed (count (or (:kg-seeds hints) []))}}))
