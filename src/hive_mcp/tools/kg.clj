(ns hive-mcp.tools.kg
  "MCP tool handlers for Knowledge Graph operations.

   Thin facade dispatching to sub-namespaces:
   - kg.queries:  Read-only operations (traverse, impact, path, subgraph, etc.)
   - kg.commands: Write/mutate operations (edge, promote, reground, backfill)

   Also hosts versioning (Yggdrasil) and migration handlers directly,
   as these are smaller domains not yet warranting separate namespaces.

   SOLID-S: Facade delegates to domain modules.
   CQRS:    Queries and commands cleanly separated."
  (:require [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.tools.kg.queries :as kg-queries]
            [hive-mcp.tools.kg.commands :as kg-commands]
            [hive-mcp.knowledge-graph.versioning :as versioning]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Re-exports from sub-namespaces (preserve public API)
;;; =============================================================================

;; Query handlers
(def handle-kg-traverse        kg-queries/handle-kg-traverse)
(def handle-kg-impact-analysis kg-queries/handle-kg-impact-analysis)
(def handle-kg-find-path       kg-queries/handle-kg-find-path)
(def handle-kg-subgraph        kg-queries/handle-kg-subgraph)
(def handle-kg-contradictions  kg-queries/handle-kg-contradictions)
(def handle-kg-node-context    kg-queries/handle-kg-node-context)
(def handle-kg-stats           kg-queries/handle-kg-stats)

;; Command handlers
(def handle-kg-add-edge           kg-commands/handle-kg-add-edge)
(def handle-kg-promote            kg-commands/handle-kg-promote)
(def handle-kg-reground           kg-commands/handle-kg-reground)
(def handle-kg-backfill-grounding kg-commands/handle-kg-backfill-grounding)

;;; =============================================================================
;;; Tool Definitions (merged from sub-namespaces)
;;; =============================================================================

(def tools
  "Core KG tools (queries + commands)."
  (into kg-queries/query-tools kg-commands/command-tools))

;;; =============================================================================
;;; Versioning Tool Handlers (Yggdrasil Integration)
;;; =============================================================================

(defn handle-kg-branch
  "Create a new branch in the versioned Knowledge Graph.

   Arguments:
     name - Branch name (required, e.g., 'experiment', 'agent-1-exploration')
     from - Optional source branch or snapshot-id to branch from"
  [{:keys [name from]}]
  (log/info "kg_branch" {:name name :from from})
  (try
    (cond
      (not (versioning/versioning-available?))
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")

      (or (nil? name) (empty? name))
      (mcp-error "name is required")

      :else
      (let [branch-kw (keyword name)
            result (if from
                     (versioning/branch! branch-kw from)
                     (versioning/branch! branch-kw))]
        (if result
          (mcp-json {:success true
                     :branch (clojure.core/name branch-kw)
                     :from (or from "current")
                     :message (str "Created branch " name)})
          (mcp-error (str "Failed to create branch " name)))))
    (catch Exception e
      (log/error e "kg_branch failed")
      (mcp-error (str "Branch creation failed: " (.getMessage e))))))

(defn handle-kg-checkout
  "Switch to a different branch in the versioned Knowledge Graph.

   Arguments:
     name - Branch name to switch to (required)"
  [{:keys [name]}]
  (log/info "kg_checkout" {:name name})
  (try
    (cond
      (not (versioning/versioning-available?))
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")

      (or (nil? name) (empty? name))
      (mcp-error "name is required")

      :else
      (let [branch-kw (keyword name)
            result (versioning/checkout branch-kw)]
        (if result
          (mcp-json {:success true
                     :branch (clojure.core/name branch-kw)
                     :snapshot-id (versioning/snapshot-id)
                     :message (str "Switched to branch " name)})
          (mcp-error (str "Failed to checkout branch " name ". Branch may not exist.")))))
    (catch Exception e
      (log/error e "kg_checkout failed")
      (mcp-error (str "Checkout failed: " (.getMessage e))))))

(defn handle-kg-branches
  "List all branches in the versioned Knowledge Graph."
  [_]
  (log/info "kg_branches")
  (try
    (if-not (versioning/versioning-available?)
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")
      (let [branches (versioning/branches)
            current (versioning/current-branch)]
        (mcp-json {:success true
                   :current-branch (when current (clojure.core/name current))
                   :branches (if branches
                               (mapv clojure.core/name branches)
                               [])
                   :count (count (or branches []))})))
    (catch Exception e
      (log/error e "kg_branches failed")
      (mcp-error (str "Failed to list branches: " (.getMessage e))))))

(defn handle-kg-snapshot-id
  "Get the current commit/snapshot ID of the versioned Knowledge Graph."
  [_]
  (log/info "kg_snapshot_id")
  (try
    (if-not (versioning/versioning-available?)
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")
      (let [snap-id (versioning/snapshot-id)
            parent-ids (versioning/parent-ids)
            branch (versioning/current-branch)]
        (mcp-json {:success true
                   :snapshot-id snap-id
                   :parent-ids (vec (or parent-ids []))
                   :branch (when branch (clojure.core/name branch))})))
    (catch Exception e
      (log/error e "kg_snapshot_id failed")
      (mcp-error (str "Failed to get snapshot ID: " (.getMessage e))))))

(defn handle-kg-history
  "Get commit history for the current branch.

   Arguments:
     limit - Maximum number of commits to return (optional, default: 100)"
  [{:keys [limit]}]
  (log/info "kg_history" {:limit limit})
  (try
    (if-not (versioning/versioning-available?)
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")
      (let [opts (when limit {:limit limit})
            history (versioning/history opts)
            branch (versioning/current-branch)]
        (mcp-json {:success true
                   :branch (when branch (clojure.core/name branch))
                   :count (count history)
                   :commits (vec history)})))
    (catch Exception e
      (log/error e "kg_history failed")
      (mcp-error (str "Failed to get history: " (.getMessage e))))))

(defn handle-kg-merge
  "Merge a source branch or snapshot into the current branch.

   Arguments:
     source - Source branch name or snapshot-id to merge (required)"
  [{:keys [source]}]
  (log/info "kg_merge" {:source source})
  (try
    (cond
      (not (versioning/versioning-available?))
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")

      (or (nil? source) (empty? source))
      (mcp-error "source is required")

      :else
      (let [;; Try as branch keyword first, fall back to snapshot-id string
            source-val (if (and (string? source)
                                (not (re-matches #"^[0-9a-f-]{36}$" source)))
                         (keyword source)
                         source)
            result (versioning/merge! source-val)]
        (if result
          (mcp-json {:success true
                     :source (str source)
                     :snapshot-id (versioning/snapshot-id)
                     :message (str "Merged " source " into current branch")})
          (mcp-error (str "Failed to merge " source)))))
    (catch Exception e
      (log/error e "kg_merge failed")
      (mcp-error (str "Merge failed: " (.getMessage e))))))

(defn handle-kg-versioning-status
  "Get the current versioning status of the Knowledge Graph."
  [_]
  (log/info "kg_versioning_status")
  (try
    (let [status (versioning/status)]
      (mcp-json {:success true
                 :available (:available status)
                 :system-id (:system-id status)
                 :branch (when (:branch status) (clojure.core/name (:branch status)))
                 :snapshot-id (:snapshot-id status)
                 :branches (when (:branches status)
                             (mapv clojure.core/name (:branches status)))}))
    (catch Exception e
      (log/error e "kg_versioning_status failed")
      (mcp-error (str "Failed to get versioning status: " (.getMessage e))))))

;;; =============================================================================
;;; Versioning Tool Definitions
;;; =============================================================================

(def versioning-tools
  [{:name "kg_branch"
    :description "Create a new branch in the versioned Knowledge Graph. Branches enable parallel exploration of knowledge without affecting the main timeline. Useful for multi-agent scenarios where each agent experiments independently."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Branch name (e.g., 'experiment', 'agent-1-exploration')"}
                               "from" {:type "string"
                                       :description "Source branch or snapshot-id to branch from (optional, default: current)"}}
                  :required ["name"]}
    :handler handle-kg-branch}

   {:name "kg_checkout"
    :description "Switch to a different branch in the versioned Knowledge Graph. Changes the active branch for all subsequent KG operations."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Branch name to switch to"}}
                  :required ["name"]}
    :handler handle-kg-checkout}

   {:name "kg_branches"
    :description "List all branches in the versioned Knowledge Graph. Shows which branches exist and which is currently active."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-kg-branches}

   {:name "kg_snapshot_id"
    :description "Get the current commit/snapshot ID of the versioned Knowledge Graph. Returns the unique identifier for the current state, useful for bookmarking or branching from specific points."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-kg-snapshot-id}

   {:name "kg_history"
    :description "Get commit history for the current branch. Returns chronological list of snapshot IDs, newest first."
    :inputSchema {:type "object"
                  :properties {"limit" {:type "integer"
                                        :description "Maximum commits to return (optional, default: 100)"}}
                  :required []}
    :handler handle-kg-history}

   {:name "kg_merge"
    :description "Merge a source branch or snapshot into the current branch. Combines knowledge from parallel exploration timelines."
    :inputSchema {:type "object"
                  :properties {"source" {:type "string"
                                         :description "Source branch name or snapshot-id to merge"}}
                  :required ["source"]}
    :handler handle-kg-merge}

   {:name "kg_versioning_status"
    :description "Get the current versioning status of the Knowledge Graph. Shows whether versioning is available, current branch, snapshot, and all branches."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-kg-versioning-status}])

;;; =============================================================================
;;; Migration Tool Handlers
;;; =============================================================================

(defn handle-kg-migrate
  "Migrate KG data from one backend to another.

   Arguments:
     source_backend - Current backend (:datascript, :datalevin, or :datahike)
     target_backend - Target backend to migrate to
     dry_run        - If true, show what would be migrated without doing it
     export_path    - Optional path to save EDN backup during migration
     target_db_path - Optional path for target backend storage (Datahike/Datalevin)"
  [{:keys [source_backend target_backend dry_run export_path target_db_path]}]
  (log/info "kg_migrate" {:source source_backend :target target_backend :dry-run dry_run})
  (try
    (require 'hive-mcp.knowledge-graph.migration)
    (let [migrate-fn (resolve 'hive-mcp.knowledge-graph.migration/migrate-store!)
          source-kw (keyword source_backend)
          target-kw (keyword target_backend)
          target-opts (when target_db_path {:db-path target_db_path})
          result (migrate-fn source-kw target-kw
                             {:target-opts target-opts
                              :dry-run (boolean dry_run)
                              :export-path export_path})]
      (mcp-json {:success true
                 :source-backend (name source-kw)
                 :target-backend (name target-kw)
                 :dry-run (:dry-run result)
                 :exported (:exported result)
                 :imported (:imported result)
                 :validation (:validation result)
                 :errors (when (seq (:errors result))
                           (count (:errors result)))}))
    (catch Exception e
      (log/error e "kg_migrate failed")
      (mcp-error (str "Migration failed: " (.getMessage e))))))

(defn handle-kg-export
  "Export KG data to EDN file for backup or migration.

   Arguments:
     path - File path to save the EDN export"
  [{:keys [path]}]
  (log/info "kg_export" {:path path})
  (try
    (require 'hive-mcp.knowledge-graph.migration)
    (let [export-fn (resolve 'hive-mcp.knowledge-graph.migration/export-to-file!)
          result (export-fn path)]
      (mcp-json {:success true
                 :path path
                 :counts (:counts result)
                 :exported-at (str (:exported-at result))}))
    (catch Exception e
      (log/error e "kg_export failed")
      (mcp-error (str "Export failed: " (.getMessage e))))))

(defn handle-kg-import
  "Import KG data from EDN file.

   Arguments:
     path - File path to the EDN export file"
  [{:keys [path]}]
  (log/info "kg_import" {:path path})
  (try
    (require 'hive-mcp.knowledge-graph.migration)
    (let [import-fn (resolve 'hive-mcp.knowledge-graph.migration/import-from-file!)
          result (import-fn path)]
      (mcp-json {:success true
                 :path path
                 :imported (:imported result)
                 :errors (when (seq (:errors result))
                           {:count (count (:errors result))
                            :first-error (first (:errors result))})}))
    (catch Exception e
      (log/error e "kg_import failed")
      (mcp-error (str "Import failed: " (.getMessage e))))))

(defn handle-kg-validate-migration
  "Validate migration by comparing expected vs actual entity counts.

   Arguments:
     expected_edges     - Expected number of edges
     expected_disc      - Expected number of disc entities
     expected_synthetic - Expected number of synthetic nodes"
  [{:keys [expected_edges expected_disc expected_synthetic]}]
  (log/info "kg_validate_migration" {:edges expected_edges :disc expected_disc :synthetic expected_synthetic})
  (try
    (require 'hive-mcp.knowledge-graph.migration)
    (let [validate-fn (resolve 'hive-mcp.knowledge-graph.migration/validate-migration)
          expected {:edges (or expected_edges 0)
                    :disc (or expected_disc 0)
                    :synthetic (or expected_synthetic 0)}
          result (validate-fn expected)]
      (mcp-json {:success true
                 :valid (:valid? result)
                 :expected (:expected result)
                 :actual (:actual result)
                 :missing (:missing result)}))
    (catch Exception e
      (log/error e "kg_validate_migration failed")
      (mcp-error (str "Validation failed: " (.getMessage e))))))

;;; =============================================================================
;;; Migration Tool Definitions
;;; =============================================================================

(def migration-tools
  [{:name "kg_migrate"
    :description "Migrate Knowledge Graph data from one backend to another. Supports DataScript (in-memory), Datalevin (persistent), and Datahike (time-travel). Use dry_run=true to preview migration without executing."
    :inputSchema {:type "object"
                  :properties {"source_backend" {:type "string"
                                                 :enum ["datascript" "datalevin" "datahike"]
                                                 :description "Current backend to migrate FROM"}
                               "target_backend" {:type "string"
                                                 :enum ["datascript" "datalevin" "datahike"]
                                                 :description "Target backend to migrate TO"}
                               "dry_run" {:type "boolean"
                                          :description "Preview migration without executing (default: false)"}
                               "export_path" {:type "string"
                                              :description "Optional path to save EDN backup during migration"}
                               "target_db_path" {:type "string"
                                                 :description "Optional storage path for target backend"}}
                  :required ["source_backend" "target_backend"]}
    :handler handle-kg-migrate}

   {:name "kg_export"
    :description "Export all Knowledge Graph data (edges, disc entities, synthetic nodes) to an EDN file. Useful for backup or migration between environments."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "File path to save the EDN export"}}
                  :required ["path"]}
    :handler handle-kg-export}

   {:name "kg_import"
    :description "Import Knowledge Graph data from an EDN file. Use after kg_export to restore or migrate data."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "File path to the EDN export file"}}
                  :required ["path"]}
    :handler handle-kg-import}

   {:name "kg_validate_migration"
    :description "Validate that a migration completed successfully by comparing expected vs actual entity counts. Use after kg_import to verify data integrity."
    :inputSchema {:type "object"
                  :properties {"expected_edges" {:type "integer"
                                                 :description "Expected number of edges"}
                               "expected_disc" {:type "integer"
                                                :description "Expected number of disc entities"}
                               "expected_synthetic" {:type "integer"
                                                     :description "Expected number of synthetic nodes"}}
                  :required []}
    :handler handle-kg-validate-migration}])

;;; =============================================================================
;;; Combined Tool Export
;;; =============================================================================

(def all-tools
  "All KG tools including versioning and migration tools."
  (-> tools
      (into versioning-tools)
      (into migration-tools)))
