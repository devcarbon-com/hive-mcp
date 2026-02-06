(ns hive-mcp.tools.consolidated.project
  "Consolidated Project CLI tool.

   Subcommands: info, files, search, find, recent, list, scan, tree

   Usage via MCP: project {\"command\": \"info\"}

   SOLID: Facade pattern - single tool entry point for project operations.
   CLARITY: L - Thin adapter delegating to domain handlers.

   HCR Wave 2: Added 'scan' command for project tree discovery."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.projectile :as projectile-handlers]
            [hive-mcp.project.tree :as tree]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Tree Handlers (HCR Wave 2)
;; =============================================================================

(defn handle-project-scan
  "Scan filesystem for .hive-project.edn files and build project hierarchy.

   Args:
     directory  - Root directory to scan (defaults to current)
     max_depth  - Maximum scan depth (default 5)
     force      - Force rescan even if fresh (default false)

   Returns project tree structure with hierarchy info."
  [{:keys [directory max_depth force]}]
  (let [effective-dir (or directory (ctx/current-directory) ".")
        opts (cond-> {}
               max_depth (assoc :max-depth max_depth)
               force (assoc :force force))]
    (log/info "project scan" {:directory effective-dir :opts opts})
    (let [result (tree/scan-project-tree! effective-dir opts)]
      {:type "text"
       :text (json/write-str result)})))

(defn handle-project-tree
  "Query the project tree structure from DataScript.

   Args:
     project_id - Optional project ID to focus on (returns descendants)

   Returns cached project hierarchy."
  [{:keys [project_id]}]
  (log/info "project tree" {:project-id project_id})
  (let [all-projects (tree/query-all-projects)
        tree-data (tree/build-project-tree all-projects)]
    (if project_id
      ;; Return subtree for specific project
      (let [descendants (tree/get-descendants tree-data project_id)
            ancestors (tree/get-ancestors tree-data project_id)
            project (tree/query-project-by-id project_id)]
        {:type "text"
         :text (json/write-str
                {:project project
                 :ancestors ancestors
                 :descendants descendants
                 :children (get (:children tree-data) project_id [])})})
      ;; Return full tree
      {:type "text"
       :text (json/write-str
              {:roots (:roots tree-data)
               :total-projects (count all-projects)
               :children (:children tree-data)
               :projects (mapv #(select-keys % [:project/id :project/path :project/type :project/parent-id])
                               all-projects)})})))

(defn handle-project-staleness
  "Check if project tree needs re-scanning.

   Args:
     directory - Root directory to check (defaults to current)

   Returns staleness status."
  [{:keys [directory]}]
  (let [effective-dir (or directory (ctx/current-directory) ".")]
    (log/info "project staleness" {:directory effective-dir})
    {:type "text"
     :text (json/write-str
            {:stale (tree/tree-stale? effective-dir)
             :directory effective-dir})}))

;; =============================================================================
;; Handlers Map - Wire commands to handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:info      projectile-handlers/handle-projectile-info
   :files     projectile-handlers/handle-projectile-files
   :search    projectile-handlers/handle-projectile-search
   :find      projectile-handlers/handle-projectile-find-file
   :recent    projectile-handlers/handle-projectile-recent
   :list      projectile-handlers/handle-projectile-list-projects
   ;; HCR Wave 2: Project tree commands
   :scan      handle-project-scan
   :tree      handle-project-tree
   :staleness handle-project-staleness})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-project
  "Unified CLI handler for project operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated project command."
  {:name "project"
   :consolidated true
   :description "Projectile project operations: info (project details), files (list files), search (content search), find (find by filename), recent (recently visited), list (all projects), scan (discover .hive-project.edn hierarchy), tree (query cached hierarchy), staleness (check if rescan needed). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["info" "files" "search" "find" "recent" "list" "scan" "tree" "staleness" "help"]
                                         :description "Project operation to perform"}
                              ;; files params
                              "pattern" {:type "string"
                                         :description "Glob pattern to filter files or search pattern"}
                              ;; find params
                              "filename" {:type "string"
                                          :description "Filename to search for"}
                              ;; scan params
                              "directory" {:type "string"
                                           :description "Root directory for scan/staleness check"}
                              "max_depth" {:type "integer"
                                           :description "Maximum scan depth (default: 5)"}
                              "force" {:type "boolean"
                                       :description "Force rescan even if fresh"}
                              ;; tree params
                              "project_id" {:type "string"
                                            :description "Project ID to query tree for"}}
                 :required ["command"]}
   :handler handle-project})

(def tools
  "Tool definitions for registration."
  [tool-def])
