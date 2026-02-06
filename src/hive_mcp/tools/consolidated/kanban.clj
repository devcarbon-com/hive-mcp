(ns hive-mcp.tools.consolidated.kanban
  "Consolidated Kanban CLI tool.

   Canonical commands: list, create, update, status, sync, plan-to-kanban
   Deprecated aliases: move→update, roadmap→status, my-tasks→list

   Usage via MCP: kanban {\"command\": \"list\", \"status\": \"inprogress\"}

   SOLID: Facade pattern - single tool entry point for Kanban operations.
   CLARITY: L - Thin adapter delegating to memory-kanban (Chroma backend)."
  (:require [clojure.tools.logging :as log]
            [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-mcp.plan.tool :as plan-tool]))

;; =============================================================================
;; Canonical Handlers - the real commands
;; =============================================================================

(def ^:private canonical-handlers
  "Core command → handler map. These are the canonical commands."
  {:list           mem-kanban/handle-mem-kanban-list-slim
   :create         mem-kanban/handle-mem-kanban-create
   :update         mem-kanban/handle-mem-kanban-move
   :status         mem-kanban/handle-mem-kanban-stats
   :sync           (fn [_] {:success true :message "Memory kanban is single-backend, no sync needed"})
   :plan-to-kanban plan-tool/handle-plan-to-kanban})

;; =============================================================================
;; Deprecated Aliases → route to canonical handlers
;; =============================================================================

(def ^:private deprecated-aliases
  "DEPRECATED alias → canonical command mapping.
   These exist for backward compatibility only.
     move     → update  (move is just a status change, subset of update)
     roadmap  → status  (roadmap is status with milestone view)
     my-tasks → list    (my-tasks is list with agent filter)"
  {:move     :update
   :roadmap  :status
   :my-tasks :list})

(defn- wrap-deprecated
  "Wrap a canonical handler with deprecation logging."
  [alias-key canonical-key handler-fn]
  (fn [params]
    (log/warn {:event :deprecation-warning
               :command (name alias-key)
               :canonical (name canonical-key)
               :message (str "DEPRECATED: '" (name alias-key)
                             "' is deprecated. Use '" (name canonical-key)
                             "' instead.")})
    (handler-fn params)))

(def handlers
  "Map of command keywords to handler functions.
   Merges canonical handlers with deprecated aliases (with deprecation logging)."
  (merge canonical-handlers
         (reduce-kv (fn [m alias-key canonical-key]
                      (assoc m alias-key
                             (wrap-deprecated alias-key canonical-key
                                              (get canonical-handlers canonical-key))))
                    {}
                    deprecated-aliases)))

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-kanban
  "Unified CLI handler for Kanban operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated kanban command."
  {:name "kanban"
   :consolidated true
   :description "Kanban task management: list (all/filtered tasks), create (new task), update (change status/modify task), status (board overview + milestones), sync (backends), plan-to-kanban (convert plan to tasks). Aliases (deprecated): move→update, roadmap→status, my-tasks→list. Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["list" "create" "move" "status" "update" "roadmap" "my-tasks" "sync" "plan-to-kanban" "help"]
                                         :description "Kanban operation to perform"}
                              ;; list params
                              "status" {:type "string"
                                        :enum ["todo" "inprogress" "inreview" "done"]
                                        :description "Filter by task status"}
                              ;; create params
                              "title" {:type "string"
                                       :description "Task title for create"}
                              "description" {:type "string"
                                             :description "Task description"}
                              ;; move/update params
                              "task_id" {:type "string"
                                         :description "Task ID to move/update"}
                              "new_status" {:type "string"
                                            :enum ["todo" "inprogress" "inreview" "done"]
                                            :description "Target status for move"}
                              ;; plan-to-kanban params
                              "plan_id" {:type "string"
                                         :description "Memory entry ID containing the plan (for plan-to-kanban)"}
                              ;; scope params
                              "directory" {:type "string"
                                           :description "Working directory for project scope (auto-detected if not provided)"}}
                 :required ["command"]}
   :handler handle-kanban})

(def tools
  "Tool definitions for registration."
  [tool-def])
