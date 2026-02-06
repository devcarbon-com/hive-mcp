(ns hive-mcp.tools.consolidated.session
  "Consolidated Session CLI tool.

   Subcommands: complete, wrap, whoami

   Usage via MCP: session {\"command\": \"complete\", \"commit_msg\": \"feat: done\"}
                  session {\"command\": \"whoami\"}

   SOLID: Facade pattern - single tool entry point for session lifecycle.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.session-complete :as session-handlers]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.crystal :as crystal]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.tools.memory.scope :as scope]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Whoami Handler - Return agent identity context
;; =============================================================================

(defn handle-whoami
  "Return the calling agent's identity context.

   Lings can call this to verify their identity since they cannot read
   shell environment variables directly (the MCP server runs in the
   coordinator's JVM, not the ling's terminal).

   Returns:
     {:agent-id   \"swarm-task-name-1234567890\"  ; From context or param
      :project-id \"hive-mcp\"                    ; Derived from directory
      :cwd        \"/home/user/project\"          ; Working directory}

   The agent_id parameter is optional - if provided (from system prompt injection),
   it takes precedence. Otherwise falls back to request context."
  [{:keys [agent_id directory]}]
  (let [;; Directory: explicit param > context > server cwd
        effective-dir (or directory
                          (ctx/current-directory)
                          (System/getProperty "user.dir"))
        ;; Agent-id: explicit param > context > env var > unknown
        effective-agent-id (or agent_id
                               (ctx/current-agent-id)
                               (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                               "unknown")
        ;; Project-id: derive from directory
        project-id (when effective-dir
                     (scope/get-current-project-id effective-dir))]
    (log/info "session-whoami" {:agent effective-agent-id :project project-id :cwd effective-dir})
    (mcp-json {:agent-id   effective-agent-id
               :project-id project-id
               :cwd        effective-dir})))

;; =============================================================================
;; Wrap Handler - Delegates to crystal wrap
;; =============================================================================

(defn handle-wrap
  "Wrap session - crystallize learnings without commit.
   Use session_complete for full lifecycle with git commit."
  [{:keys [agent_id directory]}]
  (log/info "session-wrap" {:agent agent_id})
  (try
    ;; Delegate to crystal wrap_crystallize
    (let [result (crystal/handle-wrap-crystallize {:directory directory})]
      result)
    (catch Exception e
      (mcp-error (str "Wrap failed: " (.getMessage e))))))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:complete session-handlers/handle-session-complete
   :wrap     handle-wrap
   :whoami   handle-whoami})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-session
  "Unified CLI handler for session lifecycle."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated session command."
  {:name "session"
   :consolidated true
   :description "Session lifecycle: complete (commit + kanban + wrap + shout), wrap (crystallize only without commit), whoami (get agent identity context). Use 'complete' at end of work session. Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["complete" "wrap" "whoami" "help"]
                                         :description "Session operation to perform"}
                              ;; complete params
                              "commit_msg" {:type "string"
                                            :description "Git commit message (required for complete)"}
                              "task_ids" {:type "array"
                                          :items {:type "string"}
                                          :description "Kanban task IDs to mark done"}
                              "agent_id" {:type "string"
                                          :description "Ling's slave-id (CLAUDE_SWARM_SLAVE_ID)"}
                              "directory" {:type "string"
                                           :description "Working directory for git/kanban scoping"}}
                 :required ["command"]}
   :handler handle-session})

(def tools
  "Tool definitions for registration."
  [tool-def])
