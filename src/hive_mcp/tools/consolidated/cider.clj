(ns hive-mcp.tools.consolidated.cider
  "Consolidated CIDER CLI tool.

   Subcommands: eval, doc, info, complete, apropos, status, spawn, sessions, kill-session
   Deprecated aliases: eval-explicit -> eval, eval-session -> eval

   Usage via MCP: cider {\"command\": \"eval\", \"code\": \"(+ 1 2)\"}

   SOLID: Facade pattern - single tool entry point for CIDER operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.cider :as cider-handlers]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Deprecated Alias Support
;; =============================================================================

(def ^:private deprecated-aliases
  "Map of deprecated command keywords to their canonical replacements.
   eval-explicit and eval-session are subsumed by eval with mode/session_name params."
  {:eval-explicit :eval
   :eval-session  :eval})

(defn- wrap-deprecated
  "Wrap a handler fn to emit a deprecation warning before delegating."
  [alias-kw canonical-kw handler-fn]
  (fn [params]
    (log/warn (str "DEPRECATED: command '" (name alias-kw)
                   "' is deprecated, use '" (name canonical-kw) "' instead."))
    (handler-fn params)))

;; =============================================================================
;; Handlers Map
;; =============================================================================

(def canonical-handlers
  "Map of canonical command keywords to handler functions.
   eval: unified handler routing via mode + session_name params."
  {:eval          cider-handlers/handle-cider-eval
   :doc           cider-handlers/handle-cider-doc
   :info          cider-handlers/handle-cider-info
   :complete      cider-handlers/handle-cider-complete
   :apropos       cider-handlers/handle-cider-apropos
   :status        cider-handlers/handle-cider-status
   :spawn         cider-handlers/handle-cider-spawn-session
   :sessions      cider-handlers/handle-cider-list-sessions
   :kill-session  cider-handlers/handle-cider-kill-session})

(def handlers
  "Canonical handlers merged with deprecated aliases (with log warnings)."
  (merge canonical-handlers
         (reduce-kv (fn [m alias-kw canonical-kw]
                      (assoc m alias-kw
                             (wrap-deprecated alias-kw canonical-kw
                                              (get canonical-handlers canonical-kw))))
                    {} deprecated-aliases)))

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-cider
  "Unified CLI handler for CIDER operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated cider command."
  {:name "cider"
   :consolidated true
   :description "CIDER REPL operations: eval (silent|explicit, optional session routing), doc (docstring), info (full metadata), complete (completions), apropos (search symbols), status (connection), spawn/sessions/kill-session (multi-REPL). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["eval" "eval-explicit" "doc" "info" "complete" "apropos" "status" "spawn" "sessions" "eval-session" "kill-session" "help"]
                                         :description "CIDER operation to perform"}
                              ;; eval params
                              "code" {:type "string"
                                      :description "Clojure code to evaluate"}
                              "mode" {:type "string"
                                      :enum ["silent" "explicit"]
                                      :description "Eval mode: 'silent' (default) or 'explicit' (shows in REPL buffer). Only used with eval command."}
                              ;; doc/info/complete params
                              "symbol" {:type "string"
                                        :description "Symbol name for doc/info lookup"}
                              "prefix" {:type "string"
                                        :description "Prefix for completion"}
                              ;; apropos params
                              "pattern" {:type "string"
                                         :description "Regex pattern for apropos search"}
                              "search_docs" {:type "boolean"
                                             :description "Also search docstrings"}
                              ;; session params
                              "name" {:type "string"
                                      :description "Session name for spawn"}
                              "session_name" {:type "string"
                                              :description "Session name for eval-session/kill-session. When provided with eval command, routes to session eval."}
                              "project_dir" {:type "string"
                                             :description "Project directory for spawn"}
                              "agent_id" {:type "string"
                                          :description "Agent ID to link session"}}
                 :required ["command"]}
   :handler handle-cider})

(def tools
  "Tool definitions for registration."
  [tool-def])
