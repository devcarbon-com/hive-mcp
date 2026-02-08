(ns hive-mcp.workflow.yaml.actions
  "Built-in actions for the YAML workflow engine.

   Actions are dispatched via the execute-action multimethod.
   Built-in actions:
   - :echo      - Returns args as result (with variable substitution)
   - :noop      - Does nothing (placeholder/debugging)
   - :transform - Applies a Clojure fn string to context
   - :shell     - Executes a shell command (dry-run safe)
   - :mcp-call  - Invoke an MCP tool handler (workflow composition)

   SOLID-O: Open for extension via :action-registry or additional defmethods.
   CLARITY-Y: Never throws - all errors returned in result maps."
  (:require [clojure.string :as str]
            [hive-mcp.workflow.yaml.vars :as vars]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defmulti execute-action
  "Execute a workflow step action. Dispatches on :action keyword."
  (fn [action _args _ctx _opts] action))

(defmethod execute-action :echo
  [_ args ctx _opts]
  {:success? true
   :result   (vars/substitute-in-map args ctx)})

(defmethod execute-action :noop
  [_ _args _ctx _opts]
  {:success? true
   :result   {}})

(defmethod execute-action :transform
  [_ args ctx _opts]
  (try
    (let [fn-str  (vars/substitute-vars (or (:fn args) "identity") ctx)
          the-fn  (eval (read-string fn-str))
          input   (or (:input args) ctx)
          result  (the-fn input)]
      {:success? true
       :result   result})
    (catch Exception e
      {:success? false
       :result   nil
       :errors   [(str "Transform error: " (.getMessage e))]})))

(defmethod execute-action :shell
  [_ args ctx opts]
  (if (:dry-run? opts)
    {:success? true
     :result   {:dry-run true :command (vars/substitute-vars (:command args) ctx)}}
    (try
      (let [cmd    (vars/substitute-vars (:command args) ctx)
            proc   (-> (ProcessBuilder. ["sh" "-c" cmd])
                       (.redirectErrorStream true)
                       (.start))
            output (slurp (.getInputStream proc))
            exit   (.waitFor proc)]
        {:success? (zero? exit)
         :result   {:output (str/trim output) :exit-code exit :command cmd}
         :errors   (when-not (zero? exit)
                     [(str "Shell command exited with code " exit)])})
      (catch Exception e
        {:success? false
         :result   nil
         :errors   [(str "Shell error: " (.getMessage e))]}))))

;; Execute an MCP tool handler as a workflow step.
;;
;; Args map:
;;   :tool    - Tool name string (e.g. "session", "memory", "agent")
;;   :command - Subcommand for consolidated tools (e.g. "whoami", "query")
;;   :params  - Additional params to pass to the tool handler
;;
;; Resolves tool handler via requiring-resolve to avoid circular deps.
;; Merges :command and :params into a single args map for the handler.
;; Variable substitution is applied to all string values before calling.
;;
;; CLARITY-Y: Returns error result if tool not found (never throws).
(defmethod execute-action :mcp-call
  [_ args ctx opts]
  (try
    (let [tool-name  (vars/substitute-vars (or (:tool args) (get args "tool")) ctx)
          command    (vars/substitute-vars (or (:command args) (get args "command")) ctx)
          extra-params (or (:params args) (get args "params") {})
          ;; Substitute variables in extra params
          resolved-params (vars/substitute-in-map extra-params ctx)]
      (if-not tool-name
        {:success? false
         :result   nil
         :errors   ["mcp-call requires :tool in args"]}
        ;; Resolve get-tool-by-name via requiring-resolve to avoid circular dep
        (let [get-tool-fn (requiring-resolve 'hive-mcp.tools/get-tool-by-name)
              tool-def    (get-tool-fn tool-name)]
          (if-not tool-def
            {:success? false
             :result   nil
             :errors   [(str "Tool not found: " tool-name)]}
            ;; Build the handler args: merge command + extra params
            (let [handler    (:handler tool-def)
                  handler-args (cond-> resolved-params
                                 command (assoc :command command)
                                 ;; Pass through workflow context vars that tools expect
                                 (:directory ctx) (assoc :directory (:directory ctx))
                                 (:agent_id ctx)  (assoc :agent_id (:agent_id ctx)))
                  result     (handler handler-args)]
              {:success? true
               :result   result})))))
    (catch Exception e
      {:success? false
       :result   nil
       :errors   [(str "mcp-call error: " (.getMessage e))]})))

(defmethod execute-action :default
  [action _args _ctx _opts]
  {:success? false
   :result   nil
   :errors   [(str "Unknown action: " (name action))]})
