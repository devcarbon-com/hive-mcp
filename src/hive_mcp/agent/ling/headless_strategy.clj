(ns hive-mcp.agent.ling.headless-strategy
  "Headless spawn strategy — ProcessBuilder-based ling lifecycle.

   Delegates to hive-mcp.agent.headless for process management.
   No Emacs dependency — only needs `claude` CLI on PATH.

   L2 Context Protocol (Phase 3):
   strategy-spawn! is L2-aware. When ctx-refs/kg-node-ids are available,
   builds an L2 context envelope and passes it as :system-prompt to
   spawn-headless!, which appends it via --append-system-prompt CLI flag.
   This preserves Claude Code's built-in system prompt while injecting
   structured context the agent can hydrate via MCP tools.

   strategy-dispatch! enriches the task string (stdin) with L2 context
   for mid-session dispatches, since system prompt cannot be changed
   after spawn.

   SOLID: Single Responsibility — only headless/ProcessBuilder interaction.
   SOLID-O: Open for new context types via IDispatchContext protocol.
   CLARITY: L — Pure adapter between ILingStrategy and headless module."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.context-envelope :as envelope]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; L2 Context Enrichment Helpers
;;; =============================================================================

(defn- enrich-task-with-l2
  "Enrich a task string with L2 context envelope when dispatch-context available.

   When dispatch-context is a RefContext (pass-by-reference), builds an L2
   envelope and prepends it to the task. The envelope contains compressed
   context from context-store refs + KG traversal.

   For TextContext or nil dispatch-context, returns the task unchanged.

   Arguments:
     task             - Base task string
     dispatch-context - IDispatchContext instance (may be nil)
     l2-opts          - Optional map for envelope building:
                        :mode - :inline (default) or :deferred

   Returns:
     Enriched task string (with L2 envelope prepended, if applicable)."
  [task dispatch-context l2-opts]
  (if-let [l2-envelope (envelope/envelope-from-dispatch-context dispatch-context l2-opts)]
    (do
      (log/info "L2 context envelope built for headless dispatch"
                {:envelope-chars (count l2-envelope)
                 :task-chars (count (or task ""))})
      (str l2-envelope "\n\n---\n\n" task))
    task))

(defn- build-spawn-l2-system-prompt
  "Build L2 context envelope string for headless spawn system prompt.

   Tries to build an L2 spawn envelope from:
   1. Explicit ctx_refs/kg_node_ids in opts (coordinator-provided)
   2. Context-store lookup (from recent catchup)
   3. Returns nil when no L2 context available

   The returned string is passed as :system-prompt to spawn-headless!,
   which routes it to --append-system-prompt CLI flag. This appends to
   (not replaces) Claude Code's built-in system prompt.

   Arguments:
     cwd  - Working directory for context resolution
     opts - Map with optional :ctx-refs, :kg-node-ids, :scope

   Returns:
     L2 envelope string, or nil if no L2 context available."
  [cwd opts]
  (let [{:keys [ctx-refs kg-node-ids scope]} opts]
    (if (or (seq ctx-refs) (seq kg-node-ids))
      ;; Explicit refs provided — build envelope directly
      (let [l2-envelope (envelope/build-l2-envelope ctx-refs kg-node-ids scope {})]
        (when l2-envelope
          (log/info "L2 spawn system-prompt built from explicit refs"
                    {:categories (count ctx-refs) :kg-nodes (count kg-node-ids)})
          l2-envelope))
      ;; No explicit refs — try spawn envelope (context-store lookup)
      (let [l2-envelope (envelope/build-spawn-envelope cwd {})]
        (if l2-envelope
          (do
            (log/info "L2 spawn system-prompt built from context-store lookup" {:cwd cwd})
            l2-envelope)
          (do
            (log/debug "No L2 context available for headless spawn system-prompt")
            nil))))))

;;; =============================================================================
;;; Headless Strategy Implementation
;;; =============================================================================

(defrecord HeadlessStrategy []
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (let [{:keys [id cwd presets model]} ling-ctx
          {:keys [task buffer-capacity]} opts
          ;; L2 Phase 3 (P3-T4): Build L2 context envelope as system-prompt.
          ;; Routes to --append-system-prompt CLI flag, preserving Claude Code's
          ;; built-in system prompt while injecting structured context.
          ;; Task stays clean — L2 context lives in system prompt, not task prefix.
          l2-system-prompt (build-spawn-l2-system-prompt cwd opts)
          result (headless/spawn-headless! id (cond-> {:cwd cwd
                                                       :task task
                                                       :presets (or (:presets opts) presets)
                                                       :model model
                                                       :buffer-capacity (or buffer-capacity 5000)}
                                                ;; Only add :system-prompt when L2 context exists
                                                l2-system-prompt (assoc :system-prompt l2-system-prompt)))]
      (log/info "Ling spawned headless" {:id id :pid (:pid result) :cwd cwd
                                         :model (or model "claude")
                                         :l2-system-prompt? (some? l2-system-prompt)})
      ;; Return the ling-id (headless always uses requested id)
      id))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (let [{:keys [id]} ling-ctx
          {:keys [task dispatch-context]} task-opts
          ;; L2 Phase 3: Enrich task with L2 context envelope when RefContext provided
          enriched-task (enrich-task-with-l2 task dispatch-context {})]
      (headless/dispatch-via-stdin! id enriched-task)
      (log/info "Task dispatched to headless ling via stdin" {:ling-id id
                                                              :l2-enriched (not= task enriched-task)})
      true))

  (strategy-status [_ ling-ctx ds-status]
    (let [{:keys [id]} ling-ctx
          headless-info (headless/headless-status id)]
      (if ds-status
        (cond-> ds-status
          headless-info (assoc :headless-alive? (:alive? headless-info)
                               :headless-pid (:pid headless-info)
                               :headless-uptime-ms (:uptime-ms headless-info)
                               :headless-stdout (:stdout headless-info)
                               :headless-stderr (:stderr headless-info)))
        ;; Fallback to headless-only status
        (when headless-info
          {:slave/id id
           :slave/status (if (:alive? headless-info) :idle :dead)
           :ling/spawn-mode :headless
           :headless-alive? (:alive? headless-info)
           :headless-pid (:pid headless-info)}))))

  (strategy-kill! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      (try
        (let [result (headless/kill-headless! id)]
          (log/info "Headless ling killed" {:id id :pid (:pid result)})
          {:killed? true :id id :pid (:pid result)})
        (catch Exception e
          ;; Process might already be dead
          (log/warn "Headless kill exception" {:id id :error (ex-message e)})
          {:killed? true :id id :reason :process-already-dead}))))

  (strategy-interrupt! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      {:success? false
       :ling-id id
       :errors ["Interrupt not supported for headless (ProcessBuilder) spawn mode"]})))

;;; =============================================================================
;;; Factory
;;; =============================================================================

(defn ->headless-strategy
  "Create a HeadlessStrategy instance."
  []
  (->HeadlessStrategy))
