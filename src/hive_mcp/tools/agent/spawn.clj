(ns hive-mcp.tools.agent.spawn
  "Agent spawn handler.

   Handles creation of new ling and drone agents with:
   - Project scope resolution (HCR Wave 4)
   - Spawn mode normalization (vterm/headless/agent-sdk)
   - Subagent definition normalization for Agent SDK
   - Budget guardrail support (P2-T4)

   SOLID-S: Single responsibility - spawn lifecycle only.
   CLARITY-I: Validates type and spawn_mode before dispatch."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [taoensso.timbre :as log]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Project Scope Resolution
;; =============================================================================

(defn- resolve-project-scope
  "Resolve effective project-id for a spawned agent.
   HCR Wave 4: Inherits project scope from parent agent's hierarchy.

   Resolution order:
   1. Explicit project_id parameter
   2. Inferred from cwd via kg-scope/infer-scope-from-path
   3. Parent agent's project-id (if parent specified)
   4. Fallback: last segment of cwd

   Also registers the resolved scope in kg-scope for hierarchy lookups."
  [project_id cwd parent]
  (or project_id
      ;; Try to infer from cwd using hierarchy-aware scope resolution
      (when cwd
        (let [inferred (kg-scope/infer-scope-from-path cwd)]
          (when (and inferred (not= inferred "global"))
            inferred)))
      ;; Inherit from parent agent's project scope
      (when parent
        (when-let [parent-data (queries/get-slave parent)]
          (:slave/project-id parent-data)))
      ;; Fallback to last path segment
      (when cwd
        (last (str/split cwd #"/")))))

;; =============================================================================
;; Spawn Handler
;; =============================================================================

(defn handle-spawn
  "Spawn a new agent (ling or drone).

   Parameters:
     type       - Agent type: 'ling' or 'drone' (required)
     name       - Optional agent name (auto-generated if not provided)
     cwd        - Working directory (required)
     presets    - Preset names for ling (optional, ling only)
     model      - Model override. For drones: OpenRouter model ID.
                  For lings: 'claude' (default, uses Claude Code CLI) or
                  OpenRouter model ID (auto-forces headless spawn mode).
     task       - Initial task to dispatch (optional)
     files      - Files for drone to work on (optional, drone only)
     parent     - Parent agent ID (optional)
     spawn_mode     - Spawn mode for lings: 'vterm' (default) or 'headless' (optional)
                      'headless' maps to :agent-sdk (Claude Agent SDK, default since 0.12.0).
                      Non-claude models automatically force headless/openrouter mode.
     agents         - Subagent definitions map (optional, agent-sdk/headless mode only)
                      Map of agent-name -> {description, prompt, tools?, model?}
                      Passed to ClaudeAgentOptions.agents for custom subagent definitions.
     max_budget_usd - Maximum USD spend for this agent (optional, P2-T4 budget guardrail).
                      When set, registers a budget guardrail hook that denies+interrupts
                      tool calls when cumulative cost exceeds the limit.

   HCR Wave 4: Inherits project scope from parent hierarchy.
   CLARITY: I - Validates type before dispatch."
  [{:keys [type name cwd presets model task files parent project_id kanban_task_id spawn_mode agents max_budget_usd]}]
  (let [agent-type (keyword type)]
    (if-not (#{:ling :drone} agent-type)
      (mcp-error "type must be 'ling' or 'drone'")
      (try
        (let [agent-id (or name (helpers/generate-agent-id agent-type))
              ;; HCR Wave 4: Use hierarchy-aware project scope resolution
              effective-project-id (resolve-project-scope project_id cwd parent)]
          (case agent-type
            :ling
            ;; FIX: Ensure presets is always a vector, not a string
            ;; The shim may pass "explorer" instead of ["explorer"]
            (let [presets-vec (cond
                                (nil? presets) []
                                (string? presets) [presets]
                                (sequential? presets) (vec presets)
                                :else [presets])
                  ;; Resolve spawn mode: 'headless' maps to :agent-sdk (default since 0.12.0)
                  ;; Non-claude models automatically force headless via ->ling
                  effective-spawn-mode (keyword (or spawn_mode "vterm"))
                  _ (when-not (#{:vterm :headless :agent-sdk} effective-spawn-mode)
                      (throw (ex-info "spawn_mode must be 'vterm', 'headless', or 'agent-sdk'"
                                      {:spawn-mode spawn_mode})))
                  ;; Normalize agents map: convert string keys to keyword keys for agent specs
                  ;; MCP JSON sends {"name": {"description": "...", "prompt": "...", "tools": [...]}}
                  ;; Clojure side expects {"name" {:description "..." :prompt "..." :tools [...]}}
                  normalized-agents (when (map? agents)
                                      (reduce-kv
                                       (fn [m agent-name agent-spec]
                                         (assoc m (clojure.core/name agent-name)
                                                (if (map? agent-spec)
                                                  (reduce-kv (fn [m2 k v]
                                                               (assoc m2 (keyword k) v))
                                                             {} agent-spec)
                                                  agent-spec)))
                                       {} agents))
                  ;; ->ling handles auto-forcing headless for non-claude models
                  ling-agent (ling/->ling agent-id (cond-> {:cwd cwd
                                                            :presets presets-vec
                                                            :project-id effective-project-id
                                                            :spawn-mode effective-spawn-mode
                                                            :model model}
                                                     normalized-agents (assoc :agents normalized-agents)
                                                     max_budget_usd    (assoc :max-budget-usd max_budget_usd)))
                  ;; spawn! returns the actual slave-id
                  ;; For vterm: may differ from agent-id (elisp assigns)
                  ;; For headless: same as agent-id (we control it)
                  slave-id (proto/spawn! ling-agent (cond-> {:task task
                                                             :parent parent
                                                             :kanban-task-id kanban_task_id
                                                             :spawn-mode (:spawn-mode ling-agent)
                                                             :model model}
                                                      max_budget_usd (assoc :max-budget-usd max_budget_usd)))]
              (log/info "Spawned ling" {:requested-id agent-id
                                        :slave-id slave-id
                                        :spawn-mode (:spawn-mode ling-agent)
                                        :model (or model "claude")
                                        :cwd cwd :presets presets-vec
                                        :project-id effective-project-id})
              (mcp-json {:success true
                         :agent-id slave-id
                         :type :ling
                         :spawn-mode (:spawn-mode ling-agent)
                         :model (or model "claude")
                         :cwd cwd
                         :presets presets-vec
                         :project-id effective-project-id}))

            :drone
            (let [drone-agent (drone/->drone agent-id {:cwd cwd
                                                       :model model
                                                       :parent-id parent
                                                       :project-id effective-project-id})]
              (proto/spawn! drone-agent {:files files})
              (log/info "Spawned drone" {:id agent-id :cwd cwd :model model})
              (mcp-json {:success true
                         :agent-id agent-id
                         :type :drone
                         :cwd cwd
                         :files files}))))
        (catch Exception e
          (log/error "Failed to spawn agent" {:type agent-type :error (ex-message e)})
          (mcp-error (str "Failed to spawn " (name agent-type) ": " (ex-message e))))))))
