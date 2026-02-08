(ns hive-mcp.tools.agent.kill
  "Agent kill and kill-batch handlers.

   Handles agent termination with:
   - Cross-project ownership checks (HIL safety)
   - Agent type detection and reconstruction
   - Batch kill with per-agent result reporting

   SOLID-S: Single responsibility - agent termination only.
   CLARITY-Y: Safe failure with ownership validation."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.tools.memory.scope :as scope]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Core Kill Logic
;; =============================================================================

(defn- kill-one!
  "Core kill logic for a single agent. Returns plain data:
   - {:killed id :result proto-result} on success
   - {:error \"reason\" :id id} on failure

   Used by both handle-kill and handle-kill-batch to avoid duplicating
   ownership checks, agent construction, and proto/kill! dispatch.

   Ownership rules (HIL):
   - Caller without explicit directory (coordinator): can kill anything
   - Legacy lings without project-id: can be killed by anyone
   - Same project: kill proceeds normally
   - Different project: requires force_cross_project=true"
  [agent-id {:keys [directory force_cross_project]}]
  (try
    (if-let [agent-data (queries/get-slave agent-id)]
      (let [;; CRITICAL: Only use EXPLICIT directory param, not ctx fallback.
            caller-project-id (when directory
                                (scope/get-current-project-id directory))
            target-project-id (:slave/project-id agent-data)
            can-kill? (or force_cross_project
                          (nil? caller-project-id)
                          (nil? target-project-id)
                          (= caller-project-id target-project-id))]
        (if-not can-kill?
          {:error (format "Agent '%s' belongs to project '%s', not '%s'. Pass force_cross_project=true to kill cross-project."
                          agent-id target-project-id caller-project-id)
           :id agent-id}
          (let [agent-type (if (= 1 (:slave/depth agent-data)) :ling :drone)
                agent (case agent-type
                        :ling (ling/->ling agent-id {:cwd (:slave/cwd agent-data)
                                                     :presets (:slave/presets agent-data)
                                                     :project-id (:slave/project-id agent-data)
                                                     :spawn-mode (or (:ling/spawn-mode agent-data) :vterm)})
                        :drone (drone/->drone agent-id {:cwd (:slave/cwd agent-data)
                                                        :parent-id (:slave/parent agent-data)
                                                        :project-id (:slave/project-id agent-data)}))
                result (proto/kill! agent)]
            (log/info "Kill agent result" {:agent_id agent-id
                                           :result result
                                           :caller-project caller-project-id
                                           :target-project target-project-id})
            {:killed agent-id :result result})))
      {:error (str "Agent not found: " agent-id) :id agent-id})
    (catch Exception e
      (log/error "Failed to kill agent" {:agent_id agent-id :error (ex-message e)})
      {:error (str "Failed to kill agent: " (ex-message e)) :id agent-id})))

;; =============================================================================
;; Kill Handler
;; =============================================================================

(defn handle-kill
  "Terminate an agent.

   Parameters:
     agent_id            - Agent ID to kill (required)
     directory           - Caller's working directory for ownership check (optional)
     force               - Force kill even if critical ops in progress (default: false)
     force_cross_project - Allow killing agents from different projects (default: false)

   HUMAN-IN-THE-LOOP (HIL): Cross-project kill prevention.
   If target agent's project differs from caller's project, kill is denied
   unless force_cross_project=true is explicitly passed.

   CLARITY: Y - Safe failure, checks ownership + critical ops before killing.
   CLARITY: I - Inputs guarded with HIL for cross-project safety.

   BUG FIX (2026-02): Only use EXPLICIT directory param for ownership check."
  [{:keys [agent_id] :as params}]
  (if (empty? agent_id)
    (mcp-error "agent_id is required")
    (let [result (kill-one! agent_id params)]
      (if (:error result)
        (mcp-error (:error result))
        (mcp-json (:result result))))))

;; =============================================================================
;; Kill-Batch Handler
;; =============================================================================

(defn handle-kill-batch
  "Terminate multiple agents in a single call.

   Parameters:
     agent_ids           - Array of agent IDs to kill (required)
     force               - Force kill even if critical ops in progress (default: false)
     force_cross_project - Allow killing agents from different projects (default: false)
     directory           - Caller's working directory for ownership check (optional)

   Returns: {killed: [...], failed: [{id: ..., error: ...}], summary: {...}}

   Reuses kill-one! core logic — same ownership checks apply per agent.

   CLARITY: I - Validates agent_ids array before processing.
   CLARITY: R - Returns detailed per-agent results for transparency."
  [{:keys [agent_ids] :as params}]
  (if (or (nil? agent_ids) (empty? agent_ids))
    (mcp-error "agent_ids is required (array of agent ID strings)")
    (let [results (mapv #(kill-one! % params) agent_ids)
          killed  (filterv :killed results)
          failed  (filterv :error results)]
      (log/info "Kill-batch completed" {:total (count agent_ids)
                                        :killed (count killed)
                                        :failed (count failed)})
      (mcp-json {:killed  (mapv :killed killed)
                 :failed  (mapv #(select-keys % [:id :error]) failed)
                 :summary {:total  (count agent_ids)
                           :killed (count killed)
                           :failed (count failed)}}))))
