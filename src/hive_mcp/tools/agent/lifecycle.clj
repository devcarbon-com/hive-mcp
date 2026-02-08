(ns hive-mcp.tools.agent.lifecycle
  "Agent lifecycle handlers: interrupt, cleanup, claims, collect, broadcast.

   Groups secondary agent operations that don't warrant their own module:
   - interrupt: Send interrupt signal to agent-sdk lings
   - cleanup:   Reconcile DataScript with Emacs state (orphan removal)
   - claims:    Query file ownership claims
   - collect:   Collect task results (delegates to swarm)
   - broadcast: Broadcast prompt to all lings (delegates to swarm)

   SOLID-S: Single responsibility - secondary lifecycle operations.
   CLARITY-Y: Safe failure on all operations."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.registry :as registry]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.tools.swarm.collect :as swarm-collect]
            [hive-mcp.tools.swarm.status :as swarm-status]
            [taoensso.timbre :as log]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Interrupt Handler (P3-T3)
;; =============================================================================

(defn handle-interrupt
  "Interrupt the current query/task of a running ling.

   Sends client.interrupt() to the SDK ling's ClaudeSDKClient via
   asyncio.run_coroutine_threadsafe(). Only supported for agent-sdk
   spawn mode; other modes return an unsupported error.

   Parameters:
     agent_id - Agent ID to interrupt (required, must be a ling)

   Returns:
     {:success? true :ling-id id :phase current-phase}
     or
     {:success? false :ling-id id :errors [...]}

   Does NOT throw — returns error map on failure (CLARITY-Y)."
  [{:keys [agent_id]}]
  (if (str/blank? agent_id)
    (mcp-error "agent_id is required for interrupt")
    (let [result (ling/interrupt-ling! agent_id)]
      (if (:success? result)
        (mcp-json result)
        (mcp-error (str "Interrupt failed: " (str/join ", " (:errors result))))))))

;; =============================================================================
;; Cleanup Handler (Emacs/DataScript Reconciliation)
;; =============================================================================

(defn handle-cleanup
  "Reconcile DataScript registry with actual Emacs state.

   Problem: When Emacs restarts, hive-mcp-swarm--slaves hash table is lost,
   but DataScript persists agent records. This leaves orphan agents in DataScript
   with no corresponding Emacs buffer.

   Solution: Query both DataScript and Emacs, remove orphans from DataScript.

   Algorithm:
   1. Get all agents from DataScript
   2. Query Emacs for live ling buffers
   3. For each DataScript ling (depth=1), check if Emacs has it
   4. Remove orphans (DataScript entries without Emacs buffer)

   Note: Drones (depth=2+) are JVM-side only, not affected by Emacs restart.

   Parameters: None required

   CLARITY: Y - Safe reconciliation, only removes clear orphans."
  [_params]
  (try
    (let [;; Get all slaves from DataScript
          ds-agents (queries/get-all-slaves)
          ;; Get live lings from Emacs
          elisp-lings (or (helpers/query-elisp-lings) [])
          elisp-ids (set (map :slave/id elisp-lings))
          ;; Find orphan lings (in DataScript but not in Emacs)
          ;; Only check lings (depth=1), drones are JVM-side
          orphan-lings (->> ds-agents
                            (filter #(= 1 (:slave/depth %)))
                            (filter #(not (elisp-ids (:slave/id %))))
                            (map :slave/id))
          ;; Remove orphans from DataScript
          removed (doall
                   (for [slave-id orphan-lings]
                     (do
                       (log/info "Removing orphan ling from DataScript" {:slave-id slave-id})
                       (registry/remove-slave! slave-id)
                       slave-id)))]
      (log/info "Cleanup completed" {:orphans-removed (count removed)
                                     :ds-total (count ds-agents)
                                     :elisp-lings (count elisp-lings)})
      (mcp-json {:success true
                 :orphans-removed (count removed)
                 :removed-ids (vec removed)
                 :ds-agents-before (count ds-agents)
                 :elisp-lings-found (count elisp-lings)}))
    (catch Exception e
      (log/error "Cleanup failed" {:error (ex-message e)})
      (mcp-error (str "Cleanup failed: " (ex-message e))))))

;; =============================================================================
;; Claims Handler
;; =============================================================================

(defn handle-claims
  "Get file claims for an agent.

   Parameters:
     agent_id - Agent ID to query claims for (optional)

   Returns all claims if agent_id not provided.

   CLARITY: R - Shows both DataScript and logic claims for visibility."
  [{:keys [agent_id]}]
  (try
    (if agent_id
      ;; Get claims for specific agent
      (let [logic-claims (logic/get-all-claims)
            agent-claims (->> logic-claims
                              (filter #(= agent_id (:slave-id %)))
                              (mapv (fn [{:keys [file slave-id]}]
                                      {:file file :owner slave-id})))]
        (mcp-json {:agent-id agent_id
                   :claims agent-claims
                   :count (count agent-claims)}))
      ;; Get all claims with owner info
      (let [all-claims (logic/get-all-claims)
            formatted (->> all-claims
                           (mapv (fn [{:keys [file slave-id]}]
                                   {:file file :owner slave-id})))]
        (mcp-json {:claims formatted
                   :count (count formatted)
                   :by-owner (frequencies (map :owner formatted))})))
    (catch Exception e
      (log/error "Failed to get claims" {:error (ex-message e)})
      (mcp-error (str "Failed to get claims: " (ex-message e))))))

;; =============================================================================
;; Collect Handler (delegates to swarm for backward compat)
;; =============================================================================

(defn handle-collect
  "Collect response from a dispatched task.

   Parameters:
     task_id    - ID of the task to collect results from (required)
     timeout_ms - How long to wait for completion (default: 300000 = 5min)

   CLARITY: L - Thin adapter delegating to swarm collect handler.
   DEPRECATED: Direct swarm_collect usage is preferred."
  [{:keys [task_id] :as params}]
  (if (empty? task_id)
    (mcp-error "task_id is required")
    (swarm-collect/handle-swarm-collect params)))

;; =============================================================================
;; Broadcast Handler (delegates to swarm for backward compat)
;; =============================================================================

(defn handle-broadcast
  "Broadcast a prompt to all active lings.

   Parameters:
     prompt - The prompt to broadcast to all lings (required)

   CLARITY: L - Thin adapter delegating to swarm broadcast handler.
   DEPRECATED: Direct swarm_broadcast usage is preferred."
  [{:keys [prompt] :as params}]
  (if (empty? prompt)
    (mcp-error "prompt is required")
    (swarm-status/handle-swarm-broadcast params)))
