(ns hive-mcp.tools.agent.dag
  "DAG scheduler handlers (n-depth subcommands).

   Thin wrappers around hive-mcp.scheduler.dag-waves, using
   requiring-resolve to keep startup lazy (scheduler is optional subsystem).

   Subcommands: dag start, dag stop, dag status

   SOLID-S: Single responsibility - DAG scheduler interface only.
   CLARITY-I: Validates plan_id and cwd before delegating."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; DAG Start
;; =============================================================================

(defn handle-dag-start
  "Start the DAG scheduler for a plan.

   Thin wrapper around hive-mcp.scheduler.dag-waves/start-dag!.
   Uses requiring-resolve to keep startup lazy (scheduler is optional subsystem).

   Parameters:
     plan_id    - Memory entry ID of the plan (required)
     cwd        - Working directory (required)
     max_slots  - Max concurrent lings (default: 5)
     presets    - Ling presets (default: [\"ling\"])
     project_id - Project ID (auto-detected from cwd if nil)

   CLARITY: I - Validates plan_id and cwd before delegating."
  [{:keys [plan_id cwd max_slots presets project_id]}]
  (cond
    (str/blank? plan_id)
    (mcp-error "plan_id is required for dag start")

    (str/blank? cwd)
    (mcp-error "cwd is required for dag start")

    :else
    (try
      (let [start! (requiring-resolve 'hive-mcp.scheduler.dag-waves/start-dag!)
            opts (cond-> {:cwd cwd}
                   max_slots  (assoc :max-slots (if (string? max_slots)
                                                  (parse-long max_slots)
                                                  max_slots))
                   presets    (assoc :presets (if (string? presets)
                                                [presets]
                                                (vec presets)))
                   project_id (assoc :project-id project_id))
            result (start! plan_id opts)]
        (mcp-json result))
      (catch Exception e
        (mcp-error (str "Failed to start DAG: " (ex-message e)))))))

;; =============================================================================
;; DAG Stop
;; =============================================================================

(defn handle-dag-stop
  "Stop the DAG scheduler.

   Thin wrapper around hive-mcp.scheduler.dag-waves/stop-dag!.

   CLARITY: Y - Safe to call even when no DAG is active."
  [_params]
  (try
    (let [stop! (requiring-resolve 'hive-mcp.scheduler.dag-waves/stop-dag!)
          result (stop!)]
      (mcp-json result))
    (catch Exception e
      (mcp-error (str "Failed to stop DAG: " (ex-message e))))))

;; =============================================================================
;; DAG Status
;; =============================================================================

(defn handle-dag-status
  "Get current DAG scheduler status.

   Thin wrapper around hive-mcp.scheduler.dag-waves/dag-status.

   Returns: active, plan-id, completed/failed/dispatched counts, wave-log.

   CLARITY: R - Read-only status query."
  [_params]
  (try
    (let [status-fn (requiring-resolve 'hive-mcp.scheduler.dag-waves/dag-status)
          result (status-fn)]
      (mcp-json result))
    (catch Exception e
      (mcp-error (str "Failed to get DAG status: " (ex-message e))))))
