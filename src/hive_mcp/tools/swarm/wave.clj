(ns hive-mcp.tools.swarm.wave
  "Wave execution for batch drone dispatch.

   This is a facade namespace that delegates to decomposed modules:
   - domain.clj     - Constants, configs, value objects
   - validation.clj - Pre-flight validation
   - execution.clj  - Core execution orchestration
   - status.clj     - Status queries
   - handlers.clj   - MCP handlers
   - retry.clj      - Retry logic
   - phases.clj     - Execution phases
   - batching.clj   - Batch computation

   Usage:
   1. Create plan with tasks
   2. Execute wave (async, bounded concurrency)
   3. Monitor via events or get-wave-status

   SOLID: SRP - Facade only, delegates to specialized modules
   CLARITY: L - Thin layer, no business logic"
  (:require [hive-mcp.tools.swarm.wave.domain :as domain]
            [hive-mcp.tools.swarm.wave.validation :as validation]
            [hive-mcp.tools.swarm.wave.execution :as execution]
            [hive-mcp.tools.swarm.wave.status :as status]
            [hive-mcp.tools.swarm.wave.handlers :as handlers]
            [hive-mcp.swarm.datascript :as ds]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Constants (re-exported from domain)
;;; =============================================================================

(def ^:const default-concurrency domain/default-concurrency)
(def ^:const drone-timeout-ms domain/drone-timeout-ms)
(def ^:const keepalive-interval-ms domain/keepalive-interval-ms)

;;; =============================================================================
;;; Pre-flight Validation (delegated to validation module)
;;; =============================================================================

(def ensure-parent-dirs! validation/ensure-parent-dirs!)
(def validate-task-paths validation/validate-task-paths)

;;; =============================================================================
;;; Plan Management (delegated to handlers/datascript)
;;; =============================================================================

(def create-plan! handlers/create-plan!)

(defn get-pending-items
  "Get pending items for a plan."
  [plan-id]
  (ds/get-pending-items plan-id))

(defn get-plan-items
  "Get all items for a plan."
  [plan-id]
  (ds/get-plan-items plan-id))

;;; =============================================================================
;;; Wave Execution (delegated to execution module)
;;; =============================================================================

(def execute-wave! execution/execute-wave!)
(def execute-wave-async! execution/execute-wave-async!)
(def cancel-wave! execution/cancel-wave!)

;;; =============================================================================
;;; Status Queries (delegated to status module)
;;; =============================================================================

(def get-wave-status status/get-wave-status)
(def get-plan-status status/get-plan-status)

;;; =============================================================================
;;; MCP Handlers (delegated to handlers module)
;;; =============================================================================

(def handle-get-wave-status handlers/handle-get-wave-status)
(def handle-dispatch-drone-wave handlers/handle-dispatch-drone-wave)
