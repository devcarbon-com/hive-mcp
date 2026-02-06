(ns hive-mcp.agent.ling.headless-strategy
  "Headless spawn strategy — ProcessBuilder-based ling lifecycle.

   Delegates to hive-mcp.agent.headless for process management.
   No Emacs dependency — only needs `claude` CLI on PATH.

   SOLID: Single Responsibility — only headless/ProcessBuilder interaction.
   CLARITY: L — Pure adapter between ILingStrategy and headless module."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.agent.headless :as headless]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Headless Strategy Implementation
;;; =============================================================================

(defrecord HeadlessStrategy []
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (let [{:keys [id cwd presets model]} ling-ctx
          {:keys [task buffer-capacity]} opts
          result (headless/spawn-headless! id {:cwd cwd
                                               :task task
                                               :presets (or (:presets opts) presets)
                                               :model model
                                               :buffer-capacity (or buffer-capacity 5000)})]
      (log/info "Ling spawned headless" {:id id :pid (:pid result) :cwd cwd
                                          :model (or model "claude")})
      ;; Return the ling-id (headless always uses requested id)
      id))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (let [{:keys [id]} ling-ctx
          {:keys [task]} task-opts]
      (headless/dispatch-via-stdin! id task)
      (log/info "Task dispatched to headless ling via stdin" {:ling-id id})
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
          {:killed? true :id id :reason :process-already-dead})))))

;;; =============================================================================
;;; Factory
;;; =============================================================================

(defn ->headless-strategy
  "Create a HeadlessStrategy instance."
  []
  (->HeadlessStrategy))
