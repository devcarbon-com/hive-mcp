(ns hive-mcp.hot.state
  "Hot-reload state protection for hive-mcp.

   Registers critical components with hive-hot for:
   - State validation after reload
   - Snapshot/restore on failure (future: Datahike)
   - TDD-friendly assertions

   CLARITY: Explicit state management over implicit preservation."
  (:require [taoensso.timbre :as log]
            [hive-mcp.swarm.datascript.connection :as ds-conn]
            [datascript.core :as d]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; State Snapshot (Pre-reload)
;;; =============================================================================

(defonce ^:private hot-state (atom {:snapshot nil
                                    :pre-reload-count nil
                                    :validated? false}))

;; Test isolation: allows tests to inject their own connection
(defonce ^:private test-conn-override (atom nil))

(defn set-test-conn!
  "Inject test connection for isolation. Tests only."
  [conn]
  (reset! test-conn-override conn))

(defn clear-test-conn!
  "Clear test connection override."
  []
  (reset! test-conn-override nil))

(defn- get-active-conn
  "Get active connection - test override or production."
  []
  (or @test-conn-override
      (ds-conn/get-conn)))

(defn snapshot-state!
  "Capture current state before hot-reload.
   Called by hive-hot :on-reload-start listener."
  []
  (let [conn (get-active-conn)
        db @conn
        entity-count (count (d/datoms db :eavt))]
    (swap! hot-state assoc
           :snapshot db
           :pre-reload-count entity-count
           :validated? false)
    (log/debug "[hot-state] Snapshot captured:" entity-count "datoms")
    entity-count))

;;; =============================================================================
;;; State Validation (Post-reload)
;;; =============================================================================

(defn validate-state!
  "Validate state after hot-reload.
   Returns {:valid? bool :checks {...}}."
  []
  (let [conn (get-active-conn)
        db @conn
        current-count (count (d/datoms db :eavt))
        pre-count (:pre-reload-count @hot-state)
        ;; Connection should exist and have data
        conn-valid? (some? conn)
        ;; Datom count should be preserved (or increased)
        count-valid? (or (nil? pre-count)
                         (>= current-count pre-count))
        all-valid? (and conn-valid? count-valid?)]

    (swap! hot-state assoc :validated? all-valid?)

    (when-not all-valid?
      (log/warn "[hot-state] Validation failed!"
                {:conn-valid? conn-valid?
                 :count-valid? count-valid?
                 :pre-count pre-count
                 :current-count current-count}))

    (when all-valid?
      (log/debug "[hot-state] Validation passed:" current-count "datoms"))

    {:valid? all-valid?
     :checks {:connection conn-valid?
              :datom-count count-valid?}
     :counts {:before pre-count
              :after current-count}}))

;;; =============================================================================
;;; State Restoration (On Failure)
;;; =============================================================================

(defn restore-state!
  "Restore state from snapshot on reload failure.
   Note: DataScript connections are defonce, so this is rarely needed."
  []
  (when-let [snapshot (:snapshot @hot-state)]
    (log/warn "[hot-state] Restoring from snapshot...")
    ;; For DataScript, the defonce atom survives so this is a no-op
    ;; For future Datahike integration, this would reset the conn
    (swap! hot-state assoc :validated? false)
    :restored))

;;; =============================================================================
;;; hive-hot Integration
;;; =============================================================================

(defn- resolve-hot-fn
  "Dynamically resolve hive-hot function."
  [fn-sym]
  (try
    (require 'hive-hot.core)
    (when-let [v (ns-resolve (find-ns 'hive-hot.core) fn-sym)]
      @v)
    (catch Exception _ nil)))

(defn register-with-hive-hot!
  "Register state protection callbacks with hive-hot.

   Registers:
   - :hive-mcp-state-snapshot - Before reload, capture state
   - :hive-mcp-state-validate - After reload, validate state"
  []
  (if-let [add-listener! (resolve-hot-fn 'add-listener!)]
    (do
      ;; Listener for all reload events
      (add-listener! :hive-mcp-state
                     (fn [{:keys [type] :as event}]
                       (case type
                         :reload-start (snapshot-state!)
                         :reload-success (validate-state!)
                         :reload-error (restore-state!)
                         nil)))
      (log/info "[hot-state] Registered with hive-hot")
      :registered)
    (do
      (log/debug "[hot-state] hive-hot not available")
      :not-available)))

(defn unregister-from-hive-hot!
  "Remove state protection callbacks."
  []
  (when-let [remove-listener! (resolve-hot-fn 'remove-listener!)]
    (remove-listener! :hive-mcp-state)
    (log/info "[hot-state] Unregistered from hive-hot")
    :unregistered))

;;; =============================================================================
;;; Status & Testing
;;; =============================================================================

(defn status
  "Get current hot-state status for debugging/testing."
  []
  (let [state @hot-state]
    {:has-snapshot? (some? (:snapshot state))
     :pre-reload-count (:pre-reload-count state)
     :validated? (:validated? state)}))

(defn reset-hot-state!
  "Reset hot-state for testing."
  []
  (reset! hot-state {:snapshot nil
                     :pre-reload-count nil
                     :validated? false}))
