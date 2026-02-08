(ns hive-mcp.agent.headless-sdk
  "Facade for the agent/sdk/ module system.

   Re-exports the public API from split modules for backward compatibility.
   All consumers can continue to require [hive-mcp.agent.headless-sdk :as sdk].

   Module layout:
   - sdk/python.clj      - libpython-clj bridge helpers
   - sdk/availability.clj - SDK availability detection
   - sdk/session.clj      - Session registry management
   - sdk/saa.clj          - SAA phase definitions + scoring
   - sdk/event_loop.clj   - Persistent asyncio event loop
   - sdk/options.clj      - ClaudeAgentOptions construction
   - sdk/execution.clj    - Phase query execution
   - sdk/lifecycle.clj    - Spawn/dispatch/kill/interrupt/status"
  (:require [hive-mcp.agent.sdk.availability :as avail]
            [hive-mcp.agent.sdk.lifecycle :as lifecycle]
            [hive-mcp.agent.sdk.saa :as saa]
            [hive-mcp.agent.sdk.session :as session]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Availability (from sdk/availability.clj)
;;; =============================================================================

(def sdk-status
  "Check SDK availability. Delegates to sdk.availability/sdk-status."
  avail/sdk-status)

(def available?
  "Returns true if the SDK backend is fully available."
  avail/available?)

(def reset-availability!
  "Reset cached availability check. For testing."
  avail/reset-availability!)

;;; =============================================================================
;;; SAA Phase Definitions (from sdk/saa.clj)
;;; =============================================================================

(def saa-phases
  "SAA phase configuration map."
  saa/saa-phases)

(def score-observations
  "Score observations using crystal/core promotion scoring."
  saa/score-observations)

(def with-silence-tracking
  "Wrap a phase execution with silence.clj tracking."
  saa/with-silence-tracking)

;;; =============================================================================
;;; Session Registry (from sdk/session.clj)
;;; =============================================================================

(def get-session
  "Get session data for a ling."
  session/get-session)

;;; =============================================================================
;;; Lifecycle (from sdk/lifecycle.clj)
;;; =============================================================================

(def spawn-headless-sdk!
  "Spawn a headless ling using the Claude Agent SDK."
  lifecycle/spawn-headless-sdk!)

(def dispatch-headless-sdk!
  "Dispatch a task to an SDK ling via the persistent client."
  lifecycle/dispatch-headless-sdk!)

(def kill-headless-sdk!
  "Terminate an SDK ling session (graceful disconnect)."
  lifecycle/kill-headless-sdk!)

(def interrupt-headless-sdk!
  "Interrupt the current query of an SDK ling session."
  lifecycle/interrupt-headless-sdk!)

(def sdk-status-for
  "Get the status of an SDK ling."
  lifecycle/sdk-status-for)

(def list-sdk-sessions
  "List all active SDK sessions."
  lifecycle/list-sdk-sessions)

(def sdk-session?
  "Check if a ling-id corresponds to an SDK session."
  lifecycle/sdk-session?)

(def kill-all-sdk!
  "Kill all SDK sessions. For cleanup/testing."
  lifecycle/kill-all-sdk!)

;;; =============================================================================
;;; Internal re-exports (for test backward compat with private var access)
;;; =============================================================================
;; Tests use @#'sdk/register-session! and @#'sdk/unregister-session!
;; Keep these as private delegating fns so deref still works.

(defn- register-session!
  "Register a new SDK session. Internal, for test backward compat."
  [ling-id session-data]
  (session/register-session! ling-id session-data))

(defn- unregister-session!
  "Remove a session from registry. Internal, for test backward compat."
  [ling-id]
  (session/unregister-session! ling-id))

(comment
  ;; Usage examples — same API as before

  ;; Check if SDK is available
  ;; (sdk-status)  ;; => :available | :no-libpython | :no-sdk

  ;; Spawn an SDK ling
  ;; (spawn-headless-sdk! "sdk-ling-1" {:cwd "/home/user/project"})

  ;; Dispatch SAA cycle
  ;; (let [ch (dispatch-headless-sdk! "sdk-ling-1" "Fix the auth bug")]
  ;;   (clojure.core.async/go-loop []
  ;;     (when-let [msg (clojure.core.async/<! ch)]
  ;;       (println "SAA msg:" (:type msg) (:saa-phase msg))
  ;;       (recur))))

  ;; Check status (includes :interruptable? field)
  ;; (sdk-status-for "sdk-ling-1")

  ;; Interrupt current query (P3-T3)
  ;; (interrupt-headless-sdk! "sdk-ling-1")

  ;; Kill session
  ;; (kill-headless-sdk! "sdk-ling-1")

  ;; Score observations
  ;; (score-observations [{:data "found a bug in auth.clj"}
  ;;                      {:data "read the test file"}
  ;;                      {:data "discovered a pattern for validation"}])

  ;; List all
  ;; (list-sdk-sessions)
  )
