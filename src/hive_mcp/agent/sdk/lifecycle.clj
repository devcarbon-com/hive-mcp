(ns hive-mcp.agent.sdk.lifecycle
  "SDK session lifecycle management: spawn, dispatch, kill, interrupt, status.

   Orchestrates the full lifecycle of SDK ling sessions:
   - spawn-headless-sdk!    : Create session with persistent client
   - dispatch-headless-sdk! : Send tasks (SAA cycle or raw query)
   - kill-headless-sdk!     : Graceful teardown (disconnect -> stop -> unregister)
   - interrupt-headless-sdk!: Interrupt current query via client.interrupt()
   - sdk-status-for         : Session status including multi-turn info
   - list-sdk-sessions      : List all active sessions
   - sdk-session?           : Check if ling-id has SDK session
   - kill-all-sdk!          : Kill all sessions (cleanup/testing)"
  (:require [clojure.core.async :as async :refer [chan >!! <!! close!]]
            [hive-mcp.agent.sdk.availability :as avail]
            [hive-mcp.agent.sdk.event-loop :as event-loop]
            [hive-mcp.agent.sdk.execution :as exec]
            [hive-mcp.agent.sdk.options :as opts]
            [hive-mcp.agent.sdk.python :as py]
            [hive-mcp.agent.sdk.session :as session]
            [hive-mcp.agent.sdk.saa :as saa]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Spawn
;;; =============================================================================

(defn spawn-headless-sdk!
  "Spawn a headless ling using the Claude Agent SDK.

   Initializes a new SDK session and prepares for SAA phases.
   Does NOT start execution - call dispatch-headless-sdk! for that.

   Arguments:
     ling-id - Unique identifier for this ling
     opts    - Map with:
               :cwd           - Working directory (required)
               :system-prompt - Base system prompt (optional)
               :mcp-servers   - MCP server configurations (optional)
               :presets       - Preset names (optional)
               :agents        - Subagent definitions map (optional)

   Returns:
     {:ling-id ling-id :status :spawned :backend :agent-sdk :phase :idle}

   Throws:
     ExceptionInfo if SDK not available or ling-id already exists"
  [ling-id {:keys [cwd system-prompt mcp-servers presets agents] :as _opts}]
  {:pre [(string? ling-id)
         (string? cwd)]}
  ;; Check availability
  (let [status (avail/sdk-status)]
    (when-not (= :available status)
      (throw (ex-info "Claude Agent SDK not available"
                      {:ling-id ling-id
                       :sdk-status status
                       :hint (case status
                               :no-libpython "Add clj-python/libpython-clj to deps.edn"
                               :no-sdk "Run: pip install claude-code-sdk"
                               :not-initialized "Python initialization failed"
                               "Unknown issue")}))))
  ;; Check for duplicate
  (when (session/get-session ling-id)
    (throw (ex-info "SDK session already exists with this ID"
                    {:ling-id ling-id})))
  ;; Register session with persistent loop + client (P3-T2)
  (let [message-ch (chan 4096)
        result-ch (chan 1)
        safe-id (session/ling-id->safe-id ling-id)
        ;; Start persistent event loop in background thread
        {:keys [loop-var _thread-var]} (event-loop/start-session-loop! safe-id)
        ;; Build base options (Act-phase perms, auto-obs hooks)
        base-opts (opts/build-base-options-obj {:cwd cwd
                                                :system-prompt system-prompt
                                                :agents agents})
        ;; Connect client on the persistent loop
        client-var (event-loop/connect-session-client! safe-id base-opts loop-var)
        session-data {:ling-id ling-id
                      :phase :idle
                      :phase-history []
                      :observations []
                      :plan nil
                      :message-ch message-ch
                      :result-ch result-ch
                      :started-at (System/currentTimeMillis)
                      :cwd cwd
                      :system-prompt system-prompt
                      :mcp-servers mcp-servers
                      :presets presets
                      :agents agents
                      :session-id nil
                      ;; P3-T2: Persistent client/loop refs
                      :client-ref client-var
                      :py-loop-var loop-var
                      :py-safe-id safe-id
                      :turn-count 0}]
    (session/register-session! ling-id session-data)
    (log/info "[sdk.lifecycle] Spawned SDK ling with persistent client"
              {:ling-id ling-id :cwd cwd :client-var client-var :loop-var loop-var})
    {:ling-id ling-id
     :status :spawned
     :backend :agent-sdk
     :phase :idle}))

;;; =============================================================================
;;; Dispatch
;;; =============================================================================

(defn dispatch-headless-sdk!
  "Dispatch a task to an SDK ling via the persistent client (P3-T2).

   Supports two modes:
   1. SAA cycle: Runs silence -> abstract -> act phases sequentially
   2. Raw dispatch: Sends task as a single query (no SAA wrapping)

   Arguments:
     ling-id - ID of the spawned SDK ling
     task    - Task description string
     opts    - Optional map:
               :skip-silence?  - Skip silence phase (default: false)
               :skip-abstract? - Skip abstract phase (default: false)
               :phase          - Run only this specific phase
               :raw?           - Skip SAA, send task directly as query

   Returns:
     core.async channel that will receive all messages from all phases."
  [ling-id task & [{:keys [skip-silence? skip-abstract? phase raw?] :as _opts}]]
  {:pre [(string? ling-id)
         (string? task)]}
  (let [sess (session/get-session ling-id)]
    (when-not sess
      (throw (ex-info "SDK session not found" {:ling-id ling-id})))
    (when-not (:client-ref sess)
      (throw (ex-info "No persistent client (was spawn successful?)"
                      {:ling-id ling-id})))
    (let [out-ch (chan 4096)]
      ;; Run in a background thread
      (async/thread
        (try
          (if raw?
            ;; === RAW DISPATCH (single query, no SAA) ===
            (let [phase-ch (exec/execute-phase! ling-id task :dispatch)]
              (loop []
                (when-let [msg (<!! phase-ch)]
                  (>!! out-ch (assoc msg :saa-phase :dispatch))
                  (recur))))

            ;; === SAA CYCLE (multi-phase) ===
            (do
              ;; === SILENCE PHASE ===
              (when-not (or skip-silence? (and phase (not= phase :silence)))
                (let [silence-prompt (str "TASK: " task
                                          "\n\nExplore the codebase and collect context. "
                                          "List all relevant files, patterns, and observations.")
                      phase-ch (exec/execute-phase! ling-id silence-prompt :silence)]
                  (loop []
                    (when-let [msg (<!! phase-ch)]
                      (>!! out-ch (assoc msg :saa-phase :silence))
                      (when (= :message (:type msg))
                        (session/update-session! ling-id
                                                 {:observations (conj (or (:observations (session/get-session ling-id)) [])
                                                                      (:data msg))}))
                      (recur)))))

              ;; === ABSTRACT PHASE ===
              (when-not (or skip-abstract? (and phase (not= phase :abstract)))
                (let [observations (:observations (session/get-session ling-id))
                      abstract-prompt (str "Based on these observations from the Silence phase:\n"
                                           (pr-str observations)
                                           "\n\nSynthesize these into a concrete action plan for: " task)
                      phase-ch (exec/execute-phase! ling-id abstract-prompt :abstract)]
                  (loop []
                    (when-let [msg (<!! phase-ch)]
                      (>!! out-ch (assoc msg :saa-phase :abstract))
                      (recur)))))

              ;; === ACT PHASE ===
              (when-not (and phase (not= phase :act))
                (let [act-prompt (str "Execute the plan for: " task
                                      "\n\nFollow the plan precisely. Make changes file by file.")
                      phase-ch (exec/execute-phase! ling-id act-prompt :act)]
                  (loop []
                    (when-let [msg (<!! phase-ch)]
                      (>!! out-ch (assoc msg :saa-phase :act))
                      (recur)))))))

          ;; Dispatch complete
          (>!! out-ch {:type :saa-complete
                       :ling-id ling-id
                       :turn-count (:turn-count (session/get-session ling-id))
                       :observations-count (count (:observations (session/get-session ling-id)))})
          (log/info "[sdk.lifecycle] Dispatch complete"
                    {:ling-id ling-id
                     :turn-count (:turn-count (session/get-session ling-id))})

          (catch Exception e
            (log/error "[sdk.lifecycle] Dispatch failed"
                       {:ling-id ling-id :error (ex-message e)})
            (>!! out-ch {:type :error :error (ex-message e)}))
          (finally
            (close! out-ch))))
      out-ch)))

;;; =============================================================================
;;; Kill
;;; =============================================================================

(defn kill-headless-sdk!
  "Terminate an SDK ling session (P3-T2: graceful disconnect).

   Lifecycle: disconnect client -> stop event loop -> close channels -> unregister.

   Arguments:
     ling-id - ID of the SDK ling

   Returns:
     {:killed? true :ling-id ling-id}"
  [ling-id]
  (if-let [sess (session/get-session ling-id)]
    (let [safe-id (or (:py-safe-id sess) (session/ling-id->safe-id ling-id))
          client-var (:client-ref sess)
          loop-var (:py-loop-var sess)]
      ;; Graceful teardown: disconnect client, then stop loop
      (when (and client-var loop-var)
        (event-loop/disconnect-session-client! safe-id loop-var client-var)
        (event-loop/stop-session-loop! safe-id loop-var))
      ;; Close async channels
      (when-let [msg-ch (:message-ch sess)] (close! msg-ch))
      (when-let [res-ch (:result-ch sess)] (close! res-ch))
      ;; Remove from registry
      (session/unregister-session! ling-id)
      (log/info "[sdk.lifecycle] SDK ling killed (graceful)" {:ling-id ling-id})
      {:killed? true :ling-id ling-id})
    (throw (ex-info "SDK session not found" {:ling-id ling-id}))))

;;; =============================================================================
;;; Interrupt
;;; =============================================================================

(defn interrupt-headless-sdk!
  "Interrupt the current query of an SDK ling session.

   Sends client.interrupt() to the ClaudeSDKClient via the session's
   asyncio event loop using asyncio.run_coroutine_threadsafe().

   Safe to call from any thread.

   Arguments:
     ling-id - ID of the SDK ling to interrupt

   Returns:
     {:success? true/false :ling-id ling-id ...}

   Does NOT throw -- returns error map on failure (CLARITY-Y)."
  [ling-id]
  (if-let [sess (session/get-session ling-id)]
    (let [{:keys [client-ref py-loop-var phase]} sess]
      (if (and client-ref py-loop-var)
        (try
          (py/py-run (str "import asyncio\n"
                          "_hive_interrupt_client = globals().get('" client-ref "')\n"
                          "_hive_interrupt_loop = globals().get('" py-loop-var "')\n"
                          "if _hive_interrupt_client is not None and _hive_interrupt_loop is not None and _hive_interrupt_loop.is_running():\n"
                          "    _hive_interrupt_future = asyncio.run_coroutine_threadsafe(\n"
                          "        _hive_interrupt_client.interrupt(),\n"
                          "        _hive_interrupt_loop\n"
                          "    )\n"
                          "    _hive_interrupt_future.result(timeout=10)\n"
                          "    _hive_interrupt_success = True\n"
                          "else:\n"
                          "    _hive_interrupt_success = False\n"))
          (let [success? (py/py->clj (py/py-get-global "_hive_interrupt_success"))]
            ;; Clean up temp globals
            (try (py/py-run "globals().pop('_hive_interrupt_client', None)\nglobals().pop('_hive_interrupt_loop', None)\nglobals().pop('_hive_interrupt_future', None)\nglobals().pop('_hive_interrupt_success', None)\n")
                 (catch Exception _ nil))
            (if success?
              (do
                (log/info "[sdk.lifecycle] Interrupt sent" {:ling-id ling-id :phase phase})
                {:success? true :ling-id ling-id :phase phase})
              (do
                (log/warn "[sdk.lifecycle] Interrupt failed: client or loop not available"
                          {:ling-id ling-id :phase phase})
                {:success? false
                 :ling-id ling-id
                 :errors ["Client or event loop not available (phase may have completed)"]})))
          (catch Exception e
            (log/error "[sdk.lifecycle] Interrupt exception"
                       {:ling-id ling-id :error (ex-message e)})
            {:success? false
             :ling-id ling-id
             :errors [(str "Interrupt failed: " (ex-message e))]}))
        ;; No active phase
        {:success? false
         :ling-id ling-id
         :errors [(str "No active phase to interrupt (current phase: " (name (or phase :idle)) ")")]}))
    ;; Session not found
    {:success? false
     :ling-id ling-id
     :errors ["SDK session not found"]}))

;;; =============================================================================
;;; Status & Queries
;;; =============================================================================

(defn sdk-status-for
  "Get the status of an SDK ling, including interrupt and multi-turn info."
  [ling-id]
  (when-let [sess (session/get-session ling-id)]
    {:ling-id ling-id
     :phase (:phase sess)
     :phase-history (:phase-history sess)
     :observations-count (count (:observations sess))
     :started-at (:started-at sess)
     :uptime-ms (- (System/currentTimeMillis) (:started-at sess))
     :cwd (:cwd sess)
     :backend :agent-sdk
     :session-id (:session-id sess)
     ;; P3-T2: Multi-turn tracking
     :turn-count (or (:turn-count sess) 0)
     :has-persistent-client? (boolean (:client-ref sess))
     ;; P3-T3: Interrupt capability
     :interruptable? (boolean (and (:client-ref sess)
                                   (:py-loop-var sess)))}))

(defn list-sdk-sessions
  "List all active SDK sessions."
  []
  (->> @(session/session-registry-ref)
       keys
       (map sdk-status-for)
       (remove nil?)
       vec))

(defn sdk-session?
  "Check if a ling-id corresponds to an SDK session."
  [ling-id]
  (contains? @(session/session-registry-ref) ling-id))

(defn kill-all-sdk!
  "Kill all SDK sessions. For cleanup/testing."
  []
  (let [ids (keys @(session/session-registry-ref))
        results (for [id ids]
                  (try
                    (kill-headless-sdk! id)
                    {:success true :id id}
                    (catch Exception e
                      {:success false :id id :error (ex-message e)})))]
    {:killed (count (filter :success results))
     :errors (count (remove :success results))}))
