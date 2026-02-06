(ns olympus-web.ws.client
  "WebSocket client with exponential backoff reconnection.

   Provides clean API for connecting to hive-mcp Olympus WS server (port 7911).

   Connection lifecycle:
   1. connect! opens WebSocket to configured URL
   2. On message -> dispatches :ws/message-received re-frame event
   3. On close  -> starts exponential backoff reconnection
   4. disconnect! -> clean shutdown, cancels pending reconnect

   Backoff schedule: 1s -> 2s -> 4s -> 8s -> 16s -> 30s (capped)
   Jitter: +/- 20% to prevent thundering herd

   Server protocol (from transport/olympus.clj):
   - Server sends {:type :init-snapshot ...} on connect
   - Client can send 'ping' for keepalive
   - Client can send {:type 'request-snapshot' :view 'agents'|'waves'|'kg'|'project-tree'}
   - Server broadcasts typed events: :hivemind-shout, :agent-spawned, :wave-task-update, etc."
  (:require [re-frame.core :as rf]
            [olympus-web.config :as config]))

;; =============================================================================
;; State (module-private atoms)
;; =============================================================================

(defonce ^:private ws-instance
  "Current WebSocket instance. nil when disconnected."
  (atom nil))

(defonce ^:private reconnect-timer
  "Handle for pending reconnect setTimeout. Used to cancel on disconnect!."
  (atom nil))

(defonce ^:private heartbeat-timer
  "Handle for periodic heartbeat setInterval."
  (atom nil))

(defonce ^:private intentional-close?
  "Flag to distinguish intentional disconnect! from unexpected close."
  (atom false))

;; =============================================================================
;; Exponential Backoff
;; =============================================================================

(defn compute-backoff-delay
  "Compute reconnection delay with exponential backoff and jitter.

   Formula: min(base * 2^attempt, max) * (1 + random[-0.2, +0.2])

   Parameters:
     attempt - Zero-based reconnection attempt number

   Returns delay in milliseconds.

   Examples:
     attempt 0 -> ~1000ms  (1s)
     attempt 1 -> ~2000ms  (2s)
     attempt 2 -> ~4000ms  (4s)
     attempt 3 -> ~8000ms  (8s)
     attempt 4 -> ~16000ms (16s)
     attempt 5 -> ~30000ms (30s, capped)"
  [attempt]
  (let [base    config/ws-backoff-base-ms
        max-d   config/ws-backoff-max-ms
        ;; Exponential: base * 2^attempt, capped at max
        raw     (min (* base (js/Math.pow 2 attempt)) max-d)
        ;; Jitter: +/- 20% to prevent thundering herd
        jitter  (* raw (- (* 0.4 (js/Math.random)) 0.2))]
    (js/Math.round (+ raw jitter))))

;; =============================================================================
;; Internal Helpers
;; =============================================================================

(defn- cancel-reconnect-timer!
  "Cancel any pending reconnect timer."
  []
  (when-let [timer @reconnect-timer]
    (js/clearTimeout timer)
    (reset! reconnect-timer nil)))

(defn- stop-heartbeat!
  "Stop the heartbeat interval."
  []
  (when-let [timer @heartbeat-timer]
    (js/clearInterval timer)
    (reset! heartbeat-timer nil)))

(defn- start-heartbeat!
  "Start periodic ping/pong heartbeat.
   Sends 'ping' every heartbeat-interval-ms to keep connection alive."
  []
  (stop-heartbeat!)
  (reset! heartbeat-timer
          (js/setInterval
           (fn []
             (when-let [ws @ws-instance]
               (when (= (.-readyState ws) 1) ;; WebSocket.OPEN
                 (.send ws "ping"))))
           config/ws-heartbeat-interval-ms)))

(defn- schedule-reconnect!
  "Schedule a reconnection attempt with exponential backoff.

   Parameters:
     attempt - Current attempt number (0-based)"
  [attempt]
  (cancel-reconnect-timer!)
  (let [delay (compute-backoff-delay attempt)]
    (js/console.log (str "WebSocket reconnecting in " delay "ms (attempt " (inc attempt) ")"))
    (rf/dispatch [:ws/reconnect-scheduled {:attempt attempt :delay-ms delay}])
    (reset! reconnect-timer
            (js/setTimeout
             (fn []
               (reset! reconnect-timer nil)
               (rf/dispatch [:ws/connect]))
             delay))))

;; =============================================================================
;; WebSocket Connection
;; =============================================================================

(defn- create-websocket!
  "Create a WebSocket connection and wire up event handlers.

   Parameters:
     url - WebSocket URL (e.g., 'ws://localhost:7911')

   Side effects:
     - Creates WebSocket, stores in ws-instance atom
     - Dispatches re-frame events on open/close/error/message
     - Starts heartbeat on successful connection
     - Schedules reconnect on unexpected close"
  [url]
  (try
    (let [ws (js/WebSocket. url)]
      (reset! ws-instance ws)
      (reset! intentional-close? false)

      (set! (.-onopen ws)
            (fn [_]
              (js/console.log "WebSocket connected to" url)
              (start-heartbeat!)
              (rf/dispatch [:ws/connected])))

      (set! (.-onclose ws)
            (fn [event]
              (let [code   (.-code event)
                    reason (.-reason event)]
                (js/console.log "WebSocket closed:" code reason)
                (stop-heartbeat!)
                (reset! ws-instance nil)
                (if @intentional-close?
                  ;; Intentional close - don't reconnect
                  (rf/dispatch [:ws/disconnected-clean])
                  ;; Unexpected close - trigger reconnect with backoff
                  (rf/dispatch [:ws/disconnected reason])))))

      (set! (.-onerror ws)
            (fn [error]
              (js/console.error "WebSocket error:" error)
              (rf/dispatch [:ws/error error])))

      (set! (.-onmessage ws)
            (fn [event]
              (let [data (.-data event)]
                ;; Silently handle pong responses
                (when-not (= data "pong")
                  (rf/dispatch [:ws/message-received data]))))))
    (catch :default e
      (js/console.error "Failed to create WebSocket:" e)
      (rf/dispatch [:ws/error e]))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn connect!
  "Open WebSocket connection to the Olympus WS server.

   Parameters:
     url - (optional) WebSocket URL, defaults to config/ws-url

   Cancels any pending reconnect timer before connecting."
  ([] (connect! config/ws-url))
  ([url]
   (cancel-reconnect-timer!)
   ;; Close existing connection if any
   (when-let [old-ws @ws-instance]
     (reset! intentional-close? true)
     (.close old-ws))
   (create-websocket! url)))

(defn disconnect!
  "Cleanly close the WebSocket connection.
   Cancels pending reconnect timers and stops heartbeat.
   Will NOT trigger automatic reconnection."
  []
  (cancel-reconnect-timer!)
  (stop-heartbeat!)
  (reset! intentional-close? true)
  (when-let [ws @ws-instance]
    (.close ws)
    (reset! ws-instance nil)))

(defn send!
  "Send a message through the WebSocket.

   Parameters:
     msg - String or object to send. Objects will be JSON-serialized.

   Returns true if message was sent, false if connection not open."
  [msg]
  (when-let [ws @ws-instance]
    (when (= (.-readyState ws) 1) ;; WebSocket.OPEN
      (let [payload (if (string? msg)
                      msg
                      (js/JSON.stringify (clj->js msg)))]
        (.send ws payload)
        true))))

(defn connected?
  "Check if WebSocket is currently open."
  []
  (when-let [ws @ws-instance]
    (= (.-readyState ws) 1)))

(defn request-snapshot!
  "Request a fresh snapshot for a specific view from the server.

   Parameters:
     view - Keyword: :agents, :waves, :kg, or :project-tree"
  [view]
  (send! {:type "request-snapshot" :view (name view)}))

;; =============================================================================
;; Re-frame Effect Handlers
;; =============================================================================

(rf/reg-fx
 :ws/open
 (fn [{:keys [url]}]
   (connect! url)))

(rf/reg-fx
 :ws/close
 (fn [_]
   (disconnect!)))

(rf/reg-fx
 :ws/send
 (fn [{:keys [message]}]
   (send! message)))

(rf/reg-fx
 :ws/schedule-reconnect
 (fn [{:keys [attempt]}]
   (schedule-reconnect! attempt)))
