(ns hive-mcp.channel
  "Bidirectional communication channel between Clojure and Emacs.

   Built on Aleph/Manifold for robust async networking.

   Architecture:
   - Aleph TCP server accepts Emacs connections
   - Manifold streams handle bidirectional communication
   - Bencode message format (compatible with nREPL/Emacs)
   - core.async pub/sub for internal event routing

   Usage:
     (start-server! {:type :tcp :port 9998})
     (broadcast! {:type :hivemind-progress :data {...}})
     (subscribe! :hivemind-progress) ; => core.async channel
   "
  (:require [aleph.tcp :as tcp]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [clojure.core.async :as async :refer [go go-loop <! >! chan pub sub unsub close!]]
            [nrepl.bencode :as bencode]
            [taoensso.timbre :as log])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream PushbackInputStream]))

;; =============================================================================
;; Event Bus (core.async pub/sub for internal routing)
;; =============================================================================

(defonce ^:private event-chan (chan 1024))
(defonce ^:private event-pub (pub event-chan :type))

(defn publish!
  "Publish event to the internal event bus.
   Event must have :type key for routing."
  [event]
  (when-not (:type event)
    (throw (ex-info "Event must have :type" {:event event})))
  (async/put! event-chan event))

(defn subscribe!
  "Subscribe to events of given type.
   Returns a core.async channel that receives matching events."
  [event-type]
  (let [ch (chan 256)]
    (sub event-pub event-type ch)
    ch))

(defn unsubscribe!
  "Unsubscribe channel from event type."
  [event-type ch]
  (unsub event-pub event-type ch)
  (close! ch))

;; =============================================================================
;; Bencode Helpers
;; =============================================================================

(defn- encode-msg
  "Encode Clojure map to bencode bytes."
  [msg]
  (let [baos (ByteArrayOutputStream.)]
    (bencode/write-bencode baos msg)
    (.toByteArray baos)))

(defn- bytes->str
  "Convert byte arrays to strings recursively."
  [v]
  (cond
    (bytes? v) (String. ^bytes v "UTF-8")
    (map? v) (into {} (map (fn [[k v]] [(bytes->str k) (bytes->str v)]) v))
    (sequential? v) (mapv bytes->str v)
    :else v))

(defn- decode-msg
  "Decode bencode bytes to Clojure map."
  [^bytes data]
  (try
    (let [in (PushbackInputStream. (ByteArrayInputStream. data))]
      (-> (bencode/read-bencode in)
          bytes->str))
    (catch Exception e
      (log/debug "Bencode decode error:" (.getMessage e))
      nil)))

;; =============================================================================
;; Server State
;; =============================================================================

(defonce ^:private server-state (atom nil))

(defn- handle-client
  "Handle incoming messages from a client stream."
  [stream client-id]
  (d/loop []
    (d/chain
     (s/take! stream)
     (fn [data]
       (when data
         (when-let [msg (decode-msg data)]
           (log/debug "Received from" client-id ":" msg)
           ;; Route to internal pub/sub
           (when-let [type-str (get msg "type")]
             (publish! (assoc msg :type (keyword type-str) :client-id client-id))))
         (d/recur))))))

(defn- client-handler
  "Handler function for new client connections."
  [stream info]
  (let [client-id (str (gensym "client-"))
        clients (:clients @server-state)]
    (log/info "Client connected:" client-id "from" (:remote-addr info))
    (swap! clients assoc client-id stream)

    ;; Handle incoming messages
    (handle-client stream client-id)

    ;; Cleanup on disconnect
    (s/on-closed stream
                 (fn []
                   (log/info "Client disconnected:" client-id)
                   (swap! clients dissoc client-id)))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn start-server!
  "Start the channel server on TCP port.
   Emacs will connect to this server.

   Options:
     :port - TCP port (default: 9998)

   Returns server state map."
  [{:keys [port] :or {port 9998}}]
  (if @server-state
    (do
      (log/warn "Server already running")
      @server-state)
    (let [clients (atom {})
          server (tcp/start-server client-handler {:port port})]
      (log/info "Aleph TCP server listening on port" port)
      (reset! server-state
              {:server server
               :port port
               :clients clients})
      @server-state)))

(defn stop-server!
  "Stop the channel server."
  []
  (when-let [{:keys [server clients]} @server-state]
    ;; Close all client streams
    (doseq [[id stream] @clients]
      (s/close! stream))
    ;; Stop the server
    (.close ^java.io.Closeable server)
    (reset! server-state nil)
    (log/info "Server stopped")))

(defn broadcast!
  "Send message to all connected clients."
  [msg]
  (when-let [{:keys [clients]} @server-state]
    (let [encoded (encode-msg msg)]
      (doseq [[id stream] @clients]
        (when-not (s/closed? stream)
          (d/catch
           (s/put! stream encoded)
           (fn [e]
             (log/warn "Broadcast to" id "failed:" (.getMessage e)))))))))

(defn server-connected?
  "Check if the channel server is running and has connected clients."
  []
  (when-let [{:keys [clients]} @server-state]
    (some #(not (s/closed? (second %))) @clients)))

(defn client-count
  "Return number of connected clients."
  []
  (when-let [{:keys [clients]} @server-state]
    (count (filter #(not (s/closed? (second %))) @clients))))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn emit-event!
  "Emit an event to all connected clients and local subscribers.

   Example:
     (emit-event! :task-completed {:task-id \"123\" :result \"done\"})"
  [event-type data]
  (let [string-data (into {} (map (fn [[k v]] [(name k) v]) data))
        event (assoc string-data
                     "type" (name event-type)
                     "timestamp" (System/currentTimeMillis)
                     :type event-type)]
    ;; Local pub/sub
    (publish! event)
    ;; Broadcast to Emacs clients
    (broadcast! event)))

(comment
  ;; Development REPL examples

  ;; Start server
  (start-server! {:port 9998})

  ;; Check status
  (server-connected?)
  (client-count)

  ;; Test broadcast
  (broadcast! {:type :hivemind-progress
               :data {:agent-id "test" :message "hello"}})

  ;; Stop server
  (stop-server!))

;; =============================================================================
;; MCP Tool Definitions
;; =============================================================================

(def channel-tools
  "Channel-related MCP tools - currently empty as channel operations
   are handled internally and not exposed as user-facing tools."
  [])
