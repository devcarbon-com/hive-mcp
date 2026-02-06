(ns hive-mcp.protocols.channel
  "Protocol definition for messaging channel abstractions.

   Defines the core interface for message-based communication channels
   used throughout hive-mcp for agent coordination, event distribution,
   and inter-process messaging.

   Channel Types (implementations):
   - In-memory channels (core.async based)
   - Redis pub/sub channels
   - MQTT broker channels
   - WebSocket channels
   - Unix socket channels

   Architecture:
   ```
   Publishers -> IChannel -> Subscribers
                    |
              Message Buffer
   ```

   SOLID-I: Interface segregation - messaging operations only.
   SOLID-O: Open for extension via new channel implementations.
   SOLID-D: Depend on abstraction (IChannel), not concretions.
   CLARITY-L: Layers stay pure - protocol is the boundary between
              messaging domain logic and transport implementation.
   CLARITY-Y: Yield safe failure - graceful degradation on channel errors."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IChannel Protocol (Core Messaging)
;;; ============================================================================

(defprotocol IChannel
  "Messaging channel protocol for publish/subscribe communication.

   Provides a unified interface for message passing between components,
   agents, and external systems. Channels are the backbone of hive-mcp's
   event-driven architecture.

   Implementations:
   - AsyncChannel: core.async based, in-process, fast (default)
   - RedisChannel: Redis pub/sub, distributed, persistent
   - MqttChannel: MQTT broker, IoT-friendly, QoS support
   - WebSocketChannel: Browser/client communication

   Message shape:
   {:id        \"msg-uuid\"           ; Unique message ID
    :topic     \"agent.events\"       ; Routing topic/subject
    :payload   {...}                 ; Message content (any serializable data)
    :timestamp #inst \"...\"          ; When message was created
    :sender    \"agent-id\"           ; Source identifier
    :headers   {:priority :high      ; Optional metadata
                :ttl-ms 30000
                :correlation-id \"...\"}}

   Subscription shape:
   {:id       \"sub-uuid\"            ; Unique subscription ID
    :topic    \"agent.*\"             ; Topic pattern (may include wildcards)
    :handler  (fn [msg] ...)        ; Callback function
    :opts     {:buffer-size 100     ; Implementation-specific options
               :filter (fn [m] ...)}}"

  ;;; =========================================================================
  ;;; Core Operations
  ;;; =========================================================================

  (channel-id [this]
    "Return unique identifier for this channel instance.

     Returns:
       Keyword or string identifier.
       Example: :hivemind-events, :agent-coord-123")

  (channel-info [this]
    "Return metadata about this channel.

     Returns map with:
       :id           - Channel identifier (same as channel-id)
       :name         - Human-readable name
       :description  - Channel purpose description
       :type         - Implementation type (:async, :redis, :mqtt, :websocket)
       :created-at   - Channel creation timestamp
       :capabilities - Set of supported features (:wildcards, :persistence,
                       :qos, :replay, :acknowledgment)")

  ;;; =========================================================================
  ;;; Send Operations
  ;;; =========================================================================

  (send! [this topic payload] [this topic payload opts]
    "Send a message to the channel on the specified topic.

     Arguments:
       topic   - String topic/subject for message routing
                 Examples: \"agent.started\", \"hivemind.task.123\"
       payload - Message content (must be serializable)
       opts    - Optional map with:
                 :headers      - Additional message headers
                 :sender       - Sender identifier (auto-detected if nil)
                 :timeout-ms   - Send timeout (default: 5000)
                 :priority     - :low, :normal, :high (default: :normal)
                 :ttl-ms       - Message time-to-live
                 :correlation-id - For request/response patterns

     Returns map with:
       :success?   - Boolean indicating send success
       :message-id - Generated message ID
       :errors     - Vector of error messages (empty on success)

     CLARITY-Y: Must not throw - return :success? false on failure.")

  (send-async! [this topic payload opts]
    "Send a message asynchronously without waiting for confirmation.

     Arguments:
       Same as send!

     Returns:
       A deferred/promise that resolves to send! result map.
       Useful for fire-and-forget patterns.

     Note: Not all implementations support true async. Some may
     simply wrap send! in a future.")

  (broadcast! [this payload opts]
    "Send a message to all subscribers regardless of topic.

     Arguments:
       payload - Message content
       opts    - Same as send!, plus:
                 :exclude - Set of subscriber IDs to exclude

     Returns:
       Map with :success?, :delivered-count, :errors

     Use sparingly - broadcasts can be expensive.")

  ;;; =========================================================================
  ;;; Receive Operations
  ;;; =========================================================================

  (receive! [this topic] [this topic opts]
    "Receive the next message on a topic (blocking).

     Arguments:
       topic - Topic to receive from (exact match, no wildcards)
       opts  - Optional map with:
               :timeout-ms - Max wait time (default: 30000, nil = infinite)
               :filter     - Predicate fn to filter messages

     Returns:
       Message map on success, or:
       :timeout - If timeout reached
       :closed  - If channel was closed
       nil      - On error

     Note: Creates an implicit subscription. For persistent subscriptions,
     use subscribe with a handler instead.")

  (receive-batch! [this topic opts]
    "Receive multiple messages in a batch.

     Arguments:
       topic - Topic to receive from
       opts  - Map with:
               :max-messages - Maximum messages to receive (required)
               :timeout-ms   - Max wait time for batch (default: 5000)
               :min-messages - Minimum before returning (default: 1)

     Returns map with:
       :messages - Vector of message maps
       :count    - Number of messages received
       :complete? - Whether max-messages was reached")

  ;;; =========================================================================
  ;;; Subscription Management
  ;;; =========================================================================

  (subscribe! [this topic handler] [this topic handler opts]
    "Subscribe to messages on a topic pattern.

     Arguments:
       topic   - Topic pattern to subscribe to
                 Supports wildcards (implementation-dependent):
                 - \"agent.*\"     - Single-level wildcard
                 - \"agent.#\"     - Multi-level wildcard (MQTT-style)
                 - \"agent.>\"     - Multi-level wildcard (NATS-style)
       handler - Function (fn [message] ...) called for each message
                 Handler receives full message map.
                 Return value is ignored (use acknowledgment for flow control).
       opts    - Optional map with:
                 :buffer-size - Max pending messages (default: 100)
                 :filter      - Additional filter predicate
                 :error-handler - (fn [error msg] ...) for handler errors
                 :async?      - Run handler in separate thread (default: true)

     Returns map with:
       :success?        - Boolean indicating subscription success
       :subscription-id - Unique ID for this subscription (use with unsubscribe!)
       :errors          - Vector of error messages

     Multiple subscriptions to same topic are allowed - each receives a copy.")

  (unsubscribe! [this subscription-id]
    "Remove a subscription by its ID.

     Arguments:
       subscription-id - ID returned from subscribe!

     Returns map with:
       :success? - Boolean indicating unsubscription success
       :errors   - Vector of error messages (empty on success)

     Idempotent - safe to call multiple times with same ID.")

  (subscriptions [this]
    "List all active subscriptions on this channel.

     Returns:
       Vector of subscription info maps with:
       :id           - Subscription ID
       :topic        - Topic pattern
       :created-at   - When subscription was created
       :message-count - Messages delivered to this subscription
       :pending-count - Messages buffered/pending delivery")

  ;;; =========================================================================
  ;;; Channel Lifecycle
  ;;; =========================================================================

  (open? [this]
    "Check if channel is open and accepting messages.

     Returns:
       Boolean indicating channel is operational.")

  (close! [this]
    "Close the channel and release resources.

     Behavior:
     - Stops accepting new messages
     - Delivers pending messages (best effort)
     - Removes all subscriptions
     - Releases connections/buffers

     Returns map with:
       :success?         - Boolean indicating clean close
       :pending-dropped  - Count of messages that couldn't be delivered
       :subscriptions-removed - Count of subscriptions removed

     Idempotent - safe to call multiple times.")

  (drain! [this opts]
    "Gracefully drain the channel before closing.

     Arguments:
       opts - Map with:
              :timeout-ms - Max time to wait for drain (default: 10000)
              :on-message - Optional callback for each drained message

     Returns map with:
       :success?       - Whether drain completed within timeout
       :drained-count  - Number of messages drained
       :remaining      - Messages that couldn't be drained

     Use before close! for graceful shutdown.")

  ;;; =========================================================================
  ;;; Status & Metrics
  ;;; =========================================================================

  (channel-status [this]
    "Get comprehensive channel status for introspection.

     Returns map with:
       :id                - Channel identifier
       :open?             - Whether channel is open
       :type              - Implementation type
       :subscription-count - Number of active subscriptions
       :pending-count     - Messages waiting for delivery
       :buffer-size       - Total buffer capacity
       :buffer-used       - Current buffer utilization
       :created-at        - Channel creation time
       :last-activity     - Timestamp of last send/receive
       :metrics           - Implementation-specific metrics map"))

;;; ============================================================================
;;; IChannelWithAck Protocol (Optional Extension)
;;; ============================================================================

(defprotocol IChannelWithAck
  "Extended protocol for channels supporting message acknowledgment.

   Enables reliable delivery patterns:
   - At-least-once delivery (with ack)
   - At-most-once delivery (without ack, default IChannel)
   - Exactly-once delivery (with dedup, implementation-dependent)

   Not all backends support this - check with ack-channel? predicate."

  (ack! [this message-id]
    "Acknowledge successful processing of a message.

     Arguments:
       message-id - ID of message to acknowledge

     Returns map with:
       :success? - Boolean indicating ack was recorded
       :errors   - Vector of error messages

     Unacknowledged messages may be redelivered (implementation-dependent).")

  (nack! [this message-id opts]
    "Negative acknowledge - message processing failed.

     Arguments:
       message-id - ID of message to nack
       opts       - Map with:
                    :requeue?   - Requeue for redelivery (default: true)
                    :delay-ms   - Delay before redelivery (default: 0)
                    :reason     - Failure reason string

     Returns map with:
       :success?  - Boolean indicating nack was recorded
       :requeued? - Whether message was requeued
       :errors    - Vector of error messages")

  (pending-acks [this]
    "Get messages awaiting acknowledgment.

     Returns:
       Vector of message IDs that have been delivered but not acked/nacked."))

(defn ack-channel?
  "Check if channel supports acknowledgment protocol.

   Arguments:
     channel - IChannel implementation

   Returns:
     true if channel implements IChannelWithAck."
  [channel]
  (satisfies? IChannelWithAck channel))

;;; ============================================================================
;;; IChannelWithReplay Protocol (Optional Extension)
;;; ============================================================================

(defprotocol IChannelWithReplay
  "Extended protocol for channels supporting message replay.

   Enables:
   - Late subscriber catch-up
   - Audit and debugging
   - Event sourcing patterns

   Implementations: Redis Streams, Kafka, EventStore"

  (replay! [this topic opts]
    "Replay historical messages on a topic.

     Arguments:
       topic - Topic to replay
       opts  - Map with:
               :from      - Start point (:beginning, timestamp, message-id)
               :to        - End point (:now, timestamp, message-id)
               :limit     - Max messages to replay
               :handler   - (fn [msg] ...) called for each message

     Returns map with:
       :success?      - Boolean indicating replay completed
       :replayed-count - Number of messages replayed
       :errors        - Vector of error messages")

  (history [this topic opts]
    "Get message history without triggering handlers.

     Arguments:
       topic - Topic to query
       opts  - Same as replay! but without :handler

     Returns map with:
       :success?  - Boolean indicating query success
       :messages  - Vector of historical message maps
       :has-more? - Whether more messages exist beyond limit"))

(defn replay-channel?
  "Check if channel supports replay protocol.

   Arguments:
     channel - IChannel implementation

   Returns:
     true if channel implements IChannelWithReplay."
  [channel]
  (satisfies? IChannelWithReplay channel))

;;; ============================================================================
;;; Channel Registry
;;; ============================================================================

(defonce ^:private channel-registry (atom {}))

(defn register-channel!
  "Register a channel instance in the global registry.

   Arguments:
     channel - Implementation of IChannel protocol

   Returns:
     The channel.

   Note: Does NOT create the channel - just registers an existing instance.
   Use channel factory functions to create channels first."
  [channel]
  {:pre [(satisfies? IChannel channel)]}
  (let [id (channel-id channel)]
    (swap! channel-registry assoc id channel)
    channel))

(defn get-channel
  "Get channel by ID from the registry.

   Arguments:
     id - Channel identifier (keyword or string)

   Returns:
     Channel instance or nil if not found."
  [id]
  (get @channel-registry id))

(defn list-channels
  "List all registered channels with their status.

   Returns:
     Vector of maps with channel info + current status."
  []
  (->> @channel-registry
       vals
       (mapv (fn [ch]
               (merge (channel-info ch)
                      {:status (channel-status ch)})))))

(defn channel-registered?
  "Check if a channel is registered.

   Arguments:
     id - Channel identifier

   Returns:
     Boolean."
  [id]
  (contains? @channel-registry id))

(defn unregister-channel!
  "Unregister a channel. Closes it first if open.

   Arguments:
     id - Channel identifier

   Returns:
     true if channel was removed, false if not found."
  [id]
  (if-let [channel (get-channel id)]
    (do
      (when (open? channel)
        (close! channel))
      (swap! channel-registry dissoc id)
      true)
    false))

(defn reset-registry!
  "Reset the channel registry to empty state.
   Closes all registered channels first.

   WARNING: Destructive operation - use for testing only.

   Returns:
     Count of channels that were closed."
  []
  (let [channels (vals @channel-registry)
        count (count channels)]
    (doseq [ch channels]
      (when (open? ch)
        (close! ch)))
    (reset! channel-registry {})
    count))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defn generate-message-id
  "Generate a unique message ID.
   Format: msg-<timestamp>-<random-hex>

   Returns:
     String message ID."
  []
  (str "msg-" (System/currentTimeMillis) "-" (format "%08x" (rand-int Integer/MAX_VALUE))))

(defn generate-subscription-id
  "Generate a unique subscription ID.
   Format: sub-<timestamp>-<random-hex>

   Returns:
     String subscription ID."
  []
  (str "sub-" (System/currentTimeMillis) "-" (format "%08x" (rand-int Integer/MAX_VALUE))))

(defn make-message
  "Create a well-formed message map.

   Arguments:
     topic   - Message topic
     payload - Message content
     opts    - Optional map with :sender, :headers, :priority, :ttl-ms

   Returns:
     Complete message map with all standard fields."
  [topic payload & [opts]]
  (let [{:keys [sender headers priority ttl-ms correlation-id]} opts]
    {:id (generate-message-id)
     :topic topic
     :payload payload
     :timestamp (java.time.Instant/now)
     :sender (or sender "unknown")
     :headers (merge {:priority (or priority :normal)}
                     (when ttl-ms {:ttl-ms ttl-ms})
                     (when correlation-id {:correlation-id correlation-id})
                     headers)}))

(defn topic-matches?
  "Check if a topic matches a subscription pattern.

   Arguments:
     pattern - Subscription pattern (may include wildcards)
     topic   - Actual message topic

   Wildcard rules:
     *  - Matches exactly one level (e.g., \"a.*\" matches \"a.b\" but not \"a.b.c\")
     #  - Matches zero or more levels (e.g., \"a.#\" matches \"a\", \"a.b\", \"a.b.c\")
     >  - Same as # (NATS-style)

   Returns:
     Boolean indicating match."
  [pattern topic]
  (let [pattern-parts (str/split pattern #"\.")
        topic-parts (str/split topic #"\.")]
    (loop [pp pattern-parts
           tp topic-parts]
      (cond
        ;; Both exhausted - match
        (and (empty? pp) (empty? tp)) true

        ;; Multi-level wildcard - match rest
        (and (seq pp) (#{"#" ">"} (first pp))) true

        ;; Pattern exhausted but topic has more - no match
        (empty? pp) false

        ;; Topic exhausted but pattern has more (unless wildcard)
        (empty? tp) (and (= 1 (count pp)) (#{"#" ">"} (first pp)))

        ;; Single-level wildcard - match one level
        (= "*" (first pp)) (recur (rest pp) (rest tp))

        ;; Exact match required
        (= (first pp) (first tp)) (recur (rest pp) (rest tp))

        ;; No match
        :else false))))

;;; ============================================================================
;;; NoopChannel (No-op Fallback)
;;; ============================================================================

(defrecord NoopChannel [id state]
  IChannel
  (channel-id [_] id)

  (channel-info [_]
    {:id id
     :name "Noop Channel"
     :description "No-operation channel for testing and development"
     :type :noop
     :created-at (java.time.Instant/now)
     :capabilities #{}})

  (send! [_ _topic _payload]
    {:success? true
     :message-id (generate-message-id)
     :errors []})

  (send! [_ _topic _payload _opts]
    {:success? true
     :message-id (generate-message-id)
     :errors []})

  (send-async! [_ _topic _payload _opts]
    (future {:success? true
             :message-id (generate-message-id)
             :errors []}))

  (broadcast! [_ _payload _opts]
    {:success? true
     :delivered-count 0
     :errors []})

  (receive! [_ _topic]
    nil)

  (receive! [_ _topic _opts]
    nil)

  (receive-batch! [_ _topic _opts]
    {:messages []
     :count 0
     :complete? true})

  (subscribe! [_ _topic _handler]
    {:success? true
     :subscription-id (generate-subscription-id)
     :errors []})

  (subscribe! [this topic handler _opts]
    (subscribe! this topic handler))

  (unsubscribe! [_ _subscription-id]
    {:success? true
     :errors []})

  (subscriptions [_]
    [])

  (open? [_]
    (boolean (:open? @state)))

  (close! [_]
    (swap! state assoc :open? false)
    {:success? true
     :pending-dropped 0
     :subscriptions-removed 0})

  (drain! [_ _opts]
    {:success? true
     :drained-count 0
     :remaining 0})

  (channel-status [_]
    {:id id
     :open? (boolean (:open? @state))
     :type :noop
     :subscription-count 0
     :pending-count 0
     :buffer-size 0
     :buffer-used 0
     :created-at (:created-at @state)
     :last-activity nil
     :metrics {}}))

(defn ->noop-channel
  "Create a NoopChannel for testing.

   Arguments:
     id - Keyword identifier for this channel (default: :noop-channel)"
  ([] (->noop-channel :noop-channel))
  ([id] (->NoopChannel id (atom {:open? true
                                 :created-at (java.time.Instant/now)}))))

;;; ============================================================================
;;; IVoice Protocol (Voice/Call Extension)
;;; ============================================================================

(defprotocol IVoice
  "Protocol for voice/call communication extending the channel concept.

   Provides real-time voice call management for hive-mcp agents:
   - Agent-to-agent voice calls
   - Conference calls for multi-agent coordination
   - Mute/unmute for flow control

   Implementations:
   - WebRTCVoice: Browser-based real-time voice
   - SIPVoice: Traditional telephony integration
   - NoopVoice: Testing fallback

   Call shape:
   {:id         \"call-uuid\"           ; Unique call ID
    :initiator  \"agent-id\"            ; Who started the call
    :participants [\"agent-1\" \"agent-2\"] ; All participants
    :status     :ringing | :active | :on-hold | :ended
    :started-at #inst \"...\"           ; Call start time
    :muted?     false                  ; Local mute state
    :metadata   {:codec \"opus\"        ; Implementation-specific
                 :bitrate 48000}}

   SOLID-I: Interface segregation - voice operations only.
   SOLID-O: Open for extension via new voice implementations.
   CLARITY-Y: Yield safe failure - graceful degradation on call errors."

  (voice-id [this]
    "Return unique identifier for this voice instance.

     Returns:
       Keyword or string identifier.
       Example: :webrtc-voice, :sip-voice-123")

  (start-call [this participants opts]
    "Initiate a voice call with the given participants.

     Arguments:
       participants - Vector of participant identifiers (agent IDs, user IDs)
       opts         - Optional map with:
                      :codec       - Audio codec (:opus, :g711, :g722)
                      :bitrate     - Target bitrate in bps
                      :timeout-ms  - Ring timeout (default: 30000)
                      :metadata    - Additional call metadata

     Returns map with:
       :success?  - Boolean indicating call initiation success
       :call-id   - Unique call ID for this call session
       :status    - Initial call status (:ringing, :active, :failed)
       :errors    - Vector of error messages (empty on success)

     CLARITY-Y: Must not throw - return :success? false on failure.")

  (end-call [this call-id]
    "End an active call.

     Arguments:
       call-id - ID of the call to end (from start-call)

     Returns map with:
       :success?    - Boolean indicating call was ended
       :duration-ms - Call duration in milliseconds
       :errors      - Vector of error messages

     Idempotent - safe to call on already-ended calls.")

  (mute [this call-id]
    "Mute the local audio in a call.

     Arguments:
       call-id - ID of the active call

     Returns map with:
       :success? - Boolean indicating mute was applied
       :muted?   - Current mute state (should be true)
       :errors   - Vector of error messages

     Idempotent - safe to call when already muted.")

  (unmute [this call-id]
    "Unmute the local audio in a call.

     Arguments:
       call-id - ID of the active call

     Returns map with:
       :success? - Boolean indicating unmute was applied
       :muted?   - Current mute state (should be false)
       :errors   - Vector of error messages

     Idempotent - safe to call when already unmuted.")

  (call-status [this call-id]
    "Get the current status of a call.

     Arguments:
       call-id - ID of the call to query

     Returns map with:
       :call-id       - Call identifier
       :status        - :ringing, :active, :on-hold, :ended, :unknown
       :participants  - Vector of current participants
       :initiator     - Who started the call
       :started-at    - Call start timestamp
       :duration-ms   - Duration so far (nil if not active)
       :muted?        - Local mute state
       :metadata      - Implementation-specific metadata

     Returns nil if call-id is unknown."))

;;; ============================================================================
;;; NoopVoice (No-op Fallback)
;;; ============================================================================

(defrecord NoopVoice [id state]
  IVoice
  (voice-id [_] id)

  (start-call [_ participants _opts]
    (let [call-id (str "call-" (System/currentTimeMillis) "-"
                       (format "%08x" (rand-int Integer/MAX_VALUE)))]
      (swap! state assoc-in [:calls call-id]
             {:call-id call-id
              :status :active
              :participants (vec participants)
              :initiator (first participants)
              :started-at (java.time.Instant/now)
              :muted? false})
      {:success? true
       :call-id call-id
       :status :active
       :errors []}))

  (end-call [_ call-id]
    (if-let [call (get-in @state [:calls call-id])]
      (let [duration-ms (when (:started-at call)
                          (- (System/currentTimeMillis)
                             (.toEpochMilli ^java.time.Instant (:started-at call))))]
        (swap! state assoc-in [:calls call-id :status] :ended)
        {:success? true
         :duration-ms duration-ms
         :errors []})
      {:success? false
       :duration-ms nil
       :errors ["Unknown call-id"]}))

  (mute [_ call-id]
    (if (get-in @state [:calls call-id])
      (do
        (swap! state assoc-in [:calls call-id :muted?] true)
        {:success? true :muted? true :errors []})
      {:success? false :muted? false :errors ["Unknown call-id"]}))

  (unmute [_ call-id]
    (if (get-in @state [:calls call-id])
      (do
        (swap! state assoc-in [:calls call-id :muted?] false)
        {:success? true :muted? false :errors []})
      {:success? false :muted? true :errors ["Unknown call-id"]}))

  (call-status [_ call-id]
    (when-let [call (get-in @state [:calls call-id])]
      (let [duration-ms (when (and (= :active (:status call)) (:started-at call))
                          (- (System/currentTimeMillis)
                             (.toEpochMilli ^java.time.Instant (:started-at call))))]
        (assoc call :duration-ms duration-ms)))))

(defn ->noop-voice
  "Create a NoopVoice for testing.

   Arguments:
     id - Keyword identifier for this voice instance (default: :noop-voice)"
  ([] (->noop-voice :noop-voice))
  ([id] (->NoopVoice id (atom {:calls {}}))))

;;; ============================================================================
;;; Voice Registry
;;; ============================================================================

(defonce ^:private voice-registry (atom {}))

(defn register-voice!
  "Register a voice instance in the global registry.

   Arguments:
     voice - Implementation of IVoice protocol

   Returns:
     The voice instance."
  [voice]
  {:pre [(satisfies? IVoice voice)]}
  (let [id (voice-id voice)]
    (swap! voice-registry assoc id voice)
    voice))

(defn get-voice
  "Get voice instance by ID from the registry.

   Arguments:
     id - Voice identifier (keyword or string)

   Returns:
     Voice instance or nil if not found."
  [id]
  (get @voice-registry id))

(defn list-voices
  "List all registered voice instances.

   Returns:
     Vector of voice IDs."
  []
  (vec (keys @voice-registry)))

(defn voice-registered?
  "Check if a voice instance is registered.

   Arguments:
     id - Voice identifier

   Returns:
     Boolean."
  [id]
  (contains? @voice-registry id))

(defn unregister-voice!
  "Unregister a voice instance.

   Arguments:
     id - Voice identifier

   Returns:
     true if voice was removed, false if not found."
  [id]
  (if (voice-registered? id)
    (do (swap! voice-registry dissoc id) true)
    false))

;;; ============================================================================
;;; IRouter Protocol (Message Routing)
;;; ============================================================================

(defprotocol IRouter
  "Protocol for routing messages to the appropriate channel.

   Provides a dispatch layer that sits above individual channels,
   enabling message routing based on topic patterns, channel capabilities,
   or custom routing logic.

   Architecture:
   ```
   Sender -> IRouter -> route-message -> IChannel (selected)
                  |
            Channel Registry (internal)
   ```

   Implementations:
   - TopicRouter: Route by topic prefix mapping
   - RoundRobinRouter: Load-balance across channels
   - PriorityRouter: Route by message priority
   - NoopRouter: Testing fallback

   SOLID-S: Single responsibility - message routing only.
   SOLID-O: Open for extension via new routing strategies.
   CLARITY-Y: Yield safe failure - graceful degradation on routing errors."

  (router-id [this]
    "Return unique identifier for this router instance.

     Returns:
       Keyword or string identifier.
       Example: :topic-router, :round-robin-1")

  (router-info [this]
    "Return metadata about this router.

     Returns map with:
       :id           - Router identifier (same as router-id)
       :name         - Human-readable name
       :description  - Router purpose description
       :strategy     - Routing strategy (:topic, :round-robin, :priority, :custom)
       :channel-count - Number of registered channels")

  (route-message [this topic payload opts]
    "Route a message to the appropriate channel(s) based on routing rules.

     Arguments:
       topic   - Message topic for routing decision
       payload - Message content
       opts    - Optional map with:
                 :sender    - Sender identifier
                 :priority  - Message priority (:low, :normal, :high)
                 :headers   - Additional message headers
                 :broadcast? - Send to all matching channels (default: false)

     Returns map with:
       :success?     - Boolean indicating routing success
       :routed-to    - Vector of channel IDs that received the message
       :message-id   - Generated message ID
       :errors       - Vector of error messages (empty on success)

     CLARITY-Y: Must not throw - return :success? false on failure.")

  (register-route! [this channel-id channel opts]
    "Register a channel with this router.

     Arguments:
       channel-id - Keyword identifier for the channel
       channel    - IChannel implementation
       opts       - Optional map with:
                    :topics  - Set of topic patterns this channel handles
                    :priority - Channel priority for tie-breaking
                    :filter   - Predicate fn for custom routing

     Returns map with:
       :success? - Boolean indicating registration success
       :errors   - Vector of error messages")

  (unregister-route! [this channel-id]
    "Remove a channel from the router.

     Arguments:
       channel-id - ID of channel to remove

     Returns map with:
       :success? - Boolean indicating removal success
       :errors   - Vector of error messages

     Idempotent - safe to call on already-removed channels.")

  (router-channels [this]
    "List all channels registered with this router.

     Returns:
       Vector of maps with:
       :channel-id - Channel identifier
       :topics     - Set of topic patterns
       :priority   - Channel priority
       :open?      - Whether channel is currently open")

  (resolve-route [this topic]
    "Resolve which channel(s) a topic would be routed to without sending.

     Arguments:
       topic - Topic to resolve

     Returns:
       Vector of channel IDs that would receive the message.
       Empty vector if no matching channels."))

;;; ============================================================================
;;; NoopRouter (No-op Fallback)
;;; ============================================================================

(defrecord NoopRouter [id state]
  IRouter
  (router-id [_] id)

  (router-info [_]
    {:id id
     :name "Noop Router"
     :description "No-operation router for testing and development"
     :strategy :noop
     :channel-count (count (:channels @state))})

  (route-message [_ _topic _payload _opts]
    {:success? true
     :routed-to []
     :message-id (generate-message-id)
     :errors []})

  (register-route! [_ channel-id channel opts]
    (swap! state assoc-in [:channels channel-id]
           {:channel channel
            :topics (or (:topics opts) #{})
            :priority (or (:priority opts) 0)})
    {:success? true
     :errors []})

  (unregister-route! [_ channel-id]
    (swap! state update :channels dissoc channel-id)
    {:success? true :errors []})

  (router-channels [_]
    (->> (:channels @state)
         (mapv (fn [[ch-id {:keys [topics priority channel]}]]
                 {:channel-id ch-id
                  :topics topics
                  :priority priority
                  :open? (when (satisfies? IChannel channel)
                           (open? channel))}))))

  (resolve-route [_ _topic]
    []))

(defn ->noop-router
  "Create a NoopRouter for testing.

   Arguments:
     id - Keyword identifier for this router (default: :noop-router)"
  ([] (->noop-router :noop-router))
  ([id] (->NoopRouter id (atom {:channels {}}))))

;;; ============================================================================
;;; Router Registry
;;; ============================================================================

(defonce ^:private router-registry (atom {}))

(defn register-router!
  "Register a router instance in the global registry.

   Arguments:
     router - Implementation of IRouter protocol

   Returns:
     The router instance."
  [router]
  {:pre [(satisfies? IRouter router)]}
  (let [id (router-id router)]
    (swap! router-registry assoc id router)
    router))

(defn get-router
  "Get router instance by ID from the registry.

   Arguments:
     id - Router identifier (keyword or string)

   Returns:
     Router instance or nil if not found."
  [id]
  (get @router-registry id))

(defn list-routers
  "List all registered routers with their info.

   Returns:
     Vector of router info maps."
  []
  (->> @router-registry
       vals
       (mapv router-info)))

(defn router-registered?
  "Check if a router is registered.

   Arguments:
     id - Router identifier

   Returns:
     Boolean."
  [id]
  (contains? @router-registry id))

(defn unregister-router!
  "Unregister a router.

   Arguments:
     id - Router identifier

   Returns:
     true if router was removed, false if not found."
  [id]
  (if (router-registered? id)
    (do (swap! router-registry dissoc id) true)
    false))
