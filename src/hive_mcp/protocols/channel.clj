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
