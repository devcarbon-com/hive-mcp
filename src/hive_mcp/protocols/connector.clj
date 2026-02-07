(ns hive-mcp.protocols.connector
  "Protocols for external system connectors.

   Defines abstractions for integrating hive-mcp with external systems:
   - APIs, IoT devices, apps, and other data sources
   - Bidirectional data transformation (external <-> hive internal)
   - Authentication and authorization

   Architecture (from axiom: hive-mcp as Shared Intelligence Layer):
   ```
   AIs (learn & act) <-> hive-mcp <-> Apps (retrieve & display)
                             ^
                            IoT (sense & contribute)
   ```

   CLARITY-L: Layers stay pure - protocols are the boundary between
   hive-mcp domain logic and external system implementations.
   SOLID-O: Open for extension via new protocol implementations.
   SOLID-D: Depend on abstractions (these protocols), not concretions."
  (:refer-clojure :exclude [send]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Connector Protocol (Connection Lifecycle)
;; =============================================================================

(defprotocol IConnector
  "Protocol for managing connections to external systems.

   Handles the connection lifecycle for integrations:
   - APIs (REST, GraphQL, gRPC)
   - Message brokers (MQTT, Kafka, RabbitMQ)
   - Databases (external, not hive-internal)
   - IoT gateways and device hubs
   - Webhook endpoints (outbound)

   Relationship to other protocols:
   - IConnector:   Connection lifecycle (connect, disconnect, health)
   - IDataMapper:  Data transformation (inbound/outbound conversion)
   - ITransport:   Low-level message passing (send/recv bytes)

   IConnector is the highest abstraction - it may compose ITransport
   for communication and IDataMapper for data conversion.

   CLARITY-Y: Yield safe failure - graceful degradation on connection loss.
   CLARITY-I: Introspection first - expose connection status for debugging.
   SOLID-S: Single responsibility - connection lifecycle only."

  (connector-id [this]
    "Return unique keyword identifier for this connector.
     Used for registry lookup, logging, and health checks.
     Example: :github-api, :mqtt-home, :stripe-webhook")

  (connector-info [this]
    "Return metadata about this connector.
     Returns map with:
       :id          - Keyword identifier (same as connector-id)
       :name        - Human-readable name
       :description - What this connector integrates with
       :version     - Connector version string
       :system-type - External system type (:api, :broker, :database, :iot, :webhook)
       :capabilities - Set of supported operations (:read, :write, :subscribe, :webhook)")

  (connect! [this opts]
    "Establish connection to the external system.

     Arguments:
       opts - Connection options (connector-specific):
              :credentials  - Auth credentials (API key, OAuth token, etc.)
              :endpoint     - Base URL or connection string
              :timeout-ms   - Connection timeout (default: 30000)
              :retry-policy - {:max-retries 3, :backoff-ms 1000}

     Returns map with:
       :success?    - Boolean indicating connection success
       :connection  - Opaque connection handle (for disconnect)
       :errors      - Vector of error messages (empty on success)
       :metadata    - Connection metadata (server version, capabilities, etc.)

     CLARITY-Y: Must not throw - return :success? false on failure.")

  (disconnect! [this]
    "Close connection to the external system.
     Releases resources, closes sockets, cleans up state.

     Returns map with:
       :success?  - Boolean indicating clean disconnect
       :errors    - Vector of error messages (empty on success)

     Idempotent - safe to call multiple times.")

  (connected? [this]
    "Check if connector is currently connected.
     Lightweight check, does not verify connection health.

     Returns boolean.")

  (health-check [this]
    "Perform active health check on the connection.
     May send ping/heartbeat to verify connection is alive.

     Returns map with:
       :healthy?    - Boolean indicating connection health
       :latency-ms  - Round-trip latency (nil if unhealthy)
       :errors      - Vector of health check failures
       :checked-at  - Timestamp of health check

     CLARITY-T: Should log health check results for observability.")

  (reconnect! [this opts]
    "Attempt to re-establish a lost connection.
     Combines disconnect! + connect! with optional backoff.

     Arguments:
       opts - Same as connect!, plus:
              :force? - Disconnect even if connected (default: false)

     Returns same as connect!.")

  (get-status [this]
    "Get comprehensive connector status for introspection.

     Returns map with:
       :id              - Connector ID
       :connected?      - Current connection state
       :last-connected  - Timestamp of last successful connect
       :last-error      - Most recent error (nil if none)
       :error-count     - Total errors since last connect
       :uptime-ms       - Time since last connect (nil if disconnected)
       :metrics         - Connector-specific metrics map")

  (send [this data opts]
    "Send data to the external system.

     Arguments:
       data - Data payload to send (format depends on connector)
       opts - Send options:
              :format     - Wire format (:json, :edn, :binary)
              :timeout-ms - Send timeout (default: 30000)
              :async?     - Non-blocking send (default: false)
              :metadata   - Additional metadata for the message

     Returns map with:
       :success?  - Boolean indicating send success
       :response  - Response from external system (nil if async)
       :errors    - Vector of error messages (empty on success)
       :sent-at   - Timestamp when data was sent

     CLARITY-Y: Must not throw - return :success? false on failure.")

  (receive [this opts]
    "Receive/pull data from the external system.

     Arguments:
       opts - Receive options:
              :timeout-ms - Max wait time (default: 30000)
              :limit      - Max items to receive (default: 100)
              :since      - Timestamp to receive from (nil for all available)
              :filter     - Predicate to filter received items

     Returns map with:
       :success?  - Boolean indicating receive success
       :data      - Vector of received items
       :count     - Number of items received
       :errors    - Vector of error messages (empty on success)
       :has-more? - Whether more data is available")

  (sync! [this opts]
    "Perform bidirectional synchronization with external system.

     Combines send + receive with conflict resolution.

     Arguments:
       opts - Sync options:
              :direction  - :inbound, :outbound, or :bidirectional (default)
              :since      - Sync changes since this timestamp
              :conflict   - Conflict resolution (:local-wins, :remote-wins, :newest)
              :dry-run?   - Preview changes without applying (default: false)

     Returns map with:
       :success?    - Boolean indicating sync success
       :sent        - Number of items sent to external
       :received    - Number of items received from external
       :conflicts   - Vector of conflict descriptions
       :resolved    - Number of conflicts auto-resolved
       :errors      - Vector of error messages
       :synced-at   - Timestamp of sync completion"))

;; =============================================================================
;; Connector Registry
;; =============================================================================

(defonce ^:private connector-registry (atom {}))

(defn register-connector!
  "Register an external system connector.

   Arguments:
     connector - Implementation of IConnector protocol

   Returns the connector.
   Does NOT auto-connect - call connect! separately."
  [connector]
  {:pre [(satisfies? IConnector connector)]}
  (let [id (connector-id connector)]
    (swap! connector-registry assoc id connector)
    connector))

(defn get-connector
  "Get connector by ID.

   Arguments:
     id - Connector keyword (:github-api, :mqtt-home, etc.)

   Returns connector or nil if not found."
  [id]
  (get @connector-registry id))

(defn list-connectors
  "List all registered connectors with their status.

   Returns vector of maps with connector info + current status."
  []
  (->> @connector-registry
       vals
       (mapv (fn [c]
               (merge (connector-info c)
                      {:status (get-status c)})))))

(defn connector-registered?
  "Check if a connector is registered."
  [id]
  (contains? @connector-registry id))

(defn unregister-connector!
  "Unregister a connector. Disconnects first if connected.

   Arguments:
     id - Connector keyword

   Returns true if connector was removed, false if not found."
  [id]
  (if-let [connector (get-connector id)]
    (do
      (when (connected? connector)
        (disconnect! connector))
      (swap! connector-registry dissoc id)
      true)
    false))

;; =============================================================================
;; Connector Lifecycle Helpers
;; =============================================================================

(defn connect-all!
  "Connect all registered connectors.

   Arguments:
     opts-map - Map of connector-id to connection opts
                {:github-api {:credentials {...}}
                 :mqtt-home {:endpoint \"tcp://...\"}}

   Returns map of connector-id to connect! results."
  [opts-map]
  (->> @connector-registry
       (map (fn [[id connector]]
              [id (connect! connector (get opts-map id {}))]))
       (into {})))

(defn disconnect-all!
  "Disconnect all registered connectors.

   Returns map of connector-id to disconnect! results."
  []
  (->> @connector-registry
       (map (fn [[id connector]]
              [id (disconnect! connector)]))
       (into {})))

(defn health-check-all
  "Health check all connected connectors.

   Returns map of connector-id to health-check results.
   Only checks connectors where connected? is true."
  []
  (->> @connector-registry
       (filter (fn [[_id connector]] (connected? connector)))
       (map (fn [[id connector]]
              [id (health-check connector)]))
       (into {})))

;; =============================================================================
;; Data Mapper Protocol
;; =============================================================================

(defprotocol IDataMapper
  "Protocol for bidirectional data transformation between external systems
   and hive-mcp's internal format.

   Unlike IAdapter (migration-focused, file-based), IDataMapper handles
   real-time data flow from live external systems:
   - IoT sensors producing readings
   - APIs returning JSON/XML responses
   - Apps sending user input
   - Webhooks delivering events

   Implementations should be stateless and pure where possible.
   Side effects (logging, metrics) should be minimal.

   Example implementations:
   - JsonApiMapper: REST API JSON <-> Clojure maps
   - MqttMapper: IoT MQTT messages <-> memory entries
   - WebhookMapper: Webhook payloads <-> hive events
   - GraphQLMapper: GraphQL responses <-> query results"

  (mapper-id [this]
    "Return unique keyword identifier for this mapper.
     Used for registry lookup and logging.
     Example: :json-api, :mqtt-sensor, :webhook-github")

  (mapper-info [this]
    "Return metadata about this mapper.
     Returns map with:
       :id          - Keyword identifier (same as mapper-id)
       :name        - Human-readable name
       :description - What this mapper does
       :version     - Semantic version string
       :source-type - External data type (:json, :xml, :binary, :text)
       :target-type - Internal data type (:memory-entry, :event, :entity)")

  (inbound [this external-data opts]
    "Transform external system data to hive-mcp internal format.

     Arguments:
       external-data - Raw data from external system (format varies)
       opts          - Mapper-specific options:
                       :context   - Request context (auth, headers, etc.)
                       :schema    - Optional schema for validation
                       :strict?   - Fail on unknown fields (default false)

     Returns map with:
       :success? - Boolean indicating transformation success
       :data     - Transformed data (nil on failure)
       :errors   - Vector of error messages (empty on success)
       :warnings - Vector of non-fatal warnings
       :metadata - Extraction metadata (timestamps, source info)")

  (outbound [this internal-data opts]
    "Transform hive-mcp internal data to external system format.

     Arguments:
       internal-data - Hive-mcp data structure (map, entity, etc.)
       opts          - Mapper-specific options:
                       :format    - Output format variant (:compact, :pretty)
                       :include   - Fields to include (whitelist)
                       :exclude   - Fields to exclude (blacklist)
                       :envelope? - Wrap in response envelope (default true)

     Returns map with:
       :success? - Boolean indicating transformation success
       :data     - Transformed data ready for external system
       :errors   - Vector of error messages (empty on success)
       :content-type - MIME type for the output (e.g., \"application/json\")")

  (validate-inbound [this external-data]
    "Validate external data before transformation.
     Lightweight check without full transformation.

     Arguments:
       external-data - Raw data from external system

     Returns map with:
       :valid?  - Boolean indicating validity
       :errors  - Vector of validation errors (empty if valid)
       :hints   - Suggestions for fixing invalid data")

  (supported-formats [this]
    "Return set of supported external formats.
     Used for content negotiation and capability discovery.
     Example: #{:json :json-ld :msgpack}"))

;; =============================================================================
;; Mapper Registry
;; =============================================================================

(defonce ^:private mapper-registry (atom {}))

(defn register-mapper!
  "Register a data mapper implementation.

   Arguments:
     mapper - Implementation of IDataMapper protocol

   Returns the mapper.
   Logs registration for observability."
  [mapper]
  {:pre [(satisfies? IDataMapper mapper)]}
  (let [id (mapper-id mapper)]
    (swap! mapper-registry assoc id mapper)
    mapper))

(defn get-mapper
  "Get mapper by ID.

   Arguments:
     id - Mapper keyword (:json-api, :mqtt-sensor, etc.)

   Returns mapper or nil if not found."
  [id]
  (get @mapper-registry id))

(defn list-mappers
  "List all registered mappers.

   Returns vector of mapper info maps."
  []
  (->> @mapper-registry
       vals
       (mapv mapper-info)))

(defn mapper-registered?
  "Check if a mapper is registered."
  [id]
  (contains? @mapper-registry id))

(defn unregister-mapper!
  "Unregister a data mapper.

   Arguments:
     id - Mapper keyword (:json-api, :mqtt-sensor, etc.)

   Returns true if mapper was removed, false if not found."
  [id]
  (if (mapper-registered? id)
    (do (swap! mapper-registry dissoc id) true)
    false))

;; =============================================================================
;; Transformation Helpers
;; =============================================================================

(defn transform-inbound
  "Apply mapper transformation for inbound data.

   Arguments:
     mapper-id - Keyword identifying the mapper
     data      - External data to transform
     opts      - Mapper-specific options (optional)

   Returns transformation result map or throws if mapper not found."
  [mapper-id data & [opts]]
  (if-let [mapper (get-mapper mapper-id)]
    (inbound mapper data (or opts {}))
    (throw (ex-info "Unknown mapper"
                    {:mapper mapper-id
                     :available (keys @mapper-registry)}))))

(defn transform-outbound
  "Apply mapper transformation for outbound data.

   Arguments:
     mapper-id - Keyword identifying the mapper
     data      - Internal data to transform
     opts      - Mapper-specific options (optional)

   Returns transformation result map or throws if mapper not found."
  [mapper-id data & [opts]]
  (if-let [mapper (get-mapper mapper-id)]
    (outbound mapper data (or opts {}))
    (throw (ex-info "Unknown mapper"
                    {:mapper mapper-id
                     :available (keys @mapper-registry)}))))

;; =============================================================================
;; Base Implementation (Identity Mapper)
;; =============================================================================

(defrecord IdentityMapper []
  IDataMapper
  (mapper-id [_] :identity)

  (mapper-info [_]
    {:id :identity
     :name "Identity Mapper"
     :description "Passthrough mapper, no transformation"
     :version "1.0.0"
     :source-type :clojure
     :target-type :clojure})

  (inbound [_ data _opts]
    {:success? true
     :data data
     :errors []
     :warnings []
     :metadata {:mapper :identity
                :transformed-at (java.time.Instant/now)}})

  (outbound [_ data _opts]
    {:success? true
     :data data
     :errors []
     :content-type "application/edn"})

  (validate-inbound [_ _data]
    {:valid? true
     :errors []
     :hints []})

  (supported-formats [_]
    #{:edn :clojure}))

;; Register identity mapper as default
(register-mapper! (->IdentityMapper))

;; =============================================================================
;; Base Implementation (Noop Connector)
;; =============================================================================

(defrecord NoopConnector [id state]
  IConnector
  (connector-id [_] id)

  (connector-info [_]
    {:id id
     :name "Noop Connector"
     :description "No-operation connector for testing and development"
     :version "1.0.0"
     :system-type :mock
     :capabilities #{:read :write}})

  (connect! [this _opts]
    (swap! state assoc :connected? true :connect-time (java.time.Instant/now))
    {:success? true
     :connection this
     :errors []
     :metadata {:mock true}})

  (disconnect! [_]
    (swap! state assoc :connected? false)
    {:success? true
     :errors []})

  (connected? [_]
    (boolean (:connected? @state)))

  (health-check [_]
    (let [connected? (:connected? @state)]
      {:healthy? (boolean connected?)
       :latency-ms (when connected? 0)
       :errors (if connected? [] ["Not connected"])
       :checked-at (java.time.Instant/now)}))

  (reconnect! [this opts]
    (disconnect! this)
    (connect! this opts))

  (get-status [_]
    (let [{:keys [connected? connect-time]} @state]
      {:id id
       :connected? (boolean connected?)
       :last-connected connect-time
       :last-error nil
       :error-count 0
       :uptime-ms (when (and connected? connect-time)
                    (- (System/currentTimeMillis)
                       (.toEpochMilli ^java.time.Instant connect-time)))
       :metrics {}}))

  (send [_ data _opts]
    (swap! state update :sent-items (fnil conj []) data)
    {:success? true
     :response nil
     :errors []
     :sent-at (java.time.Instant/now)})

  (receive [_ _opts]
    (let [items (:received-items @state [])]
      {:success? true
       :data items
       :count (count items)
       :errors []
       :has-more? false}))

  (sync! [_ _opts]
    {:success? true
     :sent 0
     :received 0
     :conflicts []
     :resolved 0
     :errors []
     :synced-at (java.time.Instant/now)}))

(defn ->noop-connector
  "Create a NoopConnector for testing.

   Arguments:
     id - Keyword identifier for this connector (default: :noop)"
  ([] (->noop-connector :noop))
  ([id] (->NoopConnector id (atom {:connected? false :connect-time nil}))))

;; =============================================================================
;; Auth Provider Protocol
;; =============================================================================

(defprotocol IAuthProvider
  "Protocol for authentication with external systems.

   Provides token-based authentication lifecycle:
   - Obtain initial credentials (OAuth2, API key exchange, etc.)
   - Refresh expired tokens
   - Validate token validity before use

   Relationship to IConnector:
   - IAuthProvider manages credentials/tokens
   - IConnector uses those tokens to establish connections
   - Separation allows credential rotation without reconnection

   CLARITY-Y: Yield safe failure - return error maps, never throw.
   SOLID-S: Single responsibility - authentication only.
   SOLID-I: Interface segregation - separate from connection lifecycle."

  (authenticate [this credentials]
    "Obtain authentication token/session from credentials.

     Arguments:
       credentials - Authentication input (provider-specific):
                     :username/:password - Basic auth
                     :client-id/:client-secret - OAuth2 client credentials
                     :api-key - API key exchange
                     :refresh-token - Token refresh (see also refresh-token method)

     Returns map with:
       :success?      - Boolean indicating auth success
       :token         - Auth token string (nil on failure)
       :token-type    - Token type (:bearer, :api-key, :session, :basic)
       :expires-at    - Token expiry timestamp (nil if non-expiring)
       :refresh-token - Refresh token for renewal (nil if not applicable)
       :scopes        - Set of granted scopes/permissions
       :errors        - Vector of error messages (empty on success)

     CLARITY-Y: Must not throw - return :success? false on failure.")

  (refresh-token [this token]
    "Renew an expired or expiring token.

     Arguments:
       token - Current token map (as returned by authenticate):
               :token         - The token to refresh
               :refresh-token - Refresh token (if applicable)
               :token-type    - Type of token

     Returns map with same shape as authenticate.
     May return the same token if still valid.

     Idempotent - safe to call multiple times.")

  (valid? [this token]
    "Check if a token is still valid.

     Arguments:
       token - Token map to validate:
               :token      - The token string
               :expires-at - Expiry timestamp (nil = non-expiring)

     Returns map with:
       :valid?     - Boolean indicating token validity
       :expires-in - Milliseconds until expiry (nil if non-expiring)
       :errors     - Vector of validation errors (empty if valid)

     Lightweight check - may be local-only (check expiry timestamp)
     or may call the auth server for verification."))

;; =============================================================================
;; Auth Provider Registry
;; =============================================================================

(defonce ^:private auth-provider-registry (atom {}))

(defn register-auth-provider!
  "Register an authentication provider.

   Arguments:
     id       - Keyword identifier for this provider (:oauth2, :api-key, etc.)
     provider - Implementation of IAuthProvider protocol

   Returns the provider."
  [id provider]
  {:pre [(keyword? id)
         (satisfies? IAuthProvider provider)]}
  (swap! auth-provider-registry assoc id provider)
  provider)

(defn get-auth-provider
  "Get auth provider by ID.

   Arguments:
     id - Provider keyword (:oauth2, :api-key, etc.)

   Returns provider or nil if not found."
  [id]
  (get @auth-provider-registry id))

(defn list-auth-providers
  "List all registered auth provider IDs.

   Returns vector of keyword IDs."
  []
  (vec (keys @auth-provider-registry)))

(defn auth-provider-registered?
  "Check if an auth provider is registered."
  [id]
  (contains? @auth-provider-registry id))

(defn unregister-auth-provider!
  "Unregister an auth provider.

   Arguments:
     id - Provider keyword

   Returns true if provider was removed, false if not found."
  [id]
  (if (auth-provider-registered? id)
    (do (swap! auth-provider-registry dissoc id) true)
    false))

;; =============================================================================
;; Base Implementation (Noop Auth Provider)
;; =============================================================================

(defrecord NoopAuthProvider []
  IAuthProvider
  (authenticate [_ _credentials]
    {:success? true
     :token "noop-token-000"
     :token-type :bearer
     :expires-at nil
     :refresh-token nil
     :scopes #{}
     :errors []})

  (refresh-token [_ _token]
    {:success? true
     :token "noop-token-refreshed"
     :token-type :bearer
     :expires-at nil
     :refresh-token nil
     :scopes #{}
     :errors []})

  (valid? [_ _token]
    {:valid? true
     :expires-in nil
     :errors []}))

(defn ->noop-auth-provider
  "Create a NoopAuthProvider for testing.
   Always succeeds, returns mock tokens."
  []
  (->NoopAuthProvider))
