(ns hive-mcp.protocols.connector-test
  "Tests for IConnector, IAuthProvider, and IDataMapper protocols.

   Validates:
   - IConnector protocol method signatures and return shapes (all 11 methods)
   - IAuthProvider protocol method signatures and return shapes (all 3 methods)
   - IDataMapper protocol method signatures and return shapes (all 6 methods)
   - NoopConnector fallback behavior
   - NoopAuthProvider fallback behavior
   - IdentityMapper passthrough behavior
   - Connector registry CRUD
   - Auth provider registry CRUD
   - Mapper registry CRUD (register!, get, list, unregister!)
   - Transformation helpers (transform-inbound, transform-outbound)
   - Connector lifecycle helpers (connect-all!, disconnect-all!, health-check-all)
   - Custom implementations via reify"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.connector :as conn]))

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

(defn clean-registries
  "Reset all registries between tests to ensure isolation."
  [f]
  ;; Clear connector registry (private atom, use unregister)
  (doseq [{:keys [id]} (conn/list-connectors)]
    (conn/unregister-connector! id))
  ;; Clear auth provider registry
  (doseq [id (conn/list-auth-providers)]
    (conn/unregister-auth-provider! id))
  ;; Clear mapper registry
  (doseq [{:keys [id]} (conn/list-mappers)]
    (conn/unregister-mapper! id))
  (f)
  (doseq [{:keys [id]} (conn/list-connectors)]
    (conn/unregister-connector! id))
  (doseq [id (conn/list-auth-providers)]
    (conn/unregister-auth-provider! id))
  (doseq [{:keys [id]} (conn/list-mappers)]
    (conn/unregister-mapper! id)))

(use-fixtures :each clean-registries)

;;; ============================================================================
;;; NoopConnector - Original Methods
;;; ============================================================================

(deftest noop-connector-id-test
  (testing "connector-id returns the ID passed at construction"
    (let [c (conn/->noop-connector :test-conn)]
      (is (= :test-conn (conn/connector-id c)))))

  (testing "default connector-id is :noop"
    (let [c (conn/->noop-connector)]
      (is (= :noop (conn/connector-id c))))))

(deftest noop-connector-info-test
  (testing "connector-info returns expected metadata map"
    (let [c (conn/->noop-connector :test-conn)
          info (conn/connector-info c)]
      (is (map? info))
      (is (= :test-conn (:id info)))
      (is (= "Noop Connector" (:name info)))
      (is (string? (:description info)))
      (is (= "1.0.0" (:version info)))
      (is (= :mock (:system-type info)))
      (is (set? (:capabilities info)))
      (is (contains? (:capabilities info) :read))
      (is (contains? (:capabilities info) :write)))))

(deftest noop-connector-connect-disconnect-test
  (testing "connect! transitions to connected state"
    (let [c (conn/->noop-connector :test-conn)
          result (conn/connect! c {})]
      (is (true? (:success? result)))
      (is (some? (:connection result)))
      (is (= [] (:errors result)))
      (is (map? (:metadata result)))
      (is (true? (conn/connected? c)))))

  (testing "disconnect! transitions to disconnected state"
    (let [c (conn/->noop-connector :test-conn)]
      (conn/connect! c {})
      (let [result (conn/disconnect! c)]
        (is (true? (:success? result)))
        (is (= [] (:errors result)))
        (is (false? (conn/connected? c))))))

  (testing "disconnect! is idempotent"
    (let [c (conn/->noop-connector :test-conn)]
      (let [r1 (conn/disconnect! c)
            r2 (conn/disconnect! c)]
        (is (true? (:success? r1)))
        (is (true? (:success? r2)))))))

(deftest noop-connector-connected?-test
  (testing "connected? false before connect"
    (let [c (conn/->noop-connector)]
      (is (false? (conn/connected? c)))))

  (testing "connected? true after connect"
    (let [c (conn/->noop-connector)]
      (conn/connect! c {})
      (is (true? (conn/connected? c)))))

  (testing "connected? false after disconnect"
    (let [c (conn/->noop-connector)]
      (conn/connect! c {})
      (conn/disconnect! c)
      (is (false? (conn/connected? c))))))

(deftest noop-connector-health-check-test
  (testing "health-check when connected"
    (let [c (conn/->noop-connector)]
      (conn/connect! c {})
      (let [result (conn/health-check c)]
        (is (true? (:healthy? result)))
        (is (= 0 (:latency-ms result)))
        (is (= [] (:errors result)))
        (is (instance? java.time.Instant (:checked-at result))))))

  (testing "health-check when disconnected"
    (let [c (conn/->noop-connector)]
      (let [result (conn/health-check c)]
        (is (false? (:healthy? result)))
        (is (nil? (:latency-ms result)))
        (is (= ["Not connected"] (:errors result)))))))

(deftest noop-connector-reconnect-test
  (testing "reconnect! disconnects then reconnects"
    (let [c (conn/->noop-connector)]
      (conn/connect! c {})
      (is (true? (conn/connected? c)))
      (let [result (conn/reconnect! c {})]
        (is (true? (:success? result)))
        (is (true? (conn/connected? c)))))))

(deftest noop-connector-get-status-test
  (testing "get-status when disconnected"
    (let [c (conn/->noop-connector :test-conn)
          status (conn/get-status c)]
      (is (= :test-conn (:id status)))
      (is (false? (:connected? status)))
      (is (nil? (:last-connected status)))
      (is (nil? (:last-error status)))
      (is (= 0 (:error-count status)))
      (is (nil? (:uptime-ms status)))
      (is (= {} (:metrics status)))))

  (testing "get-status when connected"
    (let [c (conn/->noop-connector :test-conn)]
      (conn/connect! c {})
      (let [status (conn/get-status c)]
        (is (true? (:connected? status)))
        (is (instance? java.time.Instant (:last-connected status)))
        (is (number? (:uptime-ms status)))))))

;;; ============================================================================
;;; NoopConnector - New Methods (send, receive, sync!)
;;; ============================================================================

(deftest noop-connector-send-test
  (testing "send returns success with timestamp"
    (let [c (conn/->noop-connector :test-conn)
          result (conn/send c {:message "hello"} {})]
      (is (true? (:success? result)))
      (is (nil? (:response result)))
      (is (= [] (:errors result)))
      (is (instance? java.time.Instant (:sent-at result)))))

  (testing "send accumulates items in state"
    (let [c (conn/->noop-connector :test-conn)]
      (conn/send c {:msg "first"} {})
      (conn/send c {:msg "second"} {})
      (conn/send c {:msg "third"} {})
      ;; Access internal state to verify accumulation
      (let [sent (:sent-items @(:state c))]
        (is (= 3 (count sent)))
        (is (= {:msg "first"} (first sent)))
        (is (= {:msg "third"} (last sent)))))))

(deftest noop-connector-receive-test
  (testing "receive returns empty data by default"
    (let [c (conn/->noop-connector :test-conn)
          result (conn/receive c {})]
      (is (true? (:success? result)))
      (is (= [] (:data result)))
      (is (= 0 (:count result)))
      (is (= [] (:errors result)))
      (is (false? (:has-more? result)))))

  (testing "receive returns pre-loaded items from state"
    (let [c (conn/->noop-connector :test-conn)]
      ;; Simulate external data available for receive
      (swap! (:state c) assoc :received-items [{:id 1} {:id 2}])
      (let [result (conn/receive c {})]
        (is (true? (:success? result)))
        (is (= [{:id 1} {:id 2}] (:data result)))
        (is (= 2 (:count result)))))))

(deftest noop-connector-sync-test
  (testing "sync! returns success with zero counts"
    (let [c (conn/->noop-connector :test-conn)
          result (conn/sync! c {})]
      (is (true? (:success? result)))
      (is (= 0 (:sent result)))
      (is (= 0 (:received result)))
      (is (= [] (:conflicts result)))
      (is (= 0 (:resolved result)))
      (is (= [] (:errors result)))
      (is (instance? java.time.Instant (:synced-at result)))))

  (testing "sync! accepts options without error"
    (let [c (conn/->noop-connector :test-conn)
          result (conn/sync! c {:direction :bidirectional
                                :conflict :local-wins
                                :dry-run? true})]
      (is (true? (:success? result))))))

;;; ============================================================================
;;; Connector Registry Tests
;;; ============================================================================

(deftest connector-registry-register-test
  (testing "register-connector! stores and returns connector"
    (let [c (conn/->noop-connector :reg-test)
          result (conn/register-connector! c)]
      (is (= c result))
      (is (true? (conn/connector-registered? :reg-test)))))

  (testing "register-connector! rejects non-IConnector"
    (is (thrown? AssertionError
                 (conn/register-connector! {:not "a connector"})))))

(deftest connector-registry-get-test
  (testing "get-connector returns registered connector"
    (let [c (conn/->noop-connector :get-test)]
      (conn/register-connector! c)
      (is (= c (conn/get-connector :get-test)))))

  (testing "get-connector returns nil for unknown"
    (is (nil? (conn/get-connector :nonexistent)))))

(deftest connector-registry-list-test
  (testing "list-connectors returns all with status"
    (conn/register-connector! (conn/->noop-connector :list-a))
    (conn/register-connector! (conn/->noop-connector :list-b))
    (let [connectors (conn/list-connectors)]
      (is (= 2 (count connectors)))
      (is (every? :id connectors))
      (is (every? :status connectors)))))

(deftest connector-registry-unregister-test
  (testing "unregister-connector! removes and disconnects"
    (let [c (conn/->noop-connector :unreg-test)]
      (conn/register-connector! c)
      (conn/connect! c {})
      (is (true? (conn/connected? c)))
      (is (true? (conn/unregister-connector! :unreg-test)))
      (is (false? (conn/connector-registered? :unreg-test)))
      (is (false? (conn/connected? c)))))

  (testing "unregister-connector! returns false for unknown"
    (is (false? (conn/unregister-connector! :nonexistent)))))

;;; ============================================================================
;;; Connector Lifecycle Helpers
;;; ============================================================================

(deftest connect-all-test
  (testing "connect-all! connects all registered connectors"
    (let [c1 (conn/->noop-connector :ca-1)
          c2 (conn/->noop-connector :ca-2)]
      (conn/register-connector! c1)
      (conn/register-connector! c2)
      (let [results (conn/connect-all! {:ca-1 {} :ca-2 {}})]
        (is (true? (get-in results [:ca-1 :success?])))
        (is (true? (get-in results [:ca-2 :success?])))
        (is (true? (conn/connected? c1)))
        (is (true? (conn/connected? c2)))))))

(deftest disconnect-all-test
  (testing "disconnect-all! disconnects all registered connectors"
    (let [c1 (conn/->noop-connector :da-1)
          c2 (conn/->noop-connector :da-2)]
      (conn/register-connector! c1)
      (conn/register-connector! c2)
      (conn/connect! c1 {})
      (conn/connect! c2 {})
      (let [results (conn/disconnect-all!)]
        (is (true? (get-in results [:da-1 :success?])))
        (is (true? (get-in results [:da-2 :success?])))
        (is (false? (conn/connected? c1)))
        (is (false? (conn/connected? c2)))))))

(deftest health-check-all-test
  (testing "health-check-all only checks connected connectors"
    (let [c1 (conn/->noop-connector :hc-1)
          c2 (conn/->noop-connector :hc-2)]
      (conn/register-connector! c1)
      (conn/register-connector! c2)
      (conn/connect! c1 {})
      ;; c2 stays disconnected
      (let [results (conn/health-check-all)]
        (is (= 1 (count results)))
        (is (true? (get-in results [:hc-1 :healthy?])))
        (is (nil? (get results :hc-2)))))))

;;; ============================================================================
;;; NoopAuthProvider Tests
;;; ============================================================================

(deftest noop-auth-provider-authenticate-test
  (testing "authenticate returns success with mock token"
    (let [provider (conn/->noop-auth-provider)
          result (conn/authenticate provider {:username "test" :password "pass"})]
      (is (true? (:success? result)))
      (is (= "noop-token-000" (:token result)))
      (is (= :bearer (:token-type result)))
      (is (nil? (:expires-at result)))
      (is (nil? (:refresh-token result)))
      (is (set? (:scopes result)))
      (is (= [] (:errors result)))))

  (testing "authenticate succeeds with any credentials"
    (let [provider (conn/->noop-auth-provider)]
      (is (true? (:success? (conn/authenticate provider {}))))
      (is (true? (:success? (conn/authenticate provider {:api-key "xyz"}))))
      (is (true? (:success? (conn/authenticate provider nil)))))))

(deftest noop-auth-provider-refresh-token-test
  (testing "refresh-token returns refreshed mock token"
    (let [provider (conn/->noop-auth-provider)
          result (conn/refresh-token provider {:token "old-token" :token-type :bearer})]
      (is (true? (:success? result)))
      (is (= "noop-token-refreshed" (:token result)))
      (is (= :bearer (:token-type result)))
      (is (= [] (:errors result)))))

  (testing "refresh-token is idempotent"
    (let [provider (conn/->noop-auth-provider)
          r1 (conn/refresh-token provider {:token "t1"})
          r2 (conn/refresh-token provider {:token "t1"})]
      (is (= (:token r1) (:token r2))))))

(deftest noop-auth-provider-valid?-test
  (testing "valid? always returns true for noop"
    (let [provider (conn/->noop-auth-provider)
          result (conn/valid? provider {:token "any-token"})]
      (is (true? (:valid? result)))
      (is (nil? (:expires-in result)))
      (is (= [] (:errors result)))))

  (testing "valid? works with nil token"
    (let [provider (conn/->noop-auth-provider)
          result (conn/valid? provider nil)]
      (is (true? (:valid? result))))))

;;; ============================================================================
;;; Auth Provider Registry Tests
;;; ============================================================================

(deftest auth-provider-registry-register-test
  (testing "register-auth-provider! stores provider"
    (let [provider (conn/->noop-auth-provider)]
      (conn/register-auth-provider! :test-auth provider)
      (is (true? (conn/auth-provider-registered? :test-auth)))))

  (testing "register-auth-provider! rejects non-IAuthProvider"
    (is (thrown? AssertionError
                 (conn/register-auth-provider! :bad {:not "a provider"}))))

  (testing "register-auth-provider! rejects non-keyword id"
    (is (thrown? AssertionError
                 (conn/register-auth-provider! "string-id" (conn/->noop-auth-provider))))))

(deftest auth-provider-registry-get-test
  (testing "get-auth-provider returns registered provider"
    (let [provider (conn/->noop-auth-provider)]
      (conn/register-auth-provider! :get-auth provider)
      (is (= provider (conn/get-auth-provider :get-auth)))))

  (testing "get-auth-provider returns nil for unknown"
    (is (nil? (conn/get-auth-provider :nonexistent)))))

(deftest auth-provider-registry-list-test
  (testing "list-auth-providers returns all IDs"
    (conn/register-auth-provider! :auth-a (conn/->noop-auth-provider))
    (conn/register-auth-provider! :auth-b (conn/->noop-auth-provider))
    (let [ids (conn/list-auth-providers)]
      (is (= 2 (count ids)))
      (is (some #{:auth-a} ids))
      (is (some #{:auth-b} ids)))))

(deftest auth-provider-registry-unregister-test
  (testing "unregister-auth-provider! removes provider"
    (conn/register-auth-provider! :unreg-auth (conn/->noop-auth-provider))
    (is (true? (conn/unregister-auth-provider! :unreg-auth)))
    (is (false? (conn/auth-provider-registered? :unreg-auth))))

  (testing "unregister-auth-provider! returns false for unknown"
    (is (false? (conn/unregister-auth-provider! :nonexistent)))))

;;; ============================================================================
;;; Protocol Satisfaction Tests
;;; ============================================================================

(deftest protocol-satisfaction-test
  (testing "NoopConnector satisfies IConnector"
    (is (satisfies? conn/IConnector (conn/->noop-connector))))

  (testing "NoopAuthProvider satisfies IAuthProvider"
    (is (satisfies? conn/IAuthProvider (conn/->noop-auth-provider))))

  (testing "IdentityMapper satisfies IDataMapper"
    (is (satisfies? conn/IDataMapper (conn/->IdentityMapper)))))

;;; ============================================================================
;;; Custom IConnector Implementation via reify
;;; ============================================================================

(deftest custom-connector-via-reify-test
  (testing "custom IConnector implementation works"
    (let [state (atom {:connected? false :data-store []})
          custom (reify conn/IConnector
                   (connector-id [_] :custom-test)
                   (connector-info [_]
                     {:id :custom-test :name "Custom" :version "2.0.0"
                      :system-type :api :capabilities #{:read :write :subscribe}})
                   (connect! [_ _opts]
                     (swap! state assoc :connected? true)
                     {:success? true :connection state :errors [] :metadata {}})
                   (disconnect! [_]
                     (swap! state assoc :connected? false)
                     {:success? true :errors []})
                   (connected? [_] (:connected? @state))
                   (health-check [_]
                     {:healthy? (:connected? @state) :latency-ms 42
                      :errors [] :checked-at (java.time.Instant/now)})
                   (reconnect! [this opts]
                     (conn/disconnect! this) (conn/connect! this opts))
                   (get-status [_]
                     {:id :custom-test :connected? (:connected? @state)
                      :metrics {:custom-metric 99}})
                   (send [_ data _opts]
                     (swap! state update :data-store conj data)
                     {:success? true :response {:ack true}
                      :errors [] :sent-at (java.time.Instant/now)})
                   (receive [_ _opts]
                     {:success? true :data (:data-store @state)
                      :count (count (:data-store @state))
                      :errors [] :has-more? false})
                   (sync! [_ _opts]
                     {:success? true :sent 5 :received 3
                      :conflicts [] :resolved 0 :errors []
                      :synced-at (java.time.Instant/now)}))]

      ;; Test connect
      (conn/connect! custom {})
      (is (true? (conn/connected? custom)))

      ;; Test health-check with custom latency
      (is (= 42 (:latency-ms (conn/health-check custom))))

      ;; Test send with response
      (let [result (conn/send custom {:event "click"} {})]
        (is (true? (:success? result)))
        (is (= {:ack true} (:response result))))

      ;; Test receive sees sent data
      (let [result (conn/receive custom {})]
        (is (= 1 (:count result)))
        (is (= [{:event "click"}] (:data result))))

      ;; Test sync with custom counts
      (let [result (conn/sync! custom {:direction :bidirectional})]
        (is (= 5 (:sent result)))
        (is (= 3 (:received result))))

      ;; Test get-status with custom metrics
      (let [status (conn/get-status custom)]
        (is (= 99 (get-in status [:metrics :custom-metric])))))))

;;; ============================================================================
;;; Custom IAuthProvider Implementation via reify
;;; ============================================================================

(deftest custom-auth-provider-via-reify-test
  (testing "custom IAuthProvider implementation works"
    (let [token-counter (atom 0)
          custom (reify conn/IAuthProvider
                   (authenticate [_ creds]
                     (let [n (swap! token-counter inc)]
                       (if (:api-key creds)
                         {:success? true
                          :token (str "custom-token-" n)
                          :token-type :api-key
                          :expires-at (.plusSeconds (java.time.Instant/now) 3600)
                          :refresh-token nil
                          :scopes #{:read :write}
                          :errors []}
                         {:success? false
                          :token nil
                          :token-type nil
                          :expires-at nil
                          :refresh-token nil
                          :scopes #{}
                          :errors ["Missing api-key"]})))
                   (refresh-token [_ token]
                     (let [n (swap! token-counter inc)]
                       {:success? true
                        :token (str "refreshed-" n)
                        :token-type (:token-type token)
                        :expires-at (.plusSeconds (java.time.Instant/now) 3600)
                        :refresh-token nil
                        :scopes #{:read :write}
                        :errors []}))
                   (valid? [_ token]
                     (if (:expires-at token)
                       (let [now (java.time.Instant/now)
                             expires (:expires-at token)]
                         {:valid? (.isAfter expires now)
                          :expires-in (- (.toEpochMilli expires) (.toEpochMilli now))
                          :errors []})
                       {:valid? false
                        :expires-in nil
                        :errors ["No expiry set"]})))]

      ;; Authenticate with valid credentials
      (let [result (conn/authenticate custom {:api-key "my-key"})]
        (is (true? (:success? result)))
        (is (= :api-key (:token-type result)))
        (is (contains? (:scopes result) :read))
        (is (instance? java.time.Instant (:expires-at result))))

      ;; Authenticate with missing credentials
      (let [result (conn/authenticate custom {})]
        (is (false? (:success? result)))
        (is (nil? (:token result)))
        (is (= ["Missing api-key"] (:errors result))))

      ;; Refresh token
      (let [result (conn/refresh-token custom {:token "old" :token-type :api-key})]
        (is (true? (:success? result)))
        (is (.startsWith (:token result) "refreshed-")))

      ;; Valid? with future expiry
      (let [future-token {:token "t" :expires-at (.plusSeconds (java.time.Instant/now) 3600)}
            result (conn/valid? custom future-token)]
        (is (true? (:valid? result)))
        (is (pos? (:expires-in result))))

      ;; Valid? with no expiry
      (let [result (conn/valid? custom {:token "t"})]
        (is (false? (:valid? result)))
        (is (= ["No expiry set"] (:errors result)))))))

;;; ============================================================================
;;; Integration: Connector + Auth Provider
;;; ============================================================================

(deftest connector-auth-integration-test
  (testing "auth provider token can be used for connector connect"
    (let [provider (conn/->noop-auth-provider)
          connector (conn/->noop-connector :auth-integrated)]
      ;; Authenticate first
      (let [auth-result (conn/authenticate provider {:api-key "test"})]
        (is (true? (:success? auth-result)))
        ;; Use token to connect
        (let [connect-result (conn/connect! connector
                                            {:credentials {:token (:token auth-result)
                                                           :token-type (:token-type auth-result)}})]
          (is (true? (:success? connect-result)))
          (is (true? (conn/connected? connector)))
          ;; Validate token is still valid
          (let [valid-result (conn/valid? provider {:token (:token auth-result)})]
            (is (true? (:valid? valid-result))))
          ;; Send data through connector
          (let [send-result (conn/send connector {:payload "test-data"} {})]
            (is (true? (:success? send-result)))))))))

;;; ============================================================================
;;; Full Lifecycle Test
;;; ============================================================================

(deftest full-connector-lifecycle-test
  (testing "complete connector lifecycle: register -> connect -> send -> receive -> sync -> disconnect -> unregister"
    (let [c (conn/->noop-connector :lifecycle-test)]
      ;; 1. Register
      (conn/register-connector! c)
      (is (true? (conn/connector-registered? :lifecycle-test)))

      ;; 2. Connect
      (let [result (conn/connect! c {})]
        (is (true? (:success? result))))
      (is (true? (conn/connected? c)))

      ;; 3. Health check
      (is (true? (:healthy? (conn/health-check c))))

      ;; 4. Send data
      (let [result (conn/send c {:event "user-login"} {:format :json})]
        (is (true? (:success? result))))

      ;; 5. Receive data
      (let [result (conn/receive c {:limit 10})]
        (is (true? (:success? result))))

      ;; 6. Sync
      (let [result (conn/sync! c {:direction :bidirectional})]
        (is (true? (:success? result))))

      ;; 7. Status check
      (let [status (conn/get-status c)]
        (is (true? (:connected? status)))
        (is (some? (:uptime-ms status))))

      ;; 8. Disconnect
      (conn/disconnect! c)
      (is (false? (conn/connected? c)))

      ;; 9. Unregister
      (is (true? (conn/unregister-connector! :lifecycle-test)))
      (is (false? (conn/connector-registered? :lifecycle-test))))))

;;; ============================================================================
;;; IDataMapper - IdentityMapper Tests
;;; ============================================================================

(deftest identity-mapper-id-test
  (testing "mapper-id returns :identity"
    (let [m (conn/->IdentityMapper)]
      (is (= :identity (conn/mapper-id m))))))

(deftest identity-mapper-info-test
  (testing "mapper-info returns expected metadata map"
    (let [m (conn/->IdentityMapper)
          info (conn/mapper-info m)]
      (is (map? info))
      (is (= :identity (:id info)))
      (is (= "Identity Mapper" (:name info)))
      (is (string? (:description info)))
      (is (= "1.0.0" (:version info)))
      (is (= :clojure (:source-type info)))
      (is (= :clojure (:target-type info))))))

(deftest identity-mapper-inbound-test
  (testing "inbound passes data through unchanged"
    (let [m (conn/->IdentityMapper)
          data {:key "value" :nested {:a 1}}
          result (conn/inbound m data {})]
      (is (true? (:success? result)))
      (is (= data (:data result)))
      (is (= [] (:errors result)))
      (is (= [] (:warnings result)))
      (is (map? (:metadata result)))
      (is (= :identity (:mapper (:metadata result))))
      (is (instance? java.time.Instant (:transformed-at (:metadata result))))))

  (testing "inbound handles nil data"
    (let [m (conn/->IdentityMapper)
          result (conn/inbound m nil {})]
      (is (true? (:success? result)))
      (is (nil? (:data result)))))

  (testing "inbound handles vector data"
    (let [m (conn/->IdentityMapper)
          data [1 2 3 "four"]
          result (conn/inbound m data {})]
      (is (true? (:success? result)))
      (is (= data (:data result)))))

  (testing "inbound handles string data"
    (let [m (conn/->IdentityMapper)
          result (conn/inbound m "raw-string" {})]
      (is (true? (:success? result)))
      (is (= "raw-string" (:data result)))))

  (testing "inbound ignores opts (identity passes through)"
    (let [m (conn/->IdentityMapper)
          data {:x 1}
          result (conn/inbound m data {:strict? true :schema :my-schema})]
      (is (true? (:success? result)))
      (is (= data (:data result))))))

(deftest identity-mapper-outbound-test
  (testing "outbound passes data through unchanged"
    (let [m (conn/->IdentityMapper)
          data {:internal "data" :tags [:a :b]}
          result (conn/outbound m data {})]
      (is (true? (:success? result)))
      (is (= data (:data result)))
      (is (= [] (:errors result)))
      (is (= "application/edn" (:content-type result)))))

  (testing "outbound handles nil data"
    (let [m (conn/->IdentityMapper)
          result (conn/outbound m nil {})]
      (is (true? (:success? result)))
      (is (nil? (:data result)))))

  (testing "outbound handles complex nested data"
    (let [m (conn/->IdentityMapper)
          data {:users [{:name "Alice" :roles #{:admin}}
                        {:name "Bob" :roles #{:user}}]}
          result (conn/outbound m data {:format :compact})]
      (is (true? (:success? result)))
      (is (= data (:data result))))))

(deftest identity-mapper-validate-inbound-test
  (testing "validate-inbound always returns valid for identity mapper"
    (let [m (conn/->IdentityMapper)
          result (conn/validate-inbound m {:any "data"})]
      (is (true? (:valid? result)))
      (is (= [] (:errors result)))
      (is (= [] (:hints result)))))

  (testing "validate-inbound valid even for nil"
    (let [m (conn/->IdentityMapper)
          result (conn/validate-inbound m nil)]
      (is (true? (:valid? result)))))

  (testing "validate-inbound valid for empty map"
    (let [m (conn/->IdentityMapper)
          result (conn/validate-inbound m {})]
      (is (true? (:valid? result))))))

(deftest identity-mapper-supported-formats-test
  (testing "supported-formats returns edn and clojure"
    (let [m (conn/->IdentityMapper)
          formats (conn/supported-formats m)]
      (is (set? formats))
      (is (contains? formats :edn))
      (is (contains? formats :clojure))
      (is (= 2 (count formats))))))

;;; ============================================================================
;;; Mapper Registry Tests
;;; ============================================================================

(deftest mapper-registry-register-test
  (testing "register-mapper! stores and returns mapper"
    (let [m (conn/->IdentityMapper)
          result (conn/register-mapper! m)]
      (is (= m result))
      (is (true? (conn/mapper-registered? :identity)))))

  (testing "register-mapper! rejects non-IDataMapper"
    (is (thrown? AssertionError
                 (conn/register-mapper! {:not "a mapper"})))))

(deftest mapper-registry-get-test
  (testing "get-mapper returns registered mapper"
    (let [m (conn/->IdentityMapper)]
      (conn/register-mapper! m)
      (is (= m (conn/get-mapper :identity)))))

  (testing "get-mapper returns nil for unknown"
    (is (nil? (conn/get-mapper :nonexistent)))))

(deftest mapper-registry-list-test
  (testing "list-mappers returns all mapper info maps"
    (conn/register-mapper! (conn/->IdentityMapper))
    (let [mappers (conn/list-mappers)]
      (is (= 1 (count mappers)))
      (is (= :identity (:id (first mappers))))
      (is (string? (:name (first mappers)))))))

(deftest mapper-registry-unregister-test
  (testing "unregister-mapper! removes mapper"
    (conn/register-mapper! (conn/->IdentityMapper))
    (is (true? (conn/mapper-registered? :identity)))
    (is (true? (conn/unregister-mapper! :identity)))
    (is (false? (conn/mapper-registered? :identity)))
    (is (nil? (conn/get-mapper :identity))))

  (testing "unregister-mapper! returns false for unknown"
    (is (false? (conn/unregister-mapper! :nonexistent))))

  (testing "unregister-mapper! is idempotent"
    (conn/register-mapper! (conn/->IdentityMapper))
    (is (true? (conn/unregister-mapper! :identity)))
    (is (false? (conn/unregister-mapper! :identity)))))

(deftest mapper-registry-multiple-test
  (testing "registry supports multiple mappers with different IDs"
    (let [m1 (reify conn/IDataMapper
               (mapper-id [_] :json-api)
               (mapper-info [_] {:id :json-api :name "JSON API" :version "1.0.0"
                                 :source-type :json :target-type :memory-entry})
               (inbound [_ data _] {:success? true :data data :errors [] :warnings [] :metadata {}})
               (outbound [_ data _] {:success? true :data data :errors [] :content-type "application/json"})
               (validate-inbound [_ _] {:valid? true :errors [] :hints []})
               (supported-formats [_] #{:json}))
          m2 (reify conn/IDataMapper
               (mapper-id [_] :xml-feed)
               (mapper-info [_] {:id :xml-feed :name "XML Feed" :version "1.0.0"
                                 :source-type :xml :target-type :event})
               (inbound [_ data _] {:success? true :data data :errors [] :warnings [] :metadata {}})
               (outbound [_ data _] {:success? true :data data :errors [] :content-type "application/xml"})
               (validate-inbound [_ _] {:valid? true :errors [] :hints []})
               (supported-formats [_] #{:xml}))]
      (conn/register-mapper! m1)
      (conn/register-mapper! m2)
      (is (= 2 (count (conn/list-mappers))))
      (is (true? (conn/mapper-registered? :json-api)))
      (is (true? (conn/mapper-registered? :xml-feed)))
      ;; Unregister one, other still present
      (conn/unregister-mapper! :json-api)
      (is (false? (conn/mapper-registered? :json-api)))
      (is (true? (conn/mapper-registered? :xml-feed)))
      (is (= 1 (count (conn/list-mappers)))))))

;;; ============================================================================
;;; Transformation Helper Tests
;;; ============================================================================

(deftest transform-inbound-test
  (testing "transform-inbound delegates to registered mapper"
    (conn/register-mapper! (conn/->IdentityMapper))
    (let [result (conn/transform-inbound :identity {:msg "hello"})]
      (is (true? (:success? result)))
      (is (= {:msg "hello"} (:data result)))))

  (testing "transform-inbound passes opts through"
    (conn/register-mapper! (conn/->IdentityMapper))
    (let [result (conn/transform-inbound :identity {:x 1} {:strict? true})]
      (is (true? (:success? result)))))

  (testing "transform-inbound works without opts"
    (conn/register-mapper! (conn/->IdentityMapper))
    (let [result (conn/transform-inbound :identity {:x 1})]
      (is (true? (:success? result)))))

  (testing "transform-inbound throws for unknown mapper"
    (is (thrown? clojure.lang.ExceptionInfo
                 (conn/transform-inbound :nonexistent {:data "test"})))))

(deftest transform-outbound-test
  (testing "transform-outbound delegates to registered mapper"
    (conn/register-mapper! (conn/->IdentityMapper))
    (let [result (conn/transform-outbound :identity {:internal "data"})]
      (is (true? (:success? result)))
      (is (= {:internal "data"} (:data result)))
      (is (= "application/edn" (:content-type result)))))

  (testing "transform-outbound passes opts through"
    (conn/register-mapper! (conn/->IdentityMapper))
    (let [result (conn/transform-outbound :identity {:x 1} {:format :compact})]
      (is (true? (:success? result)))))

  (testing "transform-outbound works without opts"
    (conn/register-mapper! (conn/->IdentityMapper))
    (let [result (conn/transform-outbound :identity {:x 1})]
      (is (true? (:success? result)))))

  (testing "transform-outbound throws for unknown mapper"
    (is (thrown? clojure.lang.ExceptionInfo
                 (conn/transform-outbound :nonexistent {:data "test"})))))

(deftest transform-error-info-test
  (testing "transform-inbound error includes available mappers"
    (conn/register-mapper! (conn/->IdentityMapper))
    (try
      (conn/transform-inbound :nonexistent {:data "test"})
      (is false "Should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (is (= :nonexistent (:mapper data)))
          (is (some #{:identity} (:available data))))))))

;;; ============================================================================
;;; Custom IDataMapper Implementation via reify
;;; ============================================================================

(deftest custom-mapper-via-reify-test
  (testing "custom IDataMapper with JSON-like transformation"
    (let [custom (reify conn/IDataMapper
                   (mapper-id [_] :json-custom)
                   (mapper-info [_]
                     {:id :json-custom
                      :name "Custom JSON Mapper"
                      :description "Transforms JSON-like maps to hive memory entries"
                      :version "2.0.0"
                      :source-type :json
                      :target-type :memory-entry})
                   (inbound [_ external-data opts]
                     (if (map? external-data)
                       {:success? true
                        :data {:type :note
                               :content (str (:message external-data))
                               :tags (vec (:tags external-data))
                               :source (:source-type opts)}
                        :errors []
                        :warnings (if (:deprecated external-data)
                                    ["Field 'deprecated' is being phased out"]
                                    [])
                        :metadata {:mapper :json-custom
                                   :fields-mapped (count external-data)
                                   :transformed-at (java.time.Instant/now)}}
                       {:success? false
                        :data nil
                        :errors ["Expected map, got " (type external-data)]
                        :warnings []
                        :metadata {}}))
                   (outbound [_ internal-data opts]
                     (if (map? internal-data)
                       {:success? true
                        :data {"message" (:content internal-data)
                               "tags" (or (:tags internal-data) [])
                               "type" (name (or (:type internal-data) :unknown))}
                        :errors []
                        :content-type (if (= :pretty (:format opts))
                                        "application/json; indent=2"
                                        "application/json")}
                       {:success? false
                        :data nil
                        :errors ["Cannot convert non-map to JSON"]
                        :content-type nil}))
                   (validate-inbound [_ data]
                     (cond
                       (not (map? data))
                       {:valid? false
                        :errors ["Input must be a map"]
                        :hints ["Wrap raw values in a map with :message key"]}

                       (not (contains? data :message))
                       {:valid? false
                        :errors ["Missing required field: :message"]
                        :hints ["Add :message key to the input map"]}

                       :else
                       {:valid? true :errors [] :hints []}))
                   (supported-formats [_]
                     #{:json :json-ld}))]

      ;; Protocol satisfaction
      (is (satisfies? conn/IDataMapper custom))

      ;; mapper-id
      (is (= :json-custom (conn/mapper-id custom)))

      ;; mapper-info
      (let [info (conn/mapper-info custom)]
        (is (= :json-custom (:id info)))
        (is (= "2.0.0" (:version info)))
        (is (= :json (:source-type info)))
        (is (= :memory-entry (:target-type info))))

      ;; inbound - success
      (let [result (conn/inbound custom {:message "Hello" :tags [:test :dev]} {:source-type :api})]
        (is (true? (:success? result)))
        (is (= :note (:type (:data result))))
        (is (= "Hello" (:content (:data result))))
        (is (= [:test :dev] (:tags (:data result))))
        (is (= :api (:source (:data result))))
        (is (= [] (:errors result)))
        (is (= [] (:warnings result)))
        (is (= 2 (:fields-mapped (:metadata result)))))

      ;; inbound - with deprecated field triggers warning
      (let [result (conn/inbound custom {:message "Old" :deprecated true} {})]
        (is (true? (:success? result)))
        (is (= 1 (count (:warnings result))))
        (is (.contains (first (:warnings result)) "deprecated")))

      ;; inbound - failure (non-map input)
      (let [result (conn/inbound custom "not a map" {})]
        (is (false? (:success? result)))
        (is (nil? (:data result)))
        (is (pos? (count (:errors result)))))

      ;; outbound - success
      (let [result (conn/outbound custom {:content "World" :tags [:prod] :type :note} {})]
        (is (true? (:success? result)))
        (is (= "World" (get (:data result) "message")))
        (is (= [:prod] (get (:data result) "tags")))
        (is (= "note" (get (:data result) "type")))
        (is (= "application/json" (:content-type result))))

      ;; outbound - pretty format
      (let [result (conn/outbound custom {:content "X"} {:format :pretty})]
        (is (= "application/json; indent=2" (:content-type result))))

      ;; outbound - failure (non-map)
      (let [result (conn/outbound custom "not a map" {})]
        (is (false? (:success? result)))
        (is (nil? (:content-type result))))

      ;; validate-inbound - valid
      (let [result (conn/validate-inbound custom {:message "ok"})]
        (is (true? (:valid? result)))
        (is (= [] (:errors result))))

      ;; validate-inbound - missing required field
      (let [result (conn/validate-inbound custom {:other "data"})]
        (is (false? (:valid? result)))
        (is (= 1 (count (:errors result))))
        (is (pos? (count (:hints result)))))

      ;; validate-inbound - non-map
      (let [result (conn/validate-inbound custom "string")]
        (is (false? (:valid? result)))
        (is (.contains (first (:errors result)) "map")))

      ;; supported-formats
      (let [formats (conn/supported-formats custom)]
        (is (set? formats))
        (is (contains? formats :json))
        (is (contains? formats :json-ld)))

      ;; Registry integration
      (conn/register-mapper! custom)
      (is (true? (conn/mapper-registered? :json-custom)))
      (is (= custom (conn/get-mapper :json-custom)))

      ;; transform-inbound via helper
      (let [result (conn/transform-inbound :json-custom {:message "via helper"} {})]
        (is (true? (:success? result)))
        (is (= "via helper" (:content (:data result)))))

      ;; transform-outbound via helper
      (let [result (conn/transform-outbound :json-custom {:content "out"} {})]
        (is (true? (:success? result)))
        (is (= "out" (get (:data result) "message")))))))

;;; ============================================================================
;;; IDataMapper + IConnector Integration
;;; ============================================================================

(deftest mapper-connector-integration-test
  (testing "mapper transforms data before connector send and after receive"
    (let [mapper (conn/->IdentityMapper)
          connector (conn/->noop-connector :mapper-integration)]
      (conn/register-mapper! mapper)
      (conn/register-connector! connector)
      (conn/connect! connector {})

      ;; Transform then send
      (let [external-data {:sensor "temp" :value 22.5}
            transform-result (conn/transform-inbound :identity external-data)
            _ (is (true? (:success? transform-result)))
            send-result (conn/send connector (:data transform-result) {})]
        (is (true? (:success? send-result))))

      ;; Receive then transform for external
      (let [internal-data {:type :note :content "sensor reading"}
            transform-result (conn/transform-outbound :identity internal-data)]
        (is (true? (:success? transform-result)))
        (is (= internal-data (:data transform-result)))
        (is (= "application/edn" (:content-type transform-result)))))))

;;; ============================================================================
;;; Full IDataMapper Lifecycle Test
;;; ============================================================================

(deftest full-mapper-lifecycle-test
  (testing "complete mapper lifecycle: register -> validate -> inbound -> outbound -> unregister"
    (let [m (conn/->IdentityMapper)]
      ;; 1. Register
      (conn/register-mapper! m)
      (is (true? (conn/mapper-registered? :identity)))
      (is (= 1 (count (conn/list-mappers))))

      ;; 2. Validate inbound data
      (let [result (conn/validate-inbound m {:data "test"})]
        (is (true? (:valid? result))))

      ;; 3. Transform inbound
      (let [result (conn/inbound m {:external "data"} {})]
        (is (true? (:success? result)))
        (is (= {:external "data"} (:data result))))

      ;; 4. Transform outbound
      (let [result (conn/outbound m {:internal "data"} {})]
        (is (true? (:success? result)))
        (is (= {:internal "data"} (:data result))))

      ;; 5. Check supported formats
      (is (contains? (conn/supported-formats m) :edn))

      ;; 6. Unregister
      (is (true? (conn/unregister-mapper! :identity)))
      (is (false? (conn/mapper-registered? :identity)))
      (is (= 0 (count (conn/list-mappers)))))))
