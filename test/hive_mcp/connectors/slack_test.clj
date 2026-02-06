(ns hive-mcp.connectors.slack-test
  "Tests for SlackConnector with mock HTTP responses.

   Tests cover:
   - IConnector lifecycle (connect!/disconnect!/connected?/health-check)
   - IConnector send/receive/sync! protocol methods
   - Web API operations (send-message, list-channels, read-history)
   - Error handling (invalid token, missing params, HTTP failures)
   - Bot-token vs webhook-only modes"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.connectors.slack :as slack]
            [hive-mcp.protocols.connector :as connector]))

;; =============================================================================
;; Mock HTTP Infrastructure
;; =============================================================================

(def ^:dynamic *mock-responses*
  "Dynamic var holding a map of endpoint -> response for mock HTTP.
   Keys are endpoint strings like \"auth.test\", \"chat.postMessage\", etc."
  {})

(defn mock-slack-api-request
  "Mock implementation of slack-api-request that returns canned responses.
   Looks up the endpoint in *mock-responses*."
  [_method endpoint _bot-token _opts]
  (if-let [response (get *mock-responses* endpoint)]
    (if (fn? response)
      (response _method endpoint _bot-token _opts)
      response)
    {:ok false :error (str "No mock configured for endpoint: " endpoint)}))

(defmacro with-mock-slack
  "Execute body with mock Slack API responses.

   Usage:
     (with-mock-slack {\"auth.test\" {:ok true :user_id \"U123\"}}
       (do-something))"
  [responses & body]
  `(binding [*mock-responses* ~responses]
     (with-redefs [slack/slack-api-request mock-slack-api-request]
       ~@body)))

;; Standard mock responses for reuse
(def mock-auth-ok
  {:ok true
   :user_id "U1234ABCD"
   :team "HivemindTeam"
   :team_id "T1234ABCD"
   :bot_id "B1234ABCD"
   :url "https://hivemindteam.slack.com/"})

(def mock-auth-fail
  {:ok false
   :error "invalid_auth"})

(def mock-channels-response
  {:ok true
   :channels [{:id "C001" :name "general"
               :topic {:value "General discussion"}
               :purpose {:value "Company-wide channel"}
               :num_members 42
               :is_archived false}
              {:id "C002" :name "hivemind"
               :topic {:value "Hive events"}
               :purpose {:value "Automated hivemind notifications"}
               :num_members 5
               :is_archived false}
              {:id "C003" :name "old-project"
               :topic {:value ""}
               :purpose {:value "Archived project"}
               :num_members 3
               :is_archived true}]
   :response_metadata {:next_cursor ""}})

(def mock-channels-paginated
  {:ok true
   :channels [{:id "C001" :name "general"
               :topic {:value ""} :purpose {:value ""}
               :num_members 10 :is_archived false}]
   :response_metadata {:next_cursor "dXNlcjpVMEc5V0ZYTlo="}})

(def mock-history-response
  {:ok true
   :messages [{:type "message" :user "U1234" :text "Hello world"
               :ts "1234567890.123456" :thread_ts nil :reply_count nil}
              {:type "message" :user "U5678" :text "Reply here"
               :ts "1234567891.123456" :thread_ts "1234567890.123456" :reply_count 3}]
   :has_more false
   :response_metadata {:next_cursor ""}})

(def mock-send-ok
  {:ok true
   :ts "1234567892.123456"
   :channel "C001"
   :message {:text "Test message"}})

(def mock-send-channel-not-found
  {:ok false
   :error "channel_not_found"})

;; =============================================================================
;; Connector Lifecycle Tests
;; =============================================================================

(deftest connect-with-bot-token
  (testing "Connect with valid bot token succeeds"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :test-slack)
            result (connector/connect! conn {:bot-token "xoxb-test-token"})]
        (is (:success? result) "Should connect successfully")
        (is (empty? (:errors result)) "No errors expected")
        (is (= conn (:connection result)) "Connection should be the connector itself")
        (is (connector/connected? conn) "Should be connected after connect!")
        (is (= "HivemindTeam" (get-in (:metadata result) [:team]))
            "Should include team from auth.test")
        (connector/disconnect! conn)))))

(deftest connect-with-invalid-token
  (testing "Connect with invalid bot token fails gracefully"
    (with-mock-slack {"auth.test" mock-auth-fail}
      (let [conn (slack/->slack-connector :test-slack)
            result (connector/connect! conn {:bot-token "xoxb-invalid"})]
        (is (not (:success? result)) "Should fail")
        (is (seq (:errors result)) "Should have error messages")
        (is (not (connector/connected? conn)) "Should not be connected")))))

(deftest connect-with-no-credentials
  (testing "Connect without bot-token or webhook-url fails"
    (let [conn (slack/->slack-connector :test-slack)
          result (connector/connect! conn {})]
      (is (not (:success? result)))
      (is (some #(re-find #"At least one" %) (:errors result))))))

(deftest connect-already-connected
  (testing "Connect when already connected returns error"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [result (connector/connect! conn {:bot-token "xoxb-test"})]
          (is (not (:success? result)))
          (is (some #(re-find #"Already connected" %) (:errors result))))
        (connector/disconnect! conn)))))

(deftest disconnect-lifecycle
  (testing "Disconnect cleans up state"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (is (connector/connected? conn))
        (let [result (connector/disconnect! conn)]
          (is (:success? result))
          (is (empty? (:errors result)))
          (is (not (connector/connected? conn)) "Should be disconnected"))))))

(deftest disconnect-when-not-connected
  (testing "Disconnect when not connected is idempotent"
    (let [conn (slack/->slack-connector :test-slack)
          result (connector/disconnect! conn)]
      (is (:success? result) "Should succeed even when not connected")
      (is (empty? (:errors result))))))

(deftest connected-predicate
  (testing "connected? reflects actual state"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (is (not (connector/connected? conn)) "Not connected initially")
        (connector/connect! conn {:bot-token "xoxb-test"})
        (is (connector/connected? conn) "Connected after connect!")
        (connector/disconnect! conn)
        (is (not (connector/connected? conn)) "Not connected after disconnect!")))))

;; =============================================================================
;; Health Check Tests
;; =============================================================================

(deftest health-check-with-bot-token
  (testing "Health check uses auth.test when bot-token available"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [health (connector/health-check conn)]
          (is (:healthy? health) "Should be healthy")
          (is (number? (:latency-ms health)) "Should have latency measurement")
          (is (empty? (:errors health)) "No errors")
          (is (some? (:checked-at health)) "Should have timestamp"))
        (connector/disconnect! conn)))))

(deftest health-check-when-disconnected
  (testing "Health check returns unhealthy when not connected"
    (let [conn (slack/->slack-connector :test-slack)
          health (connector/health-check conn)]
      (is (not (:healthy? health)))
      (is (nil? (:latency-ms health)))
      (is (seq (:errors health))))))

(deftest health-check-with-failed-auth
  (testing "Health check reports unhealthy on auth failure"
    (let [call-count (atom 0)]
      (with-mock-slack {"auth.test" (fn [& _]
                                      (swap! call-count inc)
                                      (if (= 1 @call-count)
                                        mock-auth-ok
                                        mock-auth-fail))}
        (let [conn (slack/->slack-connector :test-slack)]
          (connector/connect! conn {:bot-token "xoxb-test"})
          (let [health (connector/health-check conn)]
            (is (not (:healthy? health)) "Should be unhealthy when auth fails")
            (is (seq (:errors health))))
          (connector/disconnect! conn))))))

;; =============================================================================
;; Connector Info & Status Tests
;; =============================================================================

(deftest connector-info-metadata
  (testing "connector-info returns correct metadata"
    (let [conn (slack/->slack-connector :my-slack)
          info (connector/connector-info conn)]
      (is (= :my-slack (:id info)))
      (is (= "Slack Connector" (:name info)))
      (is (= :api (:system-type info)))
      (is (contains? (:capabilities info) :read))
      (is (contains? (:capabilities info) :write))
      (is (string? (:version info))))))

(deftest connector-id-returns-keyword
  (testing "connector-id returns the id keyword"
    (let [conn (slack/->slack-connector :test-id)]
      (is (= :test-id (connector/connector-id conn))))))

(deftest get-status-when-disconnected
  (testing "get-status returns correct state when disconnected"
    (let [conn (slack/->slack-connector :test-slack)
          status (connector/get-status conn)]
      (is (= :test-slack (:id status)))
      (is (not (:connected? status)))
      (is (nil? (:last-connected status)))
      (is (= 0 (:error-count status))))))

(deftest get-status-when-connected
  (testing "get-status returns metrics when connected"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [status (connector/get-status conn)]
          (is (:connected? status))
          (is (some? (:last-connected status)))
          (is (number? (:uptime-ms status)))
          (is (= 0 (get-in status [:metrics :message-count])))
          (is (true? (get-in status [:metrics :has-bot-token?])))
          (is (= "HivemindTeam" (get-in status [:metrics :team]))))
        (connector/disconnect! conn)))))

;; =============================================================================
;; Reconnect Tests
;; =============================================================================

(deftest reconnect-disconnects-then-connects
  (testing "reconnect! performs disconnect then connect"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (is (connector/connected? conn))
        (let [result (connector/reconnect! conn {:bot-token "xoxb-test-2"})]
          (is (:success? result) "Reconnect should succeed")
          (is (connector/connected? conn)))
        (connector/disconnect! conn)))))

;; =============================================================================
;; Send Message Tests
;; =============================================================================

(deftest send-message-success
  (testing "send-message posts to chat.postMessage"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "chat.postMessage" mock-send-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [result (slack/send-message conn {:channel "#general"
                                               :text "Hello world"})]
          (is (:ok result) "Should succeed")
          (is (= "1234567892.123456" (:ts result)) "Should return message ts")
          (is (= "C001" (:channel result))))
        (connector/disconnect! conn)))))

(deftest send-message-with-default-channel
  (testing "send-message uses default channel from connect! opts"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "chat.postMessage" mock-send-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test" :channel "#hivemind"})
        (let [result (slack/send-message conn {:text "Auto-channel"})]
          (is (:ok result)))
        (connector/disconnect! conn)))))

(deftest send-message-channel-not-found
  (testing "send-message returns error for invalid channel"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "chat.postMessage" mock-send-channel-not-found}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [result (slack/send-message conn {:channel "#nonexistent"
                                               :text "Hello"})]
          (is (not (:ok result)))
          (is (= "channel_not_found" (:error result))))
        (connector/disconnect! conn)))))

(deftest send-message-without-bot-token
  (testing "send-message fails without bot-token"
    (let [conn (slack/->slack-connector :test-slack)
          _ (reset! (:state conn) {:connected? true :webhook-url "https://hook.example.com"})
          result (slack/send-message conn {:channel "#test" :text "Hi"})]
      (is (not (:ok result)))
      (is (re-find #"bot-token" (:error result))))))

(deftest send-message-increments-message-count
  (testing "Successful send increments message-count metric"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "chat.postMessage" mock-send-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (is (= 0 (:message-count @(:state conn))))
        (slack/send-message conn {:channel "#test" :text "msg1"})
        (is (= 1 (:message-count @(:state conn))))
        (slack/send-message conn {:channel "#test" :text "msg2"})
        (is (= 2 (:message-count @(:state conn))))
        (connector/disconnect! conn)))))

(deftest send-message-increments-error-count-on-failure
  (testing "Failed send increments error-count metric"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "chat.postMessage" mock-send-channel-not-found}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (slack/send-message conn {:channel "#bad" :text "fail"})
        (is (= 1 (:error-count @(:state conn))))
        (is (= "channel_not_found" (:last-error @(:state conn))))
        (connector/disconnect! conn)))))

;; =============================================================================
;; List Channels Tests
;; =============================================================================

(deftest list-channels-success
  (testing "list-channels returns parsed channel list"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "conversations.list" mock-channels-response}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [result (slack/list-channels conn {})]
          (is (:ok result))
          (is (= 3 (count (:channels result))))
          (let [ch (first (:channels result))]
            (is (= "C001" (:id ch)))
            (is (= "general" (:name ch)))
            (is (= "General discussion" (:topic ch)))
            (is (= 42 (:num-members ch)))
            (is (false? (:is-archived ch)))))
        (connector/disconnect! conn)))))

(deftest list-channels-with-pagination
  (testing "list-channels returns cursor for pagination"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "conversations.list" mock-channels-paginated}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [result (slack/list-channels conn {:limit 1})]
          (is (:ok result))
          (is (= 1 (count (:channels result))))
          (is (= "dXNlcjpVMEc5V0ZYTlo=" (:cursor result))
              "Should return pagination cursor"))
        (connector/disconnect! conn)))))

(deftest list-channels-without-bot-token
  (testing "list-channels fails without bot-token"
    (let [conn (slack/->slack-connector :test-slack)
          _ (reset! (:state conn) {:connected? true})
          result (slack/list-channels conn {})]
      (is (not (:ok result)))
      (is (re-find #"bot-token" (:error result))))))

;; =============================================================================
;; Read History Tests
;; =============================================================================

(deftest read-history-success
  (testing "read-history returns parsed messages"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "conversations.history" mock-history-response}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [result (slack/read-history conn {:channel "C001" :limit 10})]
          (is (:ok result))
          (is (= 2 (count (:messages result))))
          (let [msg (first (:messages result))]
            (is (= "Hello world" (:text msg)))
            (is (= "U1234" (:user msg)))
            (is (= "message" (:type msg)))
            (is (nil? (:thread-ts msg))))
          (let [msg (second (:messages result))]
            (is (= "1234567890.123456" (:thread-ts msg)))
            (is (= 3 (:reply-count msg))))
          (is (false? (:has-more result))))
        (connector/disconnect! conn)))))

(deftest read-history-missing-channel
  (testing "read-history fails without :channel option"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [result (slack/read-history conn {})]
          (is (not (:ok result)))
          (is (re-find #"channel" (:error result))))
        (connector/disconnect! conn)))))

(deftest read-history-without-bot-token
  (testing "read-history fails without bot-token"
    (let [conn (slack/->slack-connector :test-slack)
          _ (reset! (:state conn) {:connected? true})
          result (slack/read-history conn {:channel "C001"})]
      (is (not (:ok result)))
      (is (re-find #"bot-token" (:error result))))))

(deftest read-history-api-error
  (testing "read-history handles API errors gracefully"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "conversations.history" {:ok false :error "channel_not_found"}}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [result (slack/read-history conn {:channel "C999"})]
          (is (not (:ok result)))
          (is (= "channel_not_found" (:error result))))
        (connector/disconnect! conn)))))

;; =============================================================================
;; IConnector send/receive/sync! Tests
;; =============================================================================

(deftest protocol-send-string
  (testing "IConnector send with string data"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "chat.postMessage" mock-send-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test" :channel "#default"})
        (let [result (connector/send conn "Hello from protocol!" {})]
          (is (:success? result))
          (is (some? (get-in result [:response :ts])))
          (is (empty? (:errors result)))
          (is (some? (:sent-at result))))
        (connector/disconnect! conn)))))

(deftest protocol-send-map-data
  (testing "IConnector send with map data extracts :text"
    (let [captured-body (atom nil)]
      (with-mock-slack {"auth.test" mock-auth-ok
                        "chat.postMessage"
                        (fn [_ _ _ opts]
                          (reset! captured-body (:body opts))
                          mock-send-ok)}
        (let [conn (slack/->slack-connector :test-slack)]
          (connector/connect! conn {:bot-token "xoxb-test" :channel "#default"})
          (connector/send conn {:text "Map message"} {})
          (is (= "Map message" (:text @captured-body)))
          (connector/disconnect! conn))))))

(deftest protocol-send-when-disconnected
  (testing "IConnector send returns failure when not connected"
    (let [conn (slack/->slack-connector :test-slack)
          result (connector/send conn "test" {})]
      (is (not (:success? result)))
      (is (some #(re-find #"Not connected" %) (:errors result))))))

(deftest protocol-send-without-bot-token
  (testing "IConnector send returns failure without bot-token"
    (let [conn (slack/->slack-connector :test-slack)
          _ (reset! (:state conn) {:connected? true})
          result (connector/send conn "test" {})]
      (is (not (:success? result)))
      (is (some #(re-find #"bot-token" %) (:errors result))))))

(deftest protocol-receive-success
  (testing "IConnector receive pulls messages from conversations.history"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "conversations.history" mock-history-response}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test" :channel "C001"})
        (let [result (connector/receive conn {:channel "C001" :limit 10})]
          (is (:success? result))
          (is (= 2 (:count result)))
          (is (= 2 (count (:data result))))
          (is (= "Hello world" (:text (first (:data result)))))
          (is (false? (:has-more? result))))
        (connector/disconnect! conn)))))

(deftest protocol-receive-when-disconnected
  (testing "IConnector receive returns failure when not connected"
    (let [conn (slack/->slack-connector :test-slack)
          result (connector/receive conn {})]
      (is (not (:success? result)))
      (is (= 0 (:count result)))
      (is (empty? (:data result))))))

(deftest protocol-sync-inbound
  (testing "IConnector sync! :inbound does receive"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "conversations.history" mock-history-response}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test" :channel "C001"})
        (let [result (connector/sync! conn {:direction :inbound :channel "C001"})]
          (is (:success? result))
          (is (= 0 (:sent result)))
          (is (= 2 (:received result)))
          (is (empty? (:conflicts result)))
          (is (some? (:synced-at result))))
        (connector/disconnect! conn)))))

(deftest protocol-sync-outbound
  (testing "IConnector sync! :outbound does send"
    (with-mock-slack {"auth.test" mock-auth-ok
                      "chat.postMessage" mock-send-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test" :channel "#test"})
        (let [result (connector/sync! conn {:direction :outbound
                                            :data "Sync message"
                                            :channel "#test"})]
          (is (:success? result))
          (is (= 1 (:sent result)))
          (is (= 0 (:received result))))
        (connector/disconnect! conn)))))

(deftest protocol-sync-outbound-no-data
  (testing "IConnector sync! :outbound without :data returns error"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :test-slack)]
        (connector/connect! conn {:bot-token "xoxb-test"})
        (let [result (connector/sync! conn {:direction :outbound})]
          (is (not (:success? result)))
          (is (seq (:errors result))))
        (connector/disconnect! conn)))))

;; =============================================================================
;; Constructor & Convenience Tests
;; =============================================================================

(deftest default-constructor
  (testing "Default constructor creates connector with :hivemind-slack id"
    (let [conn (slack/->slack-connector)]
      (is (= :hivemind-slack (connector/connector-id conn)))
      (is (not (connector/connected? conn))))))

(deftest custom-id-constructor
  (testing "Custom ID constructor works"
    (let [conn (slack/->slack-connector :my-custom-slack)]
      (is (= :my-custom-slack (connector/connector-id conn))))))

;; =============================================================================
;; Registry Integration Tests
;; =============================================================================

(deftest register-and-lookup
  (testing "SlackConnector can be registered and looked up"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/->slack-connector :registry-test)]
        (connector/register-connector! conn)
        (is (connector/connector-registered? :registry-test))
        (is (= conn (connector/get-connector :registry-test)))
        (connector/unregister-connector! :registry-test)
        (is (not (connector/connector-registered? :registry-test)))))))

(deftest start-slack-connector-convenience
  (testing "start-slack-connector! creates, registers, and connects"
    (with-mock-slack {"auth.test" mock-auth-ok}
      (let [conn (slack/start-slack-connector!
                  {:bot-token "xoxb-test"
                   :id :convenience-test
                   :channel "#hivemind"})]
        (is (connector/connected? conn))
        (is (connector/connector-registered? :convenience-test))
        (slack/stop-slack-connector! :convenience-test)
        (is (not (connector/connector-registered? :convenience-test)))))))

(deftest start-slack-connector-fails-cleanly
  (testing "start-slack-connector! cleans up on failure"
    (with-mock-slack {"auth.test" mock-auth-fail}
      (is (thrown? clojure.lang.ExceptionInfo
                   (slack/start-slack-connector!
                    {:bot-token "xoxb-invalid"
                     :id :fail-test})))
      (is (not (connector/connector-registered? :fail-test))))))

;; =============================================================================
;; Mock API Request Parameter Verification
;; =============================================================================

(deftest slack-api-request-passes-params
  (testing "slack-api-request forwards parameters correctly"
    (let [captured (atom nil)]
      (with-mock-slack {"auth.test" mock-auth-ok
                        "chat.postMessage"
                        (fn [method endpoint token opts]
                          (reset! captured {:method method
                                            :endpoint endpoint
                                            :token token
                                            :body (:body opts)})
                          mock-send-ok)}
        (let [conn (slack/->slack-connector :test-slack)]
          (connector/connect! conn {:bot-token "xoxb-captured"})
          (slack/send-message conn {:channel "#test" :text "capture me"})
          (is (= :post (:method @captured)))
          (is (= "chat.postMessage" (:endpoint @captured)))
          (is (= "xoxb-captured" (:token @captured)))
          (is (= "#test" (:channel (:body @captured))))
          (is (= "capture me" (:text (:body @captured))))
          (connector/disconnect! conn))))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest send-message-with-thread-ts
  (testing "send-message passes thread_ts for threaded replies"
    (let [captured-body (atom nil)]
      (with-mock-slack {"auth.test" mock-auth-ok
                        "chat.postMessage"
                        (fn [_ _ _ opts]
                          (reset! captured-body (:body opts))
                          mock-send-ok)}
        (let [conn (slack/->slack-connector :test-slack)]
          (connector/connect! conn {:bot-token "xoxb-test"})
          (slack/send-message conn {:channel "#test"
                                    :text "thread reply"
                                    :thread-ts "1234567890.123456"})
          (is (= "1234567890.123456" (:thread_ts @captured-body)))
          (connector/disconnect! conn))))))

(deftest send-message-with-blocks
  (testing "send-message supports Block Kit blocks"
    (let [captured-body (atom nil)]
      (with-mock-slack {"auth.test" mock-auth-ok
                        "chat.postMessage"
                        (fn [_ _ _ opts]
                          (reset! captured-body (:body opts))
                          mock-send-ok)}
        (let [conn (slack/->slack-connector :test-slack)
              blocks [{:type "section"
                       :text {:type "mrkdwn" :text "*Bold text*"}}]]
          (connector/connect! conn {:bot-token "xoxb-test"})
          (slack/send-message conn {:channel "#test"
                                    :blocks blocks})
          (is (= blocks (:blocks @captured-body)))
          (connector/disconnect! conn))))))

(deftest multiple-operations-track-metrics
  (testing "Multiple operations correctly track message and error counts"
    (let [call-count (atom 0)]
      (with-mock-slack {"auth.test" mock-auth-ok
                        "chat.postMessage"
                        (fn [_ _ _ _]
                          (swap! call-count inc)
                          (if (odd? @call-count)
                            mock-send-ok
                            mock-send-channel-not-found))}
        (let [conn (slack/->slack-connector :test-slack)]
          (connector/connect! conn {:bot-token "xoxb-test"})
          ;; 1st call: success
          (slack/send-message conn {:channel "#test" :text "1"})
          ;; 2nd call: failure
          (slack/send-message conn {:channel "#bad" :text "2"})
          ;; 3rd call: success
          (slack/send-message conn {:channel "#test" :text "3"})
          (let [status (connector/get-status conn)]
            (is (= 2 (get-in status [:metrics :message-count])))
            (is (= 1 (:error-count status))))
          (connector/disconnect! conn))))))
