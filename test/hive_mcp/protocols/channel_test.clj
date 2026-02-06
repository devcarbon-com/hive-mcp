(ns hive-mcp.protocols.channel-test
  "Tests for IChannel, IVoice, IRouter protocols and their Noop implementations.

   Validates:
   - NoopChannel: all IChannel protocol methods and return shapes
   - NoopVoice: all IVoice protocol methods (start-call, end-call, mute, unmute, call-status)
   - NoopRouter: all IRouter protocol methods (route-message, register/unregister routes)
   - Channel registry CRUD (register, get, list, unregister, reset)
   - Voice registry CRUD (register, get, list, unregister)
   - Router registry CRUD (register, get, list, unregister)
   - Protocol satisfaction checks
   - Utility functions (generate-message-id, generate-subscription-id, make-message, topic-matches?)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.channel :as ch]))

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

(defn clean-registries
  "Reset all registries between tests to ensure isolation."
  [f]
  ;; Clear channel registry
  (doseq [{:keys [id]} (ch/list-channels)]
    (ch/unregister-channel! id))
  ;; Clear voice registry
  (doseq [id (ch/list-voices)]
    (ch/unregister-voice! id))
  ;; Clear router registry
  (doseq [{:keys [id]} (ch/list-routers)]
    (ch/unregister-router! id))
  (f)
  ;; Cleanup after test
  (doseq [{:keys [id]} (ch/list-channels)]
    (ch/unregister-channel! id))
  (doseq [id (ch/list-voices)]
    (ch/unregister-voice! id))
  (doseq [{:keys [id]} (ch/list-routers)]
    (ch/unregister-router! id)))

(use-fixtures :each clean-registries)

;;; ============================================================================
;;; NoopChannel - Core Operations
;;; ============================================================================

(deftest noop-channel-id-test
  (testing "channel-id returns the ID passed at construction"
    (let [ch (ch/->noop-channel :test-ch)]
      (is (= :test-ch (ch/channel-id ch)))))

  (testing "default channel-id is :noop-channel"
    (let [ch (ch/->noop-channel)]
      (is (= :noop-channel (ch/channel-id ch))))))

(deftest noop-channel-info-test
  (testing "channel-info returns expected metadata map"
    (let [c (ch/->noop-channel :test-ch)
          info (ch/channel-info c)]
      (is (map? info))
      (is (= :test-ch (:id info)))
      (is (= "Noop Channel" (:name info)))
      (is (string? (:description info)))
      (is (= :noop (:type info)))
      (is (instance? java.time.Instant (:created-at info)))
      (is (set? (:capabilities info))))))

(deftest noop-channel-send-test
  (testing "send! with 3 args returns success"
    (let [c (ch/->noop-channel :test-ch)
          result (ch/send! c "topic" {:data "hello"})]
      (is (true? (:success? result)))
      (is (string? (:message-id result)))
      (is (.startsWith (:message-id result) "msg-"))
      (is (= [] (:errors result)))))

  (testing "send! with 4 args returns success"
    (let [c (ch/->noop-channel :test-ch)
          result (ch/send! c "topic" {:data "hello"} {:priority :high})]
      (is (true? (:success? result)))
      (is (string? (:message-id result)))
      (is (= [] (:errors result))))))

(deftest noop-channel-send-async-test
  (testing "send-async! returns a future that resolves to success"
    (let [c (ch/->noop-channel :test-ch)
          f (ch/send-async! c "topic" {:data "hello"} {})]
      (is (future? f))
      (let [result @f]
        (is (true? (:success? result)))
        (is (string? (:message-id result)))))))

(deftest noop-channel-broadcast-test
  (testing "broadcast! returns success with zero delivery"
    (let [c (ch/->noop-channel :test-ch)
          result (ch/broadcast! c {:data "hello"} {})]
      (is (true? (:success? result)))
      (is (= 0 (:delivered-count result)))
      (is (= [] (:errors result))))))

(deftest noop-channel-receive-test
  (testing "receive! with 2 args returns nil"
    (let [c (ch/->noop-channel :test-ch)]
      (is (nil? (ch/receive! c "topic")))))

  (testing "receive! with 3 args returns nil"
    (let [c (ch/->noop-channel :test-ch)]
      (is (nil? (ch/receive! c "topic" {:timeout-ms 100}))))))

(deftest noop-channel-receive-batch-test
  (testing "receive-batch! returns empty"
    (let [c (ch/->noop-channel :test-ch)
          result (ch/receive-batch! c "topic" {:max-messages 10})]
      (is (= [] (:messages result)))
      (is (= 0 (:count result)))
      (is (true? (:complete? result))))))

(deftest noop-channel-subscribe-test
  (testing "subscribe! with 3 args returns success"
    (let [c (ch/->noop-channel :test-ch)
          result (ch/subscribe! c "topic.*" identity)]
      (is (true? (:success? result)))
      (is (string? (:subscription-id result)))
      (is (.startsWith (:subscription-id result) "sub-"))
      (is (= [] (:errors result)))))

  (testing "subscribe! with 4 args returns success"
    (let [c (ch/->noop-channel :test-ch)
          result (ch/subscribe! c "topic.*" identity {:buffer-size 50})]
      (is (true? (:success? result)))
      (is (string? (:subscription-id result))))))

(deftest noop-channel-unsubscribe-test
  (testing "unsubscribe! returns success"
    (let [c (ch/->noop-channel :test-ch)
          result (ch/unsubscribe! c "sub-123")]
      (is (true? (:success? result)))
      (is (= [] (:errors result))))))

(deftest noop-channel-subscriptions-test
  (testing "subscriptions returns empty vector"
    (let [c (ch/->noop-channel :test-ch)]
      (is (= [] (ch/subscriptions c))))))

(deftest noop-channel-lifecycle-test
  (testing "open? is true after creation"
    (let [c (ch/->noop-channel :test-ch)]
      (is (true? (ch/open? c)))))

  (testing "close! transitions to closed state"
    (let [c (ch/->noop-channel :test-ch)
          result (ch/close! c)]
      (is (true? (:success? result)))
      (is (= 0 (:pending-dropped result)))
      (is (= 0 (:subscriptions-removed result)))
      (is (false? (ch/open? c)))))

  (testing "close! is idempotent"
    (let [c (ch/->noop-channel :test-ch)]
      (ch/close! c)
      (let [result (ch/close! c)]
        (is (true? (:success? result)))
        (is (false? (ch/open? c)))))))

(deftest noop-channel-drain-test
  (testing "drain! returns success with zero count"
    (let [c (ch/->noop-channel :test-ch)
          result (ch/drain! c {:timeout-ms 1000})]
      (is (true? (:success? result)))
      (is (= 0 (:drained-count result)))
      (is (= 0 (:remaining result))))))

(deftest noop-channel-status-test
  (testing "channel-status when open"
    (let [c (ch/->noop-channel :test-ch)
          status (ch/channel-status c)]
      (is (= :test-ch (:id status)))
      (is (true? (:open? status)))
      (is (= :noop (:type status)))
      (is (= 0 (:subscription-count status)))
      (is (= 0 (:pending-count status)))
      (is (= 0 (:buffer-size status)))
      (is (= 0 (:buffer-used status)))
      (is (instance? java.time.Instant (:created-at status)))
      (is (nil? (:last-activity status)))
      (is (= {} (:metrics status)))))

  (testing "channel-status when closed"
    (let [c (ch/->noop-channel :test-ch)]
      (ch/close! c)
      (let [status (ch/channel-status c)]
        (is (false? (:open? status)))))))

;;; ============================================================================
;;; Channel Registry Tests
;;; ============================================================================

(deftest channel-registry-register-test
  (testing "register-channel! stores and returns channel"
    (let [c (ch/->noop-channel :reg-test)
          result (ch/register-channel! c)]
      (is (= c result))
      (is (true? (ch/channel-registered? :reg-test)))))

  (testing "register-channel! rejects non-IChannel"
    (is (thrown? AssertionError
                 (ch/register-channel! {:not "a channel"})))))

(deftest channel-registry-get-test
  (testing "get-channel returns registered channel"
    (let [c (ch/->noop-channel :get-test)]
      (ch/register-channel! c)
      (is (= c (ch/get-channel :get-test)))))

  (testing "get-channel returns nil for unknown"
    (is (nil? (ch/get-channel :nonexistent)))))

(deftest channel-registry-list-test
  (testing "list-channels returns all with status"
    (ch/register-channel! (ch/->noop-channel :list-a))
    (ch/register-channel! (ch/->noop-channel :list-b))
    (let [channels (ch/list-channels)]
      (is (= 2 (count channels)))
      (is (every? :id channels))
      (is (every? :status channels)))))

(deftest channel-registry-unregister-test
  (testing "unregister-channel! removes and closes"
    (let [c (ch/->noop-channel :unreg-test)]
      (ch/register-channel! c)
      (is (true? (ch/open? c)))
      (is (true? (ch/unregister-channel! :unreg-test)))
      (is (false? (ch/channel-registered? :unreg-test)))
      (is (false? (ch/open? c)))))

  (testing "unregister-channel! returns false for unknown"
    (is (false? (ch/unregister-channel! :nonexistent)))))

(deftest channel-registry-reset-test
  (testing "reset-registry! removes all channels"
    (ch/register-channel! (ch/->noop-channel :reset-a))
    (ch/register-channel! (ch/->noop-channel :reset-b))
    (let [count (ch/reset-registry!)]
      (is (= 2 count))
      (is (= [] (ch/list-channels))))))

;;; ============================================================================
;;; NoopVoice - Core Operations
;;; ============================================================================

(deftest noop-voice-id-test
  (testing "voice-id returns the ID passed at construction"
    (let [v (ch/->noop-voice :test-voice)]
      (is (= :test-voice (ch/voice-id v)))))

  (testing "default voice-id is :noop-voice"
    (let [v (ch/->noop-voice)]
      (is (= :noop-voice (ch/voice-id v))))))

(deftest noop-voice-start-call-test
  (testing "start-call returns success with call-id"
    (let [v (ch/->noop-voice :test-voice)
          result (ch/start-call v ["agent-1" "agent-2"] {})]
      (is (true? (:success? result)))
      (is (string? (:call-id result)))
      (is (.startsWith (:call-id result) "call-"))
      (is (= :active (:status result)))
      (is (= [] (:errors result)))))

  (testing "start-call creates trackable call state"
    (let [v (ch/->noop-voice :test-voice)
          result (ch/start-call v ["agent-1" "agent-2"] {})
          call-id (:call-id result)
          status (ch/call-status v call-id)]
      (is (= :active (:status status)))
      (is (= ["agent-1" "agent-2"] (:participants status)))
      (is (= "agent-1" (:initiator status)))
      (is (false? (:muted? status)))
      (is (instance? java.time.Instant (:started-at status))))))

(deftest noop-voice-end-call-test
  (testing "end-call succeeds for active call"
    (let [v (ch/->noop-voice :test-voice)
          start-result (ch/start-call v ["agent-1"] {})
          call-id (:call-id start-result)
          end-result (ch/end-call v call-id)]
      (is (true? (:success? end-result)))
      (is (number? (:duration-ms end-result)))
      (is (>= (:duration-ms end-result) 0))
      (is (= [] (:errors end-result)))))

  (testing "end-call transitions status to :ended"
    (let [v (ch/->noop-voice :test-voice)
          call-id (:call-id (ch/start-call v ["agent-1"] {}))
          _ (ch/end-call v call-id)
          status (ch/call-status v call-id)]
      (is (= :ended (:status status)))))

  (testing "end-call fails for unknown call-id"
    (let [v (ch/->noop-voice :test-voice)
          result (ch/end-call v "nonexistent")]
      (is (false? (:success? result)))
      (is (nil? (:duration-ms result)))
      (is (= ["Unknown call-id"] (:errors result))))))

(deftest noop-voice-mute-test
  (testing "mute succeeds for active call"
    (let [v (ch/->noop-voice :test-voice)
          call-id (:call-id (ch/start-call v ["agent-1"] {}))
          result (ch/mute v call-id)]
      (is (true? (:success? result)))
      (is (true? (:muted? result)))
      (is (= [] (:errors result)))))

  (testing "mute updates call state"
    (let [v (ch/->noop-voice :test-voice)
          call-id (:call-id (ch/start-call v ["agent-1"] {}))
          _ (ch/mute v call-id)
          status (ch/call-status v call-id)]
      (is (true? (:muted? status)))))

  (testing "mute fails for unknown call-id"
    (let [v (ch/->noop-voice :test-voice)
          result (ch/mute v "nonexistent")]
      (is (false? (:success? result)))
      (is (= ["Unknown call-id"] (:errors result))))))

(deftest noop-voice-unmute-test
  (testing "unmute succeeds for muted call"
    (let [v (ch/->noop-voice :test-voice)
          call-id (:call-id (ch/start-call v ["agent-1"] {}))
          _ (ch/mute v call-id)
          result (ch/unmute v call-id)]
      (is (true? (:success? result)))
      (is (false? (:muted? result)))
      (is (= [] (:errors result)))))

  (testing "unmute updates call state"
    (let [v (ch/->noop-voice :test-voice)
          call-id (:call-id (ch/start-call v ["agent-1"] {}))
          _ (ch/mute v call-id)
          _ (ch/unmute v call-id)
          status (ch/call-status v call-id)]
      (is (false? (:muted? status)))))

  (testing "unmute is idempotent"
    (let [v (ch/->noop-voice :test-voice)
          call-id (:call-id (ch/start-call v ["agent-1"] {}))
          r1 (ch/unmute v call-id)
          r2 (ch/unmute v call-id)]
      (is (true? (:success? r1)))
      (is (true? (:success? r2)))
      (is (false? (:muted? r1)))
      (is (false? (:muted? r2)))))

  (testing "unmute fails for unknown call-id"
    (let [v (ch/->noop-voice :test-voice)
          result (ch/unmute v "nonexistent")]
      (is (false? (:success? result)))
      (is (= ["Unknown call-id"] (:errors result))))))

(deftest noop-voice-call-status-test
  (testing "call-status returns nil for unknown call"
    (let [v (ch/->noop-voice :test-voice)]
      (is (nil? (ch/call-status v "nonexistent")))))

  (testing "call-status returns duration-ms for active call"
    (let [v (ch/->noop-voice :test-voice)
          call-id (:call-id (ch/start-call v ["agent-1" "agent-2"] {}))
          status (ch/call-status v call-id)]
      (is (= call-id (:call-id status)))
      (is (= :active (:status status)))
      (is (number? (:duration-ms status)))
      (is (>= (:duration-ms status) 0))))

  (testing "call-status returns nil duration-ms for ended call"
    (let [v (ch/->noop-voice :test-voice)
          call-id (:call-id (ch/start-call v ["agent-1"] {}))
          _ (ch/end-call v call-id)
          status (ch/call-status v call-id)]
      (is (= :ended (:status status)))
      (is (nil? (:duration-ms status))))))

;;; ============================================================================
;;; Voice Registry Tests
;;; ============================================================================

(deftest voice-registry-register-test
  (testing "register-voice! stores and returns voice"
    (let [v (ch/->noop-voice :reg-voice)
          result (ch/register-voice! v)]
      (is (= v result))
      (is (true? (ch/voice-registered? :reg-voice)))))

  (testing "register-voice! rejects non-IVoice"
    (is (thrown? AssertionError
                 (ch/register-voice! {:not "a voice"})))))

(deftest voice-registry-get-test
  (testing "get-voice returns registered voice"
    (let [v (ch/->noop-voice :get-voice)]
      (ch/register-voice! v)
      (is (= v (ch/get-voice :get-voice)))))

  (testing "get-voice returns nil for unknown"
    (is (nil? (ch/get-voice :nonexistent)))))

(deftest voice-registry-list-test
  (testing "list-voices returns all IDs"
    (ch/register-voice! (ch/->noop-voice :voice-a))
    (ch/register-voice! (ch/->noop-voice :voice-b))
    (let [ids (ch/list-voices)]
      (is (= 2 (count ids)))
      (is (some #{:voice-a} ids))
      (is (some #{:voice-b} ids)))))

(deftest voice-registry-unregister-test
  (testing "unregister-voice! removes voice"
    (ch/register-voice! (ch/->noop-voice :unreg-voice))
    (is (true? (ch/unregister-voice! :unreg-voice)))
    (is (false? (ch/voice-registered? :unreg-voice))))

  (testing "unregister-voice! returns false for unknown"
    (is (false? (ch/unregister-voice! :nonexistent)))))

;;; ============================================================================
;;; NoopRouter - Core Operations
;;; ============================================================================

(deftest noop-router-id-test
  (testing "router-id returns the ID passed at construction"
    (let [r (ch/->noop-router :test-router)]
      (is (= :test-router (ch/router-id r)))))

  (testing "default router-id is :noop-router"
    (let [r (ch/->noop-router)]
      (is (= :noop-router (ch/router-id r))))))

(deftest noop-router-info-test
  (testing "router-info returns expected metadata"
    (let [r (ch/->noop-router :test-router)
          info (ch/router-info r)]
      (is (map? info))
      (is (= :test-router (:id info)))
      (is (= "Noop Router" (:name info)))
      (is (string? (:description info)))
      (is (= :noop (:strategy info)))
      (is (= 0 (:channel-count info))))))

(deftest noop-router-route-message-test
  (testing "route-message returns success with empty routing"
    (let [r (ch/->noop-router :test-router)
          result (ch/route-message r "topic" {:data "hello"} {})]
      (is (true? (:success? result)))
      (is (= [] (:routed-to result)))
      (is (string? (:message-id result)))
      (is (.startsWith (:message-id result) "msg-"))
      (is (= [] (:errors result))))))

(deftest noop-router-register-route-test
  (testing "register-route! adds channel"
    (let [r (ch/->noop-router :test-router)
          c (ch/->noop-channel :ch-1)
          result (ch/register-route! r :ch-1 c {:topics #{"agent.*"}})]
      (is (true? (:success? result)))
      (is (= [] (:errors result)))))

  (testing "register-route! updates channel-count in info"
    (let [r (ch/->noop-router :test-router)
          c1 (ch/->noop-channel :ch-1)
          c2 (ch/->noop-channel :ch-2)]
      (ch/register-route! r :ch-1 c1 {:topics #{"a.*"}})
      (ch/register-route! r :ch-2 c2 {:topics #{"b.*"}})
      (is (= 2 (:channel-count (ch/router-info r)))))))

(deftest noop-router-unregister-route-test
  (testing "unregister-route! removes channel"
    (let [r (ch/->noop-router :test-router)
          c (ch/->noop-channel :ch-1)]
      (ch/register-route! r :ch-1 c {})
      (let [result (ch/unregister-route! r :ch-1)]
        (is (true? (:success? result)))
        (is (= [] (:errors result))))
      (is (= 0 (:channel-count (ch/router-info r))))))

  (testing "unregister-route! is idempotent"
    (let [r (ch/->noop-router :test-router)
          r1 (ch/unregister-route! r :nonexistent)
          r2 (ch/unregister-route! r :nonexistent)]
      (is (true? (:success? r1)))
      (is (true? (:success? r2))))))

(deftest noop-router-channels-test
  (testing "router-channels returns empty for fresh router"
    (let [r (ch/->noop-router :test-router)]
      (is (= [] (ch/router-channels r)))))

  (testing "router-channels lists registered channels"
    (let [r (ch/->noop-router :test-router)
          c (ch/->noop-channel :ch-1)]
      (ch/register-route! r :ch-1 c {:topics #{"topic.*"} :priority 5})
      (let [channels (ch/router-channels r)]
        (is (= 1 (count channels)))
        (let [ch-info (first channels)]
          (is (= :ch-1 (:channel-id ch-info)))
          (is (= #{"topic.*"} (:topics ch-info)))
          (is (= 5 (:priority ch-info)))
          (is (true? (:open? ch-info))))))))

(deftest noop-router-resolve-route-test
  (testing "resolve-route returns empty vector"
    (let [r (ch/->noop-router :test-router)]
      (is (= [] (ch/resolve-route r "any.topic"))))))

;;; ============================================================================
;;; Router Registry Tests
;;; ============================================================================

(deftest router-registry-register-test
  (testing "register-router! stores and returns router"
    (let [r (ch/->noop-router :reg-router)
          result (ch/register-router! r)]
      (is (= r result))
      (is (true? (ch/router-registered? :reg-router)))))

  (testing "register-router! rejects non-IRouter"
    (is (thrown? AssertionError
                 (ch/register-router! {:not "a router"})))))

(deftest router-registry-get-test
  (testing "get-router returns registered router"
    (let [r (ch/->noop-router :get-router)]
      (ch/register-router! r)
      (is (= r (ch/get-router :get-router)))))

  (testing "get-router returns nil for unknown"
    (is (nil? (ch/get-router :nonexistent)))))

(deftest router-registry-list-test
  (testing "list-routers returns all with info"
    (ch/register-router! (ch/->noop-router :router-a))
    (ch/register-router! (ch/->noop-router :router-b))
    (let [routers (ch/list-routers)]
      (is (= 2 (count routers)))
      (is (every? :id routers))
      (is (every? :strategy routers)))))

(deftest router-registry-unregister-test
  (testing "unregister-router! removes router"
    (ch/register-router! (ch/->noop-router :unreg-router))
    (is (true? (ch/unregister-router! :unreg-router)))
    (is (false? (ch/router-registered? :unreg-router))))

  (testing "unregister-router! returns false for unknown"
    (is (false? (ch/unregister-router! :nonexistent)))))

;;; ============================================================================
;;; Protocol Satisfaction Tests
;;; ============================================================================

(deftest protocol-satisfaction-test
  (testing "NoopChannel satisfies IChannel"
    (is (satisfies? ch/IChannel (ch/->noop-channel))))

  (testing "NoopVoice satisfies IVoice"
    (is (satisfies? ch/IVoice (ch/->noop-voice))))

  (testing "NoopRouter satisfies IRouter"
    (is (satisfies? ch/IRouter (ch/->noop-router))))

  (testing "NoopChannel does NOT satisfy IVoice"
    (is (not (satisfies? ch/IVoice (ch/->noop-channel)))))

  (testing "NoopVoice does NOT satisfy IChannel"
    (is (not (satisfies? ch/IChannel (ch/->noop-voice)))))

  (testing "NoopRouter does NOT satisfy IChannel"
    (is (not (satisfies? ch/IChannel (ch/->noop-router))))))

;;; ============================================================================
;;; Utility Function Tests
;;; ============================================================================

(deftest generate-message-id-test
  (testing "generates unique IDs"
    (let [ids (repeatedly 100 ch/generate-message-id)]
      (is (= 100 (count (set ids))))))

  (testing "IDs start with msg- prefix"
    (is (.startsWith (ch/generate-message-id) "msg-"))))

(deftest generate-subscription-id-test
  (testing "generates unique IDs"
    (let [ids (repeatedly 100 ch/generate-subscription-id)]
      (is (= 100 (count (set ids))))))

  (testing "IDs start with sub- prefix"
    (is (.startsWith (ch/generate-subscription-id) "sub-"))))

(deftest make-message-test
  (testing "creates well-formed message"
    (let [msg (ch/make-message "test.topic" {:key "value"})]
      (is (string? (:id msg)))
      (is (= "test.topic" (:topic msg)))
      (is (= {:key "value"} (:payload msg)))
      (is (instance? java.time.Instant (:timestamp msg)))
      (is (= "unknown" (:sender msg)))
      (is (= :normal (get-in msg [:headers :priority])))))

  (testing "respects opts"
    (let [msg (ch/make-message "t" {} {:sender "agent-1"
                                       :priority :high
                                       :ttl-ms 5000})]
      (is (= "agent-1" (:sender msg)))
      (is (= :high (get-in msg [:headers :priority])))
      (is (= 5000 (get-in msg [:headers :ttl-ms]))))))

(deftest topic-matches?-test
  (testing "exact match"
    (is (true? (ch/topic-matches? "a.b.c" "a.b.c")))
    (is (false? (ch/topic-matches? "a.b.c" "a.b.d"))))

  (testing "single-level wildcard *"
    (is (true? (ch/topic-matches? "a.*" "a.b")))
    (is (false? (ch/topic-matches? "a.*" "a.b.c"))))

  (testing "multi-level wildcard #"
    (is (true? (ch/topic-matches? "a.#" "a.b")))
    (is (true? (ch/topic-matches? "a.#" "a.b.c")))
    (is (true? (ch/topic-matches? "a.#" "a"))))

  (testing "multi-level wildcard > (NATS-style)"
    (is (true? (ch/topic-matches? "a.>" "a.b")))
    (is (true? (ch/topic-matches? "a.>" "a.b.c.d")))))

;;; ============================================================================
;;; Full Voice Lifecycle Test
;;; ============================================================================

(deftest full-voice-lifecycle-test
  (testing "complete voice lifecycle: register -> start-call -> mute -> unmute -> end-call -> unregister"
    (let [v (ch/->noop-voice :lifecycle-voice)]
      ;; 1. Register
      (ch/register-voice! v)
      (is (true? (ch/voice-registered? :lifecycle-voice)))

      ;; 2. Start call
      (let [result (ch/start-call v ["agent-1" "agent-2" "agent-3"] {:codec :opus})]
        (is (true? (:success? result)))
        (let [call-id (:call-id result)]

          ;; 3. Check status
          (let [status (ch/call-status v call-id)]
            (is (= :active (:status status)))
            (is (= ["agent-1" "agent-2" "agent-3"] (:participants status))))

          ;; 4. Mute
          (let [mute-result (ch/mute v call-id)]
            (is (true? (:success? mute-result)))
            (is (true? (:muted? (ch/call-status v call-id)))))

          ;; 5. Unmute
          (let [unmute-result (ch/unmute v call-id)]
            (is (true? (:success? unmute-result)))
            (is (false? (:muted? (ch/call-status v call-id)))))

          ;; 6. End call
          (let [end-result (ch/end-call v call-id)]
            (is (true? (:success? end-result)))
            (is (number? (:duration-ms end-result))))

          ;; 7. Verify ended
          (is (= :ended (:status (ch/call-status v call-id))))))

      ;; 8. Unregister
      (is (true? (ch/unregister-voice! :lifecycle-voice)))
      (is (false? (ch/voice-registered? :lifecycle-voice))))))

;;; ============================================================================
;;; Full Router Lifecycle Test
;;; ============================================================================

(deftest full-router-lifecycle-test
  (testing "complete router lifecycle: register -> add channels -> route -> remove channels -> unregister"
    (let [r (ch/->noop-router :lifecycle-router)]
      ;; 1. Register router
      (ch/register-router! r)
      (is (true? (ch/router-registered? :lifecycle-router)))

      ;; 2. Register channels
      (let [c1 (ch/->noop-channel :events-ch)
            c2 (ch/->noop-channel :logs-ch)]
        (ch/register-route! r :events-ch c1 {:topics #{"events.*"} :priority 10})
        (ch/register-route! r :logs-ch c2 {:topics #{"logs.*"} :priority 5})

        ;; 3. Verify channels listed
        (let [channels (ch/router-channels r)]
          (is (= 2 (count channels)))
          (is (some #(= :events-ch (:channel-id %)) channels))
          (is (some #(= :logs-ch (:channel-id %)) channels)))

        ;; 4. Route message
        (let [result (ch/route-message r "events.user.login" {:user "test"} {})]
          (is (true? (:success? result)))
          (is (string? (:message-id result))))

        ;; 5. Remove channel
        (ch/unregister-route! r :logs-ch)
        (is (= 1 (count (ch/router-channels r))))

        ;; 6. Router info reflects channel count
        (is (= 1 (:channel-count (ch/router-info r)))))

      ;; 7. Unregister router
      (is (true? (ch/unregister-router! :lifecycle-router)))
      (is (false? (ch/router-registered? :lifecycle-router))))))
