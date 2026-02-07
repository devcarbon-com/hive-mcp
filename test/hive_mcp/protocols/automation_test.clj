(ns hive-mcp.protocols.automation-test
  "Tests for IBrowserAutomation and IAutomationSession protocols.

   Validates:
   - IBrowserAutomation protocol method signatures and return shapes (all 9 methods)
   - IAutomationSession protocol method signatures and return shapes (all 3 methods)
   - NoopBrowserAutomation fallback behavior
   - NoopAutomationSession fallback behavior
   - Automation registry CRUD
   - Active implementation management (set/get/clear)
   - Capabilities reporting
   - Custom implementation via reify
   - Full lifecycle integration test"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.automation :as auto]))

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

(defn clean-state
  "Reset all registries and active implementation between tests."
  [f]
  (auto/clear-automation!)
  (auto/clear-registry!)
  (f)
  (auto/clear-automation!)
  (auto/clear-registry!))

(use-fixtures :each clean-state)

;;; ============================================================================
;;; NoopAutomationSession Tests
;;; ============================================================================

(deftest noop-session-id-test
  (testing "session-id returns the ID passed at construction"
    (let [s (auto/->noop-session "my-session")]
      (is (= "my-session" (auto/session-id s)))))

  (testing "default session-id is auto-generated with noop prefix"
    (let [s (auto/->noop-session)]
      (is (string? (auto/session-id s)))
      (is (.startsWith (auto/session-id s) "noop-session-")))))

(deftest noop-session-info-test
  (testing "session-info returns expected metadata map"
    (let [s (auto/->noop-session "info-test")
          info (auto/session-info s)]
      (is (map? info))
      (is (= "info-test" (:id info)))
      (is (= :noop (:browser info)))
      (is (true? (:headless? info)))
      (is (instance? java.time.Instant (:created-at info)))
      (is (nil? (:user-agent info)))
      (is (nil? (:viewport info)))
      (is (map? (:metadata info)))
      (is (= :noop (get-in info [:metadata :engine]))))))

(deftest noop-session-active-test
  (testing "active? always returns false for noop session"
    (let [s (auto/->noop-session "active-test")]
      (is (false? (auto/active? s))))))

;;; ============================================================================
;;; NoopBrowserAutomation - launch! / close!
;;; ============================================================================

(deftest noop-automation-launch-test
  (testing "launch! returns failure with noop message"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/launch! a {})]
      (is (map? result))
      (is (false? (:success? result)))
      (is (nil? (:session result)))
      (is (vector? (:errors result)))
      (is (= 1 (count (:errors result))))
      (is (.contains (first (:errors result)) "NoopBrowserAutomation"))
      (is (= :noop (get-in result [:metadata :engine])))))

  (testing "launch! accepts opts without error"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/launch! a {:browser :chromium
                                   :headless? true
                                   :viewport {:width 1920 :height 1080}})]
      (is (false? (:success? result))))))

(deftest noop-automation-close-test
  (testing "close! returns success (idempotent noop)"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/close! a)]
      (is (true? (:success? result)))
      (is (= [] (:errors result)))))

  (testing "close! is idempotent"
    (let [a (auto/->NoopBrowserAutomation)
          r1 (auto/close! a)
          r2 (auto/close! a)]
      (is (true? (:success? r1)))
      (is (true? (:success? r2))))))

;;; ============================================================================
;;; NoopBrowserAutomation - navigate!
;;; ============================================================================

(deftest noop-automation-navigate-test
  (testing "navigate! returns failure with expected shape"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/navigate! a "https://example.com" {})]
      (is (false? (:success? result)))
      (is (nil? (:url result)))
      (is (nil? (:status-code result)))
      (is (= 0 (:duration-ms result)))
      (is (= 1 (count (:errors result))))))

  (testing "navigate! accepts all options"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/navigate! a "https://example.com"
                                 {:wait-until :networkidle
                                  :timeout-ms 60000
                                  :referer "https://google.com"})]
      (is (false? (:success? result))))))

;;; ============================================================================
;;; NoopBrowserAutomation - click!
;;; ============================================================================

(deftest noop-automation-click-test
  (testing "click! returns failure preserving selector"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/click! a "#submit-btn" {})]
      (is (false? (:success? result)))
      (is (= "#submit-btn" (:selector result)))
      (is (= 1 (count (:errors result))))))

  (testing "click! accepts all options"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/click! a "button.primary"
                              {:button :right
                               :count 2
                               :timeout-ms 10000
                               :force? true
                               :position {:x 10 :y 20}})]
      (is (false? (:success? result)))
      (is (= "button.primary" (:selector result))))))

;;; ============================================================================
;;; NoopBrowserAutomation - type-text!
;;; ============================================================================

(deftest noop-automation-type-text-test
  (testing "type-text! returns failure preserving selector and text"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/type-text! a "#username" "admin" {})]
      (is (false? (:success? result)))
      (is (= "#username" (:selector result)))
      (is (= "admin" (:text result)))
      (is (= 1 (count (:errors result))))))

  (testing "type-text! accepts all options"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/type-text! a "input[name=email]" "test@example.com"
                                   {:delay-ms 50
                                    :clear? true
                                    :timeout-ms 5000})]
      (is (false? (:success? result))))))

;;; ============================================================================
;;; NoopBrowserAutomation - screenshot!
;;; ============================================================================

(deftest noop-automation-screenshot-test
  (testing "screenshot! returns failure with expected shape"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/screenshot! a {})]
      (is (false? (:success? result)))
      (is (nil? (:path result)))
      (is (nil? (:bytes result)))
      (is (= :png (:format result)))
      (is (= 1 (count (:errors result))))))

  (testing "screenshot! accepts all options"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/screenshot! a {:path "/tmp/shot.png"
                                       :full-page? true
                                       :format :jpeg
                                       :quality 80
                                       :selector "#main"})]
      (is (false? (:success? result))))))

;;; ============================================================================
;;; NoopBrowserAutomation - get-page-source
;;; ============================================================================

(deftest noop-automation-get-page-source-test
  (testing "get-page-source returns failure with nil html"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/get-page-source a)]
      (is (false? (:success? result)))
      (is (nil? (:html result)))
      (is (nil? (:url result)))
      (is (= 1 (count (:errors result)))))))

;;; ============================================================================
;;; NoopBrowserAutomation - wait-for-selector
;;; ============================================================================

(deftest noop-automation-wait-for-selector-test
  (testing "wait-for-selector returns failure preserving selector"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/wait-for-selector a ".loaded" {})]
      (is (false? (:success? result)))
      (is (= ".loaded" (:selector result)))
      (is (false? (:found? result)))
      (is (= 0 (:duration-ms result)))
      (is (= 1 (count (:errors result))))))

  (testing "wait-for-selector accepts all options"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/wait-for-selector a "div#content"
                                          {:state :hidden
                                           :timeout-ms 60000})]
      (is (false? (:success? result))))))

;;; ============================================================================
;;; NoopBrowserAutomation - evaluate-js
;;; ============================================================================

(deftest noop-automation-evaluate-js-test
  (testing "evaluate-js returns failure with nil result"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/evaluate-js a "document.title" {})]
      (is (false? (:success? result)))
      (is (nil? (:result result)))
      (is (= 1 (count (:errors result))))))

  (testing "evaluate-js accepts all options"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/evaluate-js a "() => window.innerHeight"
                                    {:timeout-ms 5000
                                     :arg {:key "value"}})]
      (is (false? (:success? result))))))

;;; ============================================================================
;;; Automation Registry Tests
;;; ============================================================================

(deftest registry-register-test
  (testing "register-automation! stores and returns automation"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/register-automation! :test-backend a)]
      (is (= a result))
      (is (true? (auto/automation-registered? :test-backend)))))

  (testing "register-automation! rejects non-IBrowserAutomation"
    (is (thrown? AssertionError
                 (auto/register-automation! :bad {:not "an automation"}))))

  (testing "register-automation! rejects non-keyword id"
    (is (thrown? AssertionError
                 (auto/register-automation! "string-id" (auto/->NoopBrowserAutomation))))))

(deftest registry-get-test
  (testing "get-automation returns registered backend"
    (let [a (auto/->NoopBrowserAutomation)]
      (auto/register-automation! :get-test a)
      (is (= a (auto/get-automation :get-test)))))

  (testing "get-automation returns nil for unknown"
    (is (nil? (auto/get-automation :nonexistent)))))

(deftest registry-list-test
  (testing "list-automations returns all IDs"
    (auto/register-automation! :backend-a (auto/->NoopBrowserAutomation))
    (auto/register-automation! :backend-b (auto/->NoopBrowserAutomation))
    (let [ids (auto/list-automations)]
      (is (= 2 (count ids)))
      (is (some #{:backend-a} ids))
      (is (some #{:backend-b} ids)))))

(deftest registry-unregister-test
  (testing "unregister-automation! removes backend"
    (auto/register-automation! :unreg-test (auto/->NoopBrowserAutomation))
    (is (true? (auto/unregister-automation! :unreg-test)))
    (is (false? (auto/automation-registered? :unreg-test))))

  (testing "unregister-automation! returns false for unknown"
    (is (false? (auto/unregister-automation! :nonexistent)))))

;;; ============================================================================
;;; Active Implementation Management Tests
;;; ============================================================================

(deftest set-get-automation-test
  (testing "get-active-automation returns NoopBrowserAutomation when nothing set"
    (let [impl (auto/get-active-automation)]
      (is (instance? hive_mcp.protocols.automation.NoopBrowserAutomation impl))))

  (testing "set-automation! stores and returns implementation"
    (let [a (auto/->NoopBrowserAutomation)
          result (auto/set-automation! a)]
      (is (= a result))
      (is (= a (auto/get-active-automation)))))

  (testing "automation-set? reflects state"
    (auto/clear-automation!)
    (is (false? (auto/automation-set?)))
    (auto/set-automation! (auto/->NoopBrowserAutomation))
    (is (true? (auto/automation-set?))))

  (testing "set-automation! rejects non-satisfying impl"
    (is (thrown? AssertionError
                 (auto/set-automation! {:not "a protocol impl"})))))

(deftest clear-automation-test
  (testing "clear-automation! resets to nil"
    (auto/set-automation! (auto/->NoopBrowserAutomation))
    (is (true? (auto/automation-set?)))
    (auto/clear-automation!)
    (is (false? (auto/automation-set?)))))

(deftest clear-registry-test
  (testing "clear-registry! empties all registrations"
    (auto/register-automation! :a (auto/->NoopBrowserAutomation))
    (auto/register-automation! :b (auto/->NoopBrowserAutomation))
    (is (= 2 (count (auto/list-automations))))
    (auto/clear-registry!)
    (is (= 0 (count (auto/list-automations))))))

;;; ============================================================================
;;; Capabilities Tests
;;; ============================================================================

(deftest capabilities-no-enhanced-test
  (testing "capabilities with no enhanced implementation"
    (let [caps (auto/capabilities)]
      (is (map? caps))
      (is (= :noop (:engine-type caps)))
      (is (false? (:enhanced? caps)))
      ;; All operations always "available" (may be no-op)
      (is (true? (:launch? caps)))
      (is (true? (:close? caps)))
      (is (true? (:navigate? caps)))
      (is (true? (:click? caps)))
      (is (true? (:type-text? caps)))
      (is (true? (:screenshot? caps)))
      (is (true? (:get-page-source? caps)))
      (is (true? (:wait-for-selector? caps)))
      (is (true? (:evaluate-js? caps))))))

(deftest capabilities-with-noop-set-test
  (testing "capabilities with NoopBrowserAutomation set (still not enhanced)"
    (auto/set-automation! (auto/->NoopBrowserAutomation))
    (let [caps (auto/capabilities)]
      (is (= :noop (:engine-type caps)))
      (is (false? (:enhanced? caps))))))

;;; ============================================================================
;;; Enhanced? Helper Tests
;;; ============================================================================

(deftest enhanced-helper-test
  (testing "enhanced? false when nothing set"
    (is (false? (auto/enhanced?))))

  (testing "enhanced? false when NoopBrowserAutomation set"
    (auto/set-automation! (auto/->NoopBrowserAutomation))
    (is (false? (auto/enhanced?))))

  (testing "enhanced? true when custom impl set"
    (let [custom (reify auto/IBrowserAutomation
                   (launch! [_ _opts] {:success? true})
                   (close! [_] {:success? true})
                   (navigate! [_ _url _opts] {:success? true})
                   (click! [_ _sel _opts] {:success? true})
                   (type-text! [_ _sel _text _opts] {:success? true})
                   (screenshot! [_ _opts] {:success? true})
                   (get-page-source [_] {:success? true})
                   (wait-for-selector [_ _sel _opts] {:success? true})
                   (evaluate-js [_ _expr _opts] {:success? true}))]
      (auto/set-automation! custom)
      (is (true? (auto/enhanced?))))))

;;; ============================================================================
;;; Protocol Satisfaction Tests
;;; ============================================================================

(deftest protocol-satisfaction-test
  (testing "NoopBrowserAutomation satisfies IBrowserAutomation"
    (is (satisfies? auto/IBrowserAutomation (auto/->NoopBrowserAutomation))))

  (testing "NoopAutomationSession satisfies IAutomationSession"
    (is (satisfies? auto/IAutomationSession (auto/->noop-session))))

  (testing "browser-automation? helper works"
    (is (true? (auto/browser-automation? (auto/->NoopBrowserAutomation))))
    (is (false? (auto/browser-automation? {:not "automation"}))))

  (testing "automation-session? helper works"
    (is (true? (auto/automation-session? (auto/->noop-session))))
    (is (false? (auto/automation-session? {:not "session"})))))

;;; ============================================================================
;;; Custom IBrowserAutomation Implementation via reify
;;; ============================================================================

(deftest custom-automation-via-reify-test
  (testing "custom IBrowserAutomation implementation works"
    (let [state (atom {:launched? false
                       :pages []
                       :current-url nil})
          session (auto/->noop-session "custom-session")
          custom (reify auto/IBrowserAutomation
                   (launch! [_ opts]
                     (swap! state assoc :launched? true)
                     {:success? true
                      :session session
                      :errors []
                      :metadata {:browser (:browser opts :chromium)}})
                   (close! [_]
                     (swap! state assoc :launched? false)
                     {:success? true :errors []})
                   (navigate! [_ url _opts]
                     (swap! state assoc :current-url url)
                     {:success? true
                      :url url
                      :status-code 200
                      :errors []
                      :duration-ms 150})
                   (click! [_ selector _opts]
                     (swap! state update :pages conj {:action :click :selector selector})
                     {:success? true
                      :selector selector
                      :errors []})
                   (type-text! [_ selector text _opts]
                     (swap! state update :pages conj {:action :type :selector selector :text text})
                     {:success? true
                      :selector selector
                      :text text
                      :errors []})
                   (screenshot! [_ opts]
                     {:success? true
                      :path (:path opts)
                      :bytes (when-not (:path opts) (byte-array [0x89 0x50]))
                      :format (:format opts :png)
                      :errors []})
                   (get-page-source [_]
                     {:success? true
                      :html "<html><body>Test</body></html>"
                      :url (:current-url @state)
                      :errors []})
                   (wait-for-selector [_ selector _opts]
                     {:success? true
                      :selector selector
                      :found? true
                      :duration-ms 42
                      :errors []})
                   (evaluate-js [_ expression _opts]
                     {:success? true
                      :result (str "eval:" expression)
                      :errors []}))]

      ;; Test launch
      (let [result (auto/launch! custom {:browser :chromium})]
        (is (true? (:success? result)))
        (is (some? (:session result)))
        (is (= [] (:errors result)))
        (is (= :chromium (get-in result [:metadata :browser]))))
      (is (true? (:launched? @state)))

      ;; Test navigate
      (let [result (auto/navigate! custom "https://example.com" {})]
        (is (true? (:success? result)))
        (is (= "https://example.com" (:url result)))
        (is (= 200 (:status-code result)))
        (is (= 150 (:duration-ms result))))

      ;; Test click
      (let [result (auto/click! custom "#btn" {})]
        (is (true? (:success? result)))
        (is (= "#btn" (:selector result))))

      ;; Test type-text
      (let [result (auto/type-text! custom "#input" "hello" {})]
        (is (true? (:success? result)))
        (is (= "#input" (:selector result)))
        (is (= "hello" (:text result))))

      ;; Test screenshot (with path)
      (let [result (auto/screenshot! custom {:path "/tmp/test.png"})]
        (is (true? (:success? result)))
        (is (= "/tmp/test.png" (:path result)))
        (is (nil? (:bytes result))))

      ;; Test screenshot (bytes)
      (let [result (auto/screenshot! custom {})]
        (is (true? (:success? result)))
        (is (nil? (:path result)))
        (is (some? (:bytes result))))

      ;; Test get-page-source
      (let [result (auto/get-page-source custom)]
        (is (true? (:success? result)))
        (is (= "<html><body>Test</body></html>" (:html result)))
        (is (= "https://example.com" (:url result))))

      ;; Test wait-for-selector
      (let [result (auto/wait-for-selector custom ".ready" {})]
        (is (true? (:success? result)))
        (is (true? (:found? result)))
        (is (= 42 (:duration-ms result))))

      ;; Test evaluate-js
      (let [result (auto/evaluate-js custom "document.title" {})]
        (is (true? (:success? result)))
        (is (= "eval:document.title" (:result result))))

      ;; Test close
      (let [result (auto/close! custom)]
        (is (true? (:success? result))))
      (is (false? (:launched? @state)))

      ;; Verify state accumulated actions
      (is (= 2 (count (:pages @state)))))))

;;; ============================================================================
;;; Custom IAutomationSession Implementation via reify
;;; ============================================================================

(deftest custom-session-via-reify-test
  (testing "custom IAutomationSession implementation works"
    (let [active-atom (atom true)
          custom (reify auto/IAutomationSession
                   (session-id [_] "custom-sess-42")
                   (session-info [_]
                     {:id "custom-sess-42"
                      :browser :chromium
                      :headless? false
                      :created-at (java.time.Instant/parse "2026-01-01T00:00:00Z")
                      :user-agent "CustomBot/1.0"
                      :viewport {:width 1920 :height 1080}
                      :metadata {:pid 12345}})
                   (active? [_] @active-atom))]

      (is (= "custom-sess-42" (auto/session-id custom)))

      (let [info (auto/session-info custom)]
        (is (= :chromium (:browser info)))
        (is (false? (:headless? info)))
        (is (= "CustomBot/1.0" (:user-agent info)))
        (is (= {:width 1920 :height 1080} (:viewport info)))
        (is (= 12345 (get-in info [:metadata :pid]))))

      (is (true? (auto/active? custom)))

      ;; Simulate session becoming inactive
      (reset! active-atom false)
      (is (false? (auto/active? custom))))))

;;; ============================================================================
;;; Registry + Active Integration
;;; ============================================================================

(deftest registry-and-active-integration-test
  (testing "registered backend can be set as active"
    (let [a (auto/->NoopBrowserAutomation)]
      (auto/register-automation! :my-backend a)
      (auto/set-automation! (auto/get-automation :my-backend))
      (is (= a (auto/get-active-automation)))
      (is (true? (auto/automation-registered? :my-backend)))
      (is (true? (auto/automation-set?))))))

;;; ============================================================================
;;; Full Lifecycle Test
;;; ============================================================================

(deftest full-lifecycle-test
  (testing "complete automation lifecycle: register -> set-active -> launch -> navigate -> interact -> screenshot -> close -> unregister"
    (let [state (atom {:launched? false :url nil :actions []})
          session (auto/->noop-session "lifecycle-sess")
          backend (reify auto/IBrowserAutomation
                    (launch! [_ _opts]
                      (swap! state assoc :launched? true)
                      {:success? true :session session :errors [] :metadata {}})
                    (close! [_]
                      (swap! state assoc :launched? false)
                      {:success? true :errors []})
                    (navigate! [_ url _opts]
                      (swap! state assoc :url url)
                      {:success? true :url url :status-code 200 :errors [] :duration-ms 100})
                    (click! [_ sel _opts]
                      (swap! state update :actions conj {:type :click :sel sel})
                      {:success? true :selector sel :errors []})
                    (type-text! [_ sel text _opts]
                      (swap! state update :actions conj {:type :type :sel sel :text text})
                      {:success? true :selector sel :text text :errors []})
                    (screenshot! [_ _opts]
                      {:success? true :path nil :bytes (byte-array 10) :format :png :errors []})
                    (get-page-source [_]
                      {:success? true :html "<html>lifecycle</html>" :url (:url @state) :errors []})
                    (wait-for-selector [_ sel _opts]
                      {:success? true :selector sel :found? true :duration-ms 5 :errors []})
                    (evaluate-js [_ expr _opts]
                      {:success? true :result 42 :errors []}))]

      ;; 1. Register
      (auto/register-automation! :lifecycle-backend backend)
      (is (true? (auto/automation-registered? :lifecycle-backend)))

      ;; 2. Set as active
      (auto/set-automation! (auto/get-automation :lifecycle-backend))
      (is (true? (auto/automation-set?)))
      (is (true? (auto/enhanced?)))

      ;; 3. Capabilities show enhanced
      (let [caps (auto/capabilities)]
        (is (true? (:enhanced? caps)))
        (is (not= :noop (:engine-type caps))))

      ;; 4. Launch
      (let [result (auto/launch! backend {})]
        (is (true? (:success? result)))
        (is (some? (:session result))))
      (is (true? (:launched? @state)))

      ;; 5. Navigate
      (let [result (auto/navigate! backend "https://example.com/login" {})]
        (is (true? (:success? result)))
        (is (= 200 (:status-code result))))

      ;; 6. Wait for page
      (let [result (auto/wait-for-selector backend "#login-form" {})]
        (is (true? (:found? result))))

      ;; 7. Type credentials
      (let [result (auto/type-text! backend "#username" "admin" {})]
        (is (true? (:success? result))))

      ;; 8. Click submit
      (let [result (auto/click! backend "#submit" {})]
        (is (true? (:success? result))))

      ;; 9. Get page source
      (let [result (auto/get-page-source backend)]
        (is (true? (:success? result)))
        (is (string? (:html result))))

      ;; 10. Screenshot
      (let [result (auto/screenshot! backend {:format :png})]
        (is (true? (:success? result)))
        (is (some? (:bytes result))))

      ;; 11. Evaluate JS
      (let [result (auto/evaluate-js backend "document.querySelectorAll('a').length" {})]
        (is (true? (:success? result)))
        (is (= 42 (:result result))))

      ;; 12. Close
      (auto/close! backend)
      (is (false? (:launched? @state)))

      ;; 13. Verify actions accumulated
      (is (= 2 (count (:actions @state))))

      ;; 14. Unregister
      (auto/clear-automation!)
      (is (true? (auto/unregister-automation! :lifecycle-backend)))
      (is (false? (auto/automation-registered? :lifecycle-backend))))))
