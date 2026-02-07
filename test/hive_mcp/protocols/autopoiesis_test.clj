(ns hive-mcp.protocols.autopoiesis-test
  "Tests for IAutopoiesis and IIntrospect protocols.

   Validates:
   - Protocol method signatures and return shapes
   - BasicAutopoiesis fallback behavior (all 9 methods)
   - BasicIntrospect fallback behavior (all 6 methods)
   - Implementation management (set/get/clear)
   - Capabilities reporting
   - Custom implementation via reify"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.autopoiesis :as ap]))

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

(defn clean-implementations [f]
  (ap/clear-implementations!)
  (f)
  (ap/clear-implementations!))

(use-fixtures :each clean-implementations)

;;; ============================================================================
;;; BasicAutopoiesis Tests
;;; ============================================================================

(deftest basic-autopoiesis-observe-test
  (testing "observe returns empty results with enhanced?=false"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/observe basic "test query" {:scope "test"})]
      (is (map? result))
      (is (= [] (:entries result)))
      (is (= [] (:gaps result)))
      (is (= {} (:staleness result)))
      (is (= [] (:suggestions result)))
      (is (false? (:enhanced? result)))
      (is (string? (:message result))))))

(deftest basic-autopoiesis-trust-test
  (testing "trust? returns trustworthy with 0.5 confidence"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/trust? basic {:id "test-entry" :content "test"})]
      (is (map? result))
      (is (true? (:trustworthy? result)))
      (is (= 0.5 (:confidence result)))
      (is (= :use (:action result)))
      (is (vector? (:reasons result)))
      (is (false? (:enhanced? result))))))

(deftest basic-autopoiesis-emerge-test
  (testing "emerge returns no patterns"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/emerge basic "test-scope")]
      (is (map? result))
      (is (= [] (:patterns result)))
      (is (= [] (:clusters result)))
      (is (false? (:novel? result)))
      (is (false? (:enhanced? result))))))

(deftest basic-autopoiesis-cross-pollinate-test
  (testing "cross-pollinate returns no analogues"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/cross-pollinate basic "entry-123")]
      (is (map? result))
      (is (= [] (:analogues result)))
      (is (false? (:transferable? result)))
      (is (= [] (:adaptations result)))
      (is (false? (:enhanced? result))))))

(deftest basic-autopoiesis-learn-test
  (testing "learn returns empty results"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/learn basic {:files-read ["test.clj"]
                                  :patterns-found []
                                  :context "test"
                                  :agent-id "test-agent"})]
      (is (map? result))
      (is (= [] (:entries-created result)))
      (is (= [] (:entries-updated result)))
      (is (= [] (:gaps-filled result)))
      (is (= [] (:new-gaps result)))
      (is (false? (:enhanced? result))))))

(deftest basic-autopoiesis-decay-test
  (testing "decay returns zero counts"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/decay basic)]
      (is (map? result))
      (is (= 0 (:decayed result)))
      (is (= 0 (:pruned result)))
      (is (= 0 (:promoted result)))
      (is (= 0 (:demoted result)))
      (is (false? (:enhanced? result))))))

;;; --- New IAutopoiesis Methods ---

(deftest basic-autopoiesis-self-modify-test
  (testing "self-modify! returns not-applied with timestamp"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/self-modify! basic {:parameter :decay-threshold
                                         :old-value 0.7
                                         :new-value 0.8
                                         :reason "test modification"
                                         :evidence {:metric 42}})]
      (is (map? result))
      (is (false? (:applied? result)))
      (is (nil? (:parameter result)))
      (is (nil? (:old-value result)))
      (is (nil? (:new-value result)))
      (is (instance? java.time.Instant (:timestamp result)))
      (is (nil? (:rollback-fn result)))
      (is (false? (:enhanced? result)))
      (is (string? (:message result))))))

(deftest basic-autopoiesis-adaptation-history-test
  (testing "adaptation-history returns empty history"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/adaptation-history basic {:limit 10})]
      (is (map? result))
      (is (= [] (:modifications result)))
      (is (= 0 (:count result)))
      (is (= {} (:active-params result)))
      (is (false? (:enhanced? result)))
      (is (string? (:message result)))))

  (testing "adaptation-history with parameter filter"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/adaptation-history basic {:parameter :decay-threshold
                                               :limit 5})]
      (is (= [] (:modifications result)))
      (is (= 0 (:count result))))))

(deftest basic-autopoiesis-adaptation-score-test
  (testing "adaptation-score returns zero/neutral score"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/adaptation-score basic)]
      (is (map? result))
      (is (= 0.0 (:score result)))
      (is (= :stable (:trend result)))
      (is (= 0 (:recent-mods result)))
      (is (= 0 (:effective-mods result)))
      (is (= 0 (:ineffective-mods result)))
      (is (= [] (:recommendations result)))
      (is (= 0.0 (:confidence result)))
      (is (false? (:enhanced? result)))
      (is (string? (:message result))))))

;;; ============================================================================
;;; BasicIntrospect Tests
;;; ============================================================================

(deftest basic-introspect-explain-test
  (testing "explain returns minimal explanation with entry-id in narrative"
    (let [basic (ap/->BasicIntrospect)
          result (ap/explain basic "entry-456")]
      (is (map? result))
      (is (= :unknown (:origin result)))
      (is (= [] (:evolution result)))
      (is (= [] (:influences result)))
      (is (= [] (:influenced result)))
      (is (= [] (:trust-chain result)))
      (is (string? (:narrative result)))
      (is (.contains (:narrative result) "entry-456"))
      (is (false? (:enhanced? result))))))

(deftest basic-introspect-trace-test
  (testing "trace returns empty trace"
    (let [basic (ap/->BasicIntrospect)
          result (ap/trace basic "test query" {:entries []})]
      (is (map? result))
      (is (= [] (:steps result)))
      (is (= [] (:filters result)))
      (is (= {} (:scores result)))
      (is (= [] (:alternatives result)))
      (is (string? (:narrative result)))
      (is (false? (:enhanced? result))))))

(deftest basic-introspect-diff-test
  (testing "diff returns minimal comparison with entry IDs in narrative"
    (let [basic (ap/->BasicIntrospect)
          result (ap/diff basic "entry-a" "entry-b")]
      (is (map? result))
      (is (nil? (:content-diff result)))
      (is (nil? (:metadata-diff result)))
      (is (nil? (:evolution-diff result)))
      (is (nil? (:semantic-diff result)))
      (is (= :unknown (:relationship result)))
      (is (.contains (:narrative result) "entry-a"))
      (is (.contains (:narrative result) "entry-b"))
      (is (false? (:enhanced? result))))))

;;; --- New IIntrospect Methods ---

(deftest basic-introspect-observe-state-test
  (testing "observe-state returns minimal state snapshot"
    (let [basic (ap/->BasicIntrospect)
          result (ap/observe-state basic)]
      (is (map? result))
      (is (= 0 (:memory-count result)))
      (is (= 0 (:kg-node-count result)))
      (is (= 0 (:kg-edge-count result)))
      (is (= #{} (:active-scopes result)))
      (is (= 0 (:uptime-ms result)))
      (is (nil? (:last-decay result)))
      (is (nil? (:last-emergence result)))
      (is (= 0 (:pending-ops result)))
      (is (false? (:enhanced? result)))
      (is (string? (:message result))))))

(deftest basic-introspect-diagnose-test
  (testing "diagnose returns unavailable diagnosis"
    (let [basic (ap/->BasicIntrospect)
          result (ap/diagnose basic {:type :slow-query
                                     :context "Queries taking >5s"
                                     :severity :medium})]
      (is (map? result))
      (is (= :unavailable (:diagnosis result)))
      (is (string? (:explanation result)))
      (is (= [] (:contributing result)))
      (is (= [] (:recommended result)))
      (is (= [] (:related-entries result)))
      (is (false? (:enhanced? result))))))

(deftest basic-introspect-health-report-test
  (testing "health-report returns minimal report with nested structures"
    (let [basic (ap/->BasicIntrospect)
          result (ap/health-report basic)]
      (is (map? result))
      (is (= :unknown (:status result)))
      (is (= [] (:checks result)))
      ;; memory-health sub-map
      (is (map? (:memory-health result)))
      (is (= 0 (get-in result [:memory-health :total])))
      (is (= 0 (get-in result [:memory-health :stale-count])))
      (is (= 0.0 (get-in result [:memory-health :stale-pct])))
      (is (= 0.0 (get-in result [:memory-health :avg-staleness])))
      (is (= 0 (get-in result [:memory-health :gap-count])))
      ;; kg-health sub-map
      (is (map? (:kg-health result)))
      (is (= 0 (get-in result [:kg-health :nodes])))
      (is (= 0 (get-in result [:kg-health :edges])))
      (is (= 0 (get-in result [:kg-health :orphans])))
      (is (= 0.0 (get-in result [:kg-health :avg-connectivity])))
      (is (= 0 (get-in result [:kg-health :largest-component])))
      ;; adaptation sub-map
      (is (map? (:adaptation result)))
      (is (= 0.0 (get-in result [:adaptation :score])))
      (is (= :stable (get-in result [:adaptation :trend])))
      (is (= 0 (get-in result [:adaptation :recent-mods])))
      ;; metadata
      (is (= [] (:recommendations result)))
      (is (instance? java.time.Instant (:generated-at result)))
      (is (false? (:enhanced? result)))
      (is (string? (:message result))))))

;;; ============================================================================
;;; Implementation Management Tests
;;; ============================================================================

(deftest set-get-autopoiesis-test
  (testing "get-autopoiesis returns BasicAutopoiesis when nothing set"
    (let [impl (ap/get-autopoiesis)]
      (is (instance? hive_mcp.protocols.autopoiesis.BasicAutopoiesis impl))))

  (testing "set-autopoiesis! stores and returns implementation"
    (let [basic (ap/->BasicAutopoiesis)
          result (ap/set-autopoiesis! basic)]
      (is (= basic result))
      (is (= basic (ap/get-autopoiesis)))))

  (testing "autopoiesis-set? reflects state"
    (ap/clear-implementations!)
    (is (false? (ap/autopoiesis-set?)))
    (ap/set-autopoiesis! (ap/->BasicAutopoiesis))
    (is (true? (ap/autopoiesis-set?)))))

(deftest set-get-introspect-test
  (testing "get-introspect returns BasicIntrospect when nothing set"
    (let [impl (ap/get-introspect)]
      (is (instance? hive_mcp.protocols.autopoiesis.BasicIntrospect impl))))

  (testing "set-introspect! stores and returns implementation"
    (let [basic (ap/->BasicIntrospect)
          result (ap/set-introspect! basic)]
      (is (= basic result))
      (is (= basic (ap/get-introspect)))))

  (testing "introspect-set? reflects state"
    (ap/clear-implementations!)
    (is (false? (ap/introspect-set?)))
    (ap/set-introspect! (ap/->BasicIntrospect))
    (is (true? (ap/introspect-set?)))))

(deftest clear-implementations-test
  (testing "clear-implementations! resets both"
    (ap/set-autopoiesis! (ap/->BasicAutopoiesis))
    (ap/set-introspect! (ap/->BasicIntrospect))
    (is (true? (ap/autopoiesis-set?)))
    (is (true? (ap/introspect-set?)))
    (ap/clear-implementations!)
    (is (false? (ap/autopoiesis-set?)))
    (is (false? (ap/introspect-set?)))))

;;; ============================================================================
;;; Capabilities Tests
;;; ============================================================================

(deftest capabilities-no-enhanced-test
  (testing "capabilities with no enhanced implementations"
    (let [caps (ap/capabilities)]
      (is (map? caps))
      ;; Not enhanced
      (is (false? (:autopoiesis-enhanced? caps)))
      (is (false? (:introspect-enhanced? caps)))
      ;; Always-available methods
      (is (true? (:observe? caps)))
      (is (true? (:trust? caps)))
      (is (true? (:explain? caps)))
      (is (true? (:observe-state? caps)))
      (is (true? (:health-report? caps)))
      ;; Enhanced-only methods
      (is (false? (:emerge? caps)))
      (is (false? (:cross-pollinate? caps)))
      (is (false? (:learn? caps)))
      (is (false? (:decay? caps)))
      (is (false? (:self-modify? caps)))
      (is (false? (:adaptation-history? caps)))
      (is (false? (:adaptation-score? caps)))
      (is (false? (:diagnose? caps))))))

(deftest capabilities-with-basic-set-test
  (testing "capabilities with Basic* implementations set (still not truly enhanced)"
    (ap/set-autopoiesis! (ap/->BasicAutopoiesis))
    (ap/set-introspect! (ap/->BasicIntrospect))
    (let [caps (ap/capabilities)]
      ;; BasicAutopoiesis/BasicIntrospect are explicitly not "enhanced"
      (is (false? (:autopoiesis-enhanced? caps)))
      (is (false? (:introspect-enhanced? caps)))
      ;; But introspect-set? is true, so trace/diff become available
      (is (true? (:trace? caps)))
      (is (true? (:diff? caps))))))

;;; ============================================================================
;;; Custom Implementation via reify
;;; ============================================================================

(deftest custom-autopoiesis-implementation-test
  (testing "custom IAutopoiesis implementation via reify works"
    (let [custom (reify ap/IAutopoiesis
                   (observe [_ _q _opts] {:entries [{:id "custom"}] :enhanced? true})
                   (trust? [_ _entry] {:trustworthy? false :confidence 0.9 :enhanced? true})
                   (emerge [_ _scope] {:patterns [{:id "p1"}] :novel? true :enhanced? true})
                   (cross-pollinate [_ _eid] {:analogues [] :enhanced? true})
                   (learn [_ _er] {:entries-created ["e1"] :enhanced? true})
                   (decay [_] {:decayed 5 :pruned 2 :enhanced? true})
                   (self-modify! [_ mod] {:applied? true
                                          :parameter (:parameter mod)
                                          :new-value (:new-value mod)
                                          :enhanced? true})
                   (adaptation-history [_ _opts] {:modifications [{:id "m1"}]
                                                  :count 1
                                                  :enhanced? true})
                   (adaptation-score [_] {:score 0.85
                                          :trend :improving
                                          :enhanced? true}))]
      ;; Can be set as active implementation
      (ap/set-autopoiesis! custom)
      (is (= custom (ap/get-autopoiesis)))

      ;; observe
      (let [r (ap/observe custom "q" {})]
        (is (true? (:enhanced? r)))
        (is (= [{:id "custom"}] (:entries r))))

      ;; self-modify!
      (let [r (ap/self-modify! custom {:parameter :decay-threshold
                                       :new-value 0.9})]
        (is (true? (:applied? r)))
        (is (= :decay-threshold (:parameter r)))
        (is (= 0.9 (:new-value r))))

      ;; adaptation-history
      (let [r (ap/adaptation-history custom {})]
        (is (= 1 (:count r)))
        (is (= [{:id "m1"}] (:modifications r))))

      ;; adaptation-score
      (let [r (ap/adaptation-score custom)]
        (is (= 0.85 (:score r)))
        (is (= :improving (:trend r)))))))

(deftest custom-introspect-implementation-test
  (testing "custom IIntrospect implementation via reify works"
    (let [custom (reify ap/IIntrospect
                   (explain [_ eid] {:origin :learned :narrative (str "Custom: " eid) :enhanced? true})
                   (trace [_ _q _r] {:steps [{:op :match}] :enhanced? true})
                   (diff [_ a b] {:relationship :refines :narrative (str a " refines " b) :enhanced? true})
                   (observe-state [_] {:memory-count 42
                                       :kg-node-count 100
                                       :active-scopes #{"proj-a" "proj-b"}
                                       :enhanced? true})
                   (diagnose [_ symptom] {:diagnosis (:type symptom)
                                          :explanation "Custom diagnosis"
                                          :contributing ["factor-1"]
                                          :enhanced? true})
                   (health-report [_] {:status :healthy
                                       :checks [{:name "mem" :status :pass}]
                                       :enhanced? true}))]
      ;; Can be set
      (ap/set-introspect! custom)
      (is (= custom (ap/get-introspect)))

      ;; observe-state
      (let [r (ap/observe-state custom)]
        (is (true? (:enhanced? r)))
        (is (= 42 (:memory-count r)))
        (is (= 100 (:kg-node-count r)))
        (is (= #{"proj-a" "proj-b"} (:active-scopes r))))

      ;; diagnose
      (let [r (ap/diagnose custom {:type :slow-query :severity :high})]
        (is (= :slow-query (:diagnosis r)))
        (is (= ["factor-1"] (:contributing r))))

      ;; health-report
      (let [r (ap/health-report custom)]
        (is (= :healthy (:status r)))
        (is (= 1 (count (:checks r))))))))

;;; ============================================================================
;;; Protocol Satisfaction Tests
;;; ============================================================================

(deftest protocol-satisfaction-test
  (testing "BasicAutopoiesis satisfies IAutopoiesis"
    (is (satisfies? ap/IAutopoiesis (ap/->BasicAutopoiesis))))

  (testing "BasicIntrospect satisfies IIntrospect"
    (is (satisfies? ap/IIntrospect (ap/->BasicIntrospect))))

  (testing "set-autopoiesis! rejects non-satisfying impl"
    (is (thrown? AssertionError
                 (ap/set-autopoiesis! {:not "a protocol impl"}))))

  (testing "set-introspect! rejects non-satisfying impl"
    (is (thrown? AssertionError
                 (ap/set-introspect! {:not "a protocol impl"})))))

;;; ============================================================================
;;; Enhanced? Helper Tests
;;; ============================================================================

(deftest enhanced-helper-test
  (testing "enhanced? false when nothing set"
    (is (false? (ap/enhanced?))))

  (testing "enhanced? false when BasicAutopoiesis set"
    (ap/set-autopoiesis! (ap/->BasicAutopoiesis))
    (is (false? (ap/enhanced?))))

  (testing "enhanced? true when custom impl set"
    (let [custom (reify ap/IAutopoiesis
                   (observe [_ _q _opts] {})
                   (trust? [_ _e] {})
                   (emerge [_ _s] {})
                   (cross-pollinate [_ _eid] {})
                   (learn [_ _er] {})
                   (decay [_] {})
                   (self-modify! [_ _m] {})
                   (adaptation-history [_ _opts] {})
                   (adaptation-score [_] {}))]
      (ap/set-autopoiesis! custom)
      (is (true? (ap/enhanced?))))))
