(ns hive-mcp.tools.multi-test
  "Tests for hive-mcp.tools.multi — cross-tool batch multiplexer.

   Tests cover:
   1. validate-ops — required fields, duplicate IDs, dependency refs, circular deps
   2. assign-waves — topological wave assignment
   3. execute-op — single operation execution with error isolation
   4. run-multi — full pipeline: validate → assign-waves → execute-per-wave
   5. format-results — MCP response formatting
   6. resolve-tool-handler — tool resolution via requiring-resolve"
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [hive-mcp.tools.multi :as multi]))

;; =============================================================================
;; Part 1: validate-ops Tests
;; =============================================================================

(deftest validate-ops-valid-basic-test
  (testing "Simple valid ops pass validation"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add"}
                   {:id "b" :tool "kg" :command "stats"}])]
      (is (:valid result))
      (is (nil? (:errors result))))))

(deftest validate-ops-valid-with-deps-test
  (testing "Valid ops with dependencies pass validation"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add"}
                   {:id "b" :tool "kg" :command "stats" :depends_on ["a"]}
                   {:id "c" :tool "preset" :command "list" :depends_on ["a" "b"]}])]
      (is (:valid result)))))

(deftest validate-ops-empty-vector-test
  (testing "Empty ops vector passes validation (no ops to check)"
    (let [result (multi/validate-ops [])]
      (is (:valid result)))))

(deftest validate-ops-missing-id-test
  (testing "Op without :id fails validation"
    (let [result (multi/validate-ops
                  [{:tool "memory" :command "add"}])]
      (is (not (:valid result)))
      (is (some #(str/includes? % "missing :id") (:errors result))))))

(deftest validate-ops-blank-id-test
  (testing "Op with blank :id fails validation"
    (let [result (multi/validate-ops
                  [{:id "" :tool "memory" :command "add"}])]
      (is (not (:valid result)))
      (is (some #(str/includes? % "missing :id") (:errors result))))))

(deftest validate-ops-missing-tool-test
  (testing "Op without :tool fails validation"
    (let [result (multi/validate-ops
                  [{:id "a" :command "add"}])]
      (is (not (:valid result)))
      (is (some #(str/includes? % "missing :tool") (:errors result))))))

(deftest validate-ops-blank-tool-test
  (testing "Op with blank :tool fails validation"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "" :command "add"}])]
      (is (not (:valid result)))
      (is (some #(str/includes? % "missing :tool") (:errors result))))))

(deftest validate-ops-duplicate-ids-test
  (testing "Duplicate IDs fail validation"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add"}
                   {:id "a" :tool "kg" :command "stats"}])]
      (is (not (:valid result)))
      (is (some #(str/includes? % "Duplicate") (:errors result))))))

(deftest validate-ops-nonexistent-dependency-test
  (testing "Reference to non-existent dependency fails"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add" :depends_on ["z"]}])]
      (is (not (:valid result)))
      (is (some #(str/includes? % "non-existent") (:errors result))))))

(deftest validate-ops-self-dependency-test
  (testing "Self-dependency fails validation"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add" :depends_on ["a"]}])]
      (is (not (:valid result)))
      (is (some #(str/includes? % "depends on itself") (:errors result))))))

(deftest validate-ops-circular-dependency-test
  (testing "Circular dependency detected"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add" :depends_on ["b"]}
                   {:id "b" :tool "kg" :command "stats" :depends_on ["a"]}])]
      (is (not (:valid result)))
      (is (some #(str/includes? % "Circular") (:errors result))))))

(deftest validate-ops-three-node-cycle-test
  (testing "Three-node circular dependency detected"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add" :depends_on ["c"]}
                   {:id "b" :tool "kg" :command "stats" :depends_on ["a"]}
                   {:id "c" :tool "preset" :command "list" :depends_on ["b"]}])]
      (is (not (:valid result)))
      (is (some #(str/includes? % "Circular") (:errors result))))))

(deftest validate-ops-multiple-errors-test
  (testing "Multiple errors collected simultaneously"
    (let [result (multi/validate-ops
                  [{:id "" :tool "" :command "add"}
                   {:id "a" :tool "memory" :command "add"}
                   {:id "a" :tool "kg" :command "stats"}])]
      (is (not (:valid result)))
      ;; Should have at least: missing id, missing tool, duplicate id
      (is (>= (count (:errors result)) 2)))))

(deftest validate-ops-no-deps-key-test
  (testing "Ops without depends_on field are fine"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add"}
                   {:id "b" :tool "kg" :command "stats"}])]
      (is (:valid result)))))

(deftest validate-ops-empty-deps-test
  (testing "Ops with empty depends_on vector are fine"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add" :depends_on []}
                   {:id "b" :tool "kg" :command "stats" :depends_on []}])]
      (is (:valid result)))))

;; =============================================================================
;; Part 2: assign-waves Tests
;; =============================================================================

(deftest assign-waves-independent-ops-test
  (testing "Independent ops all in wave 1"
    (let [ops [{:id "a" :tool "memory"}
               {:id "b" :tool "kg"}
               {:id "c" :tool "preset"}]
          result (multi/assign-waves ops)]
      (is (= 3 (count result)))
      (is (every? #(= 1 (:wave %)) result)))))

(deftest assign-waves-linear-chain-test
  (testing "Linear dependency chain gets sequential waves"
    (let [ops [{:id "a" :tool "memory"}
               {:id "b" :tool "kg" :depends_on ["a"]}
               {:id "c" :tool "preset" :depends_on ["b"]}]
          result (multi/assign-waves ops)]
      (is (= 3 (count result)))
      (let [by-id (into {} (map (juxt :id identity) result))]
        (is (= 1 (:wave (get by-id "a"))))
        (is (= 2 (:wave (get by-id "b"))))
        (is (= 3 (:wave (get by-id "c"))))))))

(deftest assign-waves-diamond-test
  (testing "Diamond dependency pattern — parallel middle, sequential top/bottom"
    ;; a → b, a → c, b → d, c → d
    (let [ops [{:id "a" :tool "memory"}
               {:id "b" :tool "kg" :depends_on ["a"]}
               {:id "c" :tool "preset" :depends_on ["a"]}
               {:id "d" :tool "agora" :depends_on ["b" "c"]}]
          result (multi/assign-waves ops)
          by-id (into {} (map (juxt :id identity) result))]
      (is (= 1 (:wave (get by-id "a"))))
      ;; b and c should be in the same wave (both depend only on a)
      (is (= (:wave (get by-id "b")) (:wave (get by-id "c"))))
      ;; d should be in a later wave
      (is (> (:wave (get by-id "d")) (:wave (get by-id "b")))))))

(deftest assign-waves-empty-ops-test
  (testing "Empty ops returns empty result"
    (is (= [] (multi/assign-waves [])))))

(deftest assign-waves-single-op-test
  (testing "Single op gets wave 1"
    (let [result (multi/assign-waves [{:id "a" :tool "memory"}])]
      (is (= 1 (count result)))
      (is (= 1 (:wave (first result)))))))

(deftest assign-waves-preserves-op-data-test
  (testing "Wave assignment preserves all original op keys"
    (let [ops [{:id "a" :tool "memory" :command "add" :content "hello"}]
          result (multi/assign-waves ops)]
      (is (= "a" (:id (first result))))
      (is (= "memory" (:tool (first result))))
      (is (= "add" (:command (first result))))
      (is (= "hello" (:content (first result))))
      (is (= 1 (:wave (first result)))))))

(deftest assign-waves-multi-root-test
  (testing "Multiple root ops + one dependent"
    (let [ops [{:id "a" :tool "memory"}
               {:id "b" :tool "kg"}
               {:id "c" :tool "preset" :depends_on ["a" "b"]}]
          result (multi/assign-waves ops)
          by-id (into {} (map (juxt :id identity) result))]
      (is (= 1 (:wave (get by-id "a"))))
      (is (= 1 (:wave (get by-id "b"))))
      (is (= 2 (:wave (get by-id "c")))))))

;; =============================================================================
;; Part 3: execute-op Tests
;; =============================================================================

(deftest execute-op-unknown-tool-test
  (testing "Unknown tool returns error result"
    (let [result (multi/execute-op {:id "x" :tool "nonexistent-tool-xyz" :command "noop"})]
      (is (= "x" (:id result)))
      (is (false? (:success result)))
      (is (str/includes? (:error result) "not found")))))

(deftest execute-op-result-structure-test
  (testing "execute-op always returns :id and :success"
    (let [result (multi/execute-op {:id "test-1" :tool "nonexistent" :command "x"})]
      (is (contains? result :id))
      (is (contains? result :success)))))

;; =============================================================================
;; Part 4: run-multi Tests (integration)
;; =============================================================================

(deftest run-multi-validation-failure-test
  (testing "run-multi returns error on invalid ops"
    (let [result (multi/run-multi [{:id "" :tool ""}])]
      (is (false? (:success result)))
      (is (seq (:errors result)))
      (is (= 0 (get-in result [:summary :success]))))))

(deftest run-multi-dry-run-test
  (testing "run-multi dry-run returns plan without executing"
    (let [result (multi/run-multi
                  [{:id "a" :tool "memory" :command "help"}
                   {:id "b" :tool "kg" :command "help" :depends_on ["a"]}]
                  :dry-run true)]
      (is (:success result))
      (is (true? (:dry-run result)))
      ;; Should have wave plan
      (is (some? (:waves result)))
      ;; Summary should show 0 success/0 failed (no execution)
      (is (= 0 (get-in result [:summary :success])))
      (is (= 0 (get-in result [:summary :failed])))
      (is (> (get-in result [:summary :waves]) 0)))))

(deftest run-multi-dry-run-wave-structure-test
  (testing "dry-run waves have ops with expected keys"
    (let [result (multi/run-multi
                  [{:id "a" :tool "memory" :command "help"}
                   {:id "b" :tool "kg" :command "help"}]
                  :dry-run true)
          wave-1 (get-in result [:waves 1])]
      (is (some? wave-1))
      (is (vector? (:ops wave-1)))
      ;; Each op in plan should have at least :id :tool :command
      (doseq [op (:ops wave-1)]
        (is (contains? op :id))
        (is (contains? op :tool))
        (is (contains? op :command))))))

(deftest run-multi-empty-ops-test
  (testing "run-multi with empty ops passes validation (0 ops is trivially valid)"
    (let [result (multi/run-multi [])]
      ;; Empty ops should pass validation and return success with 0 ops
      (is (:success result))
      (is (= 0 (get-in result [:summary :total]))))))

(deftest run-multi-summary-structure-test
  (testing "run-multi summary always has :total :success :failed :waves"
    (let [result (multi/run-multi [{:id "a" :tool "memory" :command "help"}])]
      (is (contains? (:summary result) :total))
      (is (contains? (:summary result) :success))
      (is (contains? (:summary result) :failed))
      (is (contains? (:summary result) :waves)))))

(deftest run-multi-circular-dep-rejects-test
  (testing "run-multi rejects circular deps in validation phase"
    (let [result (multi/run-multi
                  [{:id "a" :tool "memory" :command "add" :depends_on ["b"]}
                   {:id "b" :tool "kg" :command "stats" :depends_on ["a"]}])]
      (is (false? (:success result)))
      (is (seq (:errors result))))))

;; =============================================================================
;; Part 5: format-results Tests
;; =============================================================================

(deftest format-results-dry-run-test
  (testing "format-results for dry-run includes plan"
    (let [input {:success true
                 :dry-run true
                 :waves {1 {:ops [{:id "a" :tool "memory" :command "add"}]}}
                 :summary {:total 1 :success 0 :failed 0 :waves 1}}
          result (multi/format-results input)]
      (is (= "text" (:type result)))
      (let [parsed (json/read-str (:text result) :key-fn keyword)]
        (is (:success parsed))
        (is (true? (:dry_run parsed)))
        (is (some? (:plan parsed)))))))

(deftest format-results-execution-test
  (testing "format-results for execution includes wave results"
    (let [input {:success true
                 :waves {1 {:ops [{:id "a" :tool "memory" :command "add"}]
                            :results [{:id "a" :success true :result {:type "text" :text "ok"}}]}}
                 :summary {:total 1 :success 1 :failed 0 :waves 1}}
          result (multi/format-results input)]
      (is (= "text" (:type result)))
      (let [parsed (json/read-str (:text result) :key-fn keyword)]
        (is (:success parsed))
        (is (nil? (:dry_run parsed)))
        (is (some? (:waves parsed)))))))

(deftest format-results-with-errors-test
  (testing "format-results includes errors when present"
    (let [input {:success false
                 :errors ["Op missing :id"]
                 :summary {:total 1 :success 0 :failed 0 :waves 0}}
          result (multi/format-results input)]
      (let [parsed (json/read-str (:text result) :key-fn keyword)]
        (is (false? (:success parsed)))
        (is (seq (:errors parsed)))))))

(deftest format-results-summary-preserved-test
  (testing "format-results preserves summary fields"
    (let [input {:success true
                 :waves {}
                 :summary {:total 5 :success 3 :failed 2 :waves 2}}
          result (multi/format-results input)]
      (let [parsed (json/read-str (:text result) :key-fn keyword)]
        (is (= 5 (get-in parsed [:summary :total])))
        (is (= 3 (get-in parsed [:summary :success])))
        (is (= 2 (get-in parsed [:summary :failed])))
        (is (= 2 (get-in parsed [:summary :waves])))))))

(deftest format-results-string-result-test
  (testing "format-results handles string result values"
    (let [input {:success true
                 :waves {1 {:ops [{:id "a" :tool "memory" :command "add"}]
                            :results [{:id "a" :success true :result "plain-text"}]}}
                 :summary {:total 1 :success 1 :failed 0 :waves 1}}
          result (multi/format-results input)]
      (let [parsed (json/read-str (:text result) :key-fn keyword)]
        (is (:success parsed))))))

(deftest format-results-error-op-test
  (testing "format-results includes error for failed ops"
    (let [input {:success false
                 :waves {1 {:ops [{:id "a" :tool "bad" :command "x"}]
                            :results [{:id "a" :success false :error "Tool not found: bad"}]}}
                 :summary {:total 1 :success 0 :failed 1 :waves 1}}
          result (multi/format-results input)]
      (let [parsed (json/read-str (:text result) :key-fn keyword)]
        (is (false? (:success parsed)))
        (let [wave-results (get-in parsed [:waves :wave_1])]
          (is (= 1 (count wave-results)))
          (is (false? (:success (first wave-results))))
          (is (some? (:error (first wave-results)))))))))

;; =============================================================================
;; Part 6: resolve-tool-handler Tests
;; =============================================================================

(deftest resolve-tool-handler-unknown-tool-test
  (testing "Resolving unknown tool returns nil"
    (is (nil? (multi/resolve-tool-handler "completely-nonexistent-tool-name")))))

;; =============================================================================
;; Part 7: Edge Cases
;; =============================================================================

(deftest validate-ops-nil-depends-on-test
  (testing "nil depends_on is treated as no dependencies"
    (let [result (multi/validate-ops
                  [{:id "a" :tool "memory" :command "add" :depends_on nil}])]
      (is (:valid result)))))

(deftest assign-waves-preserves-depends-on-test
  (testing "assign-waves preserves :depends_on in output"
    (let [result (multi/assign-waves
                  [{:id "a" :tool "memory"}
                   {:id "b" :tool "kg" :depends_on ["a"]}])]
      (let [b (first (filter #(= "b" (:id %)) result))]
        (is (= ["a"] (:depends_on b)))))))

(deftest run-multi-dep-failure-skips-downstream-test
  (testing "run-multi skips ops whose dependencies failed"
    ;; Use a tool that won't exist to guarantee failure
    (let [result (multi/run-multi
                  [{:id "a" :tool "nonexistent-tool-xyz" :command "noop"}
                   {:id "b" :tool "memory" :command "help" :depends_on ["a"]}])]
      ;; a should fail (tool not found)
      ;; b should be skipped (dependency a failed)
      (is (false? (:success result)))
      (is (= 2 (get-in result [:summary :total])))
      (is (= 0 (get-in result [:summary :success])))
      (is (= 2 (get-in result [:summary :failed]))))))

;; =============================================================================
;; Part 8: FX Effect Registration Tests (Step 4)
;; =============================================================================

(deftest fx-handlers-registered-test
  (testing ":multi/wave-complete and :multi/op-error are registered in hive.events.fx"
    (require 'hive.events.fx)
    (let [get-fx (resolve 'hive.events.fx/get-fx)]
      (is (fn? (get-fx :multi/wave-complete))
          ":multi/wave-complete should be registered as an FX handler")
      (is (fn? (get-fx :multi/op-error))
          ":multi/op-error should be registered as an FX handler"))))

(deftest register-fx-idempotent-test
  (testing "register-fx! can be called multiple times without error"
    (is (true? (multi/register-fx!)))
    (is (true? (multi/register-fx!)))))

(deftest fx-wave-complete-emitted-on-execution-test
  (testing "run-multi emits :multi/wave-complete FX for each wave"
    (require 'hive.events.fx)
    (let [wave-calls (atom [])
          reg-fx (resolve 'hive.events.fx/reg-fx)]
      ;; Install capturing handler
      (reg-fx :multi/wave-complete (fn [data] (swap! wave-calls conj data)))
      (try
        ;; 2 independent ops = 1 wave
        (multi/run-multi
         [{:id "a" :tool "fake-tool-xyz" :command "noop"}
          {:id "b" :tool "fake-tool-xyz" :command "noop"}])
        (is (= 1 (count @wave-calls))
            "Should emit exactly 1 wave-complete for 1 wave")
        (let [wc (first @wave-calls)]
          (is (= 1 (:wave-num wc)))
          (is (= 2 (:op-count wc)))
          (is (= 1 (:total-waves wc))))
        (finally
          ;; Restore real handler
          (multi/register-fx!))))))

(deftest fx-wave-complete-multi-wave-test
  (testing "run-multi emits :multi/wave-complete for each wave in multi-wave execution"
    (require 'hive.events.fx)
    (let [wave-calls (atom [])
          reg-fx (resolve 'hive.events.fx/reg-fx)]
      (reg-fx :multi/wave-complete (fn [data] (swap! wave-calls conj data)))
      (try
        ;; a (wave 1) -> b (wave 2): 2 waves
        (multi/run-multi
         [{:id "a" :tool "fake-tool-xyz" :command "noop"}
          {:id "b" :tool "fake-tool-xyz" :command "noop" :depends_on ["a"]}])
        (is (= 2 (count @wave-calls))
            "Should emit 2 wave-complete events for 2 waves")
        (is (= [1 2] (mapv :wave-num @wave-calls)))
        (is (every? #(= 2 (:total-waves %)) @wave-calls))
        (finally
          (multi/register-fx!))))))

(deftest fx-op-error-emitted-on-failure-test
  (testing "run-multi emits :multi/op-error FX for each failed op"
    (require 'hive.events.fx)
    (let [error-calls (atom [])
          reg-fx (resolve 'hive.events.fx/reg-fx)]
      (reg-fx :multi/op-error (fn [data] (swap! error-calls conj data)))
      (try
        (multi/run-multi
         [{:id "a" :tool "fake-tool-xyz" :command "noop"}
          {:id "b" :tool "fake-tool-xyz" :command "noop"}])
        (is (= 2 (count @error-calls))
            "Should emit 2 op-error events for 2 failed ops")
        (is (= #{"a" "b"} (set (map :op-id @error-calls))))
        (is (every? #(= 1 (:wave-num %)) @error-calls))
        (is (every? #(string? (:error %)) @error-calls))
        (finally
          (multi/register-fx!))))))

(deftest fx-no-emission-on-dry-run-test
  (testing "run-multi does not emit FX on dry-run"
    (require 'hive.events.fx)
    (let [wave-calls (atom [])
          error-calls (atom [])
          reg-fx (resolve 'hive.events.fx/reg-fx)]
      (reg-fx :multi/wave-complete (fn [data] (swap! wave-calls conj data)))
      (reg-fx :multi/op-error (fn [data] (swap! error-calls conj data)))
      (try
        (multi/run-multi
         [{:id "a" :tool "memory" :command "help"}]
         :dry-run true)
        (is (= 0 (count @wave-calls))
            "Dry-run should not emit wave-complete FX")
        (is (= 0 (count @error-calls))
            "Dry-run should not emit op-error FX")
        (finally
          (multi/register-fx!))))))

(deftest fx-wave-complete-counts-correct-test
  (testing ":multi/wave-complete reports correct success/failed counts"
    (require 'hive.events.fx)
    (let [wave-calls (atom [])
          reg-fx (resolve 'hive.events.fx/reg-fx)]
      (reg-fx :multi/wave-complete (fn [data] (swap! wave-calls conj data)))
      (try
        ;; All ops fail (fake tools)
        (multi/run-multi
         [{:id "a" :tool "fake-xyz" :command "noop"}
          {:id "b" :tool "fake-xyz" :command "noop"}
          {:id "c" :tool "fake-xyz" :command "noop"}])
        (let [wc (first @wave-calls)]
          (is (= 3 (:op-count wc)))
          (is (= 0 (:success-count wc)))
          (is (= 3 (:failed-count wc))))
        (finally
          (multi/register-fx!))))))

(comment
  ;; Run all tests in this namespace via REPL
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.tools.multi-test))
