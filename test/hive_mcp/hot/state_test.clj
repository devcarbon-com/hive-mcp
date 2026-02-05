(ns hive-mcp.hot.state-test
  "TDD tests for hot-reload state preservation.

   Tests that:
   1. DataScript state survives hot-reload
   2. Snapshot/validate cycle works correctly
   3. State validation catches corruption

   ISOLATION: Uses test-specific DataScript connection to avoid
   interfering with running server state."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.hot.state :as hot-state]
            [hive-mcp.swarm.datascript.schema :as schema]
            [datascript.core :as d]))

;;; =============================================================================
;;; Test-Isolated DataScript Connection
;;; =============================================================================

;; Test-only connection - never touches production state
(def ^:dynamic *test-conn* nil)

(defn create-test-conn []
  (d/create-conn schema/schema))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn isolated-state-fixture [f]
  ;; Create isolated connection for this test
  (binding [*test-conn* (create-test-conn)]
    ;; Reset hot-state and inject test connection
    (hot-state/reset-hot-state!)
    (hot-state/set-test-conn! *test-conn*)
    (try
      (f)
      (finally
        (hot-state/clear-test-conn!)
        (hot-state/reset-hot-state!)))))

(use-fixtures :each isolated-state-fixture)

;;; =============================================================================
;;; Test: Snapshot Captures State
;;; =============================================================================

(deftest test-snapshot-captures-datom-count
  (testing "snapshot-state! captures current datom count"
    (let [conn *test-conn*
          _ (d/transact! conn [{:db/id -1 :test/name "test-entity"}])
          count-before (count (d/datoms @conn :eavt))
          snapshot-count (hot-state/snapshot-state!)
          status (hot-state/status)]

      (is (> count-before 0) "Should have some datoms")
      (is (= snapshot-count count-before) "Snapshot count should match")
      (is (true? (:has-snapshot? status)) "Should have snapshot")
      (is (= count-before (:pre-reload-count status)) "Pre-reload count stored"))))

;;; =============================================================================
;;; Test: Validation After Reload
;;; =============================================================================

(deftest test-validation-passes-with-preserved-state
  (testing "validate-state! passes when state is preserved"
    ;; Snapshot current state
    (hot-state/snapshot-state!)

    ;; Simulate reload (state preserved via defonce)
    ;; In reality, clj-reload preserves defonce atoms

    ;; Validate
    (let [result (hot-state/validate-state!)]
      (is (true? (:valid? result)) "Validation should pass")
      (is (true? (get-in result [:checks :connection])) "Connection valid")
      (is (true? (get-in result [:checks :datom-count])) "Datom count valid"))))

(deftest test-validation-passes-with-new-data
  (testing "validate-state! passes when new data added after reload"
    (let [conn *test-conn*]
      ;; Snapshot current state
      (hot-state/snapshot-state!)

      ;; Simulate reload + new data (state grows)
      (d/transact! conn [{:db/id -1 :test/name "new-post-reload"}])

      ;; Validate - should pass because count increased
      (let [result (hot-state/validate-state!)]
        (is (true? (:valid? result)) "Validation should pass with growth")
        (is (> (get-in result [:counts :after])
               (get-in result [:counts :before]))
            "After count > before count")))))

;;; =============================================================================
;;; Test: Status Reporting
;;; =============================================================================

(deftest test-status-before-snapshot
  (testing "status shows no snapshot before snapshot-state!"
    (let [status (hot-state/status)]
      (is (false? (:has-snapshot? status)))
      (is (nil? (:pre-reload-count status)))
      (is (false? (:validated? status))))))

(deftest test-status-after-validation
  (testing "status shows validated after validate-state!"
    (hot-state/snapshot-state!)
    (hot-state/validate-state!)

    (let [status (hot-state/status)]
      (is (true? (:validated? status))))))

;;; =============================================================================
;;; Test: Full Reload Cycle Simulation
;;; =============================================================================

(deftest test-full-reload-cycle
  (testing "Full snapshot → reload → validate cycle"
    (let [conn *test-conn*]
      ;; Add test data
      (d/transact! conn [{:db/id -1 :test/cycle "before"}])
      (let [before-count (count (d/datoms @conn :eavt))]

        ;; Phase 1: Pre-reload snapshot
        (hot-state/snapshot-state!)
        (is (= before-count (:pre-reload-count (hot-state/status))))

        ;; Phase 2: Simulate hot-reload
        ;; (In real scenario, clj-reload would reload namespaces)
        ;; The defonce conn atom survives

        ;; Phase 3: Post-reload validation
        (let [result (hot-state/validate-state!)]
          (is (true? (:valid? result)) "Full cycle should validate")
          (is (true? (:validated? (hot-state/status))))

          ;; Verify data still exists
          (let [entities (d/q '[:find ?e ?n
                                :where [?e :test/cycle ?n]]
                              @conn)]
            (is (= 1 (count entities)) "Test entity should survive")))))))

;;; =============================================================================
;;; Test: Reset Clears State
;;; =============================================================================

(deftest test-reset-clears-hot-state
  (testing "reset-hot-state! clears all hot-state"
    (hot-state/snapshot-state!)
    (hot-state/validate-state!)

    (is (true? (:validated? (hot-state/status))))

    (hot-state/reset-hot-state!)

    (let [status (hot-state/status)]
      (is (false? (:has-snapshot? status)))
      (is (nil? (:pre-reload-count status)))
      (is (false? (:validated? status))))))
