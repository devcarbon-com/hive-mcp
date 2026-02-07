(ns hive-mcp.hot.silence-test
  "TDD tests for SAA Silence strategy - hot-reload aware exploration.

   Tests that:
   1. Exploration sessions can be started and tracked
   2. Hot-reload events pause/resume exploration
   3. Grounding metadata is captured for files read
   4. Exploration summaries are generated correctly

   ISOLATION: Uses atom-based state that can be reset between tests.
   TDD: Tests written first to define the API contract."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.hot.silence :as silence]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn reset-silence-state-fixture [f]
  ;; Reset silence state before each test
  (silence/reset-state!)
  (try
    (f)
    (finally
      (silence/reset-state!))))

(use-fixtures :each reset-silence-state-fixture)

;;; =============================================================================
;;; Test: Session Lifecycle
;;; =============================================================================

(deftest test-start-exploration-session
  (testing "start-exploration! creates a new session"
    (let [session-id (silence/start-exploration! {:task "Analyze auth module"
                                                   :agent-id "ling-123"})]
      (is (string? session-id) "Returns session ID")
      (is (re-matches #"silence-\d+-\w+" session-id) "Session ID has expected format"))))

(deftest test-session-status
  (testing "status returns current session info"
    (silence/start-exploration! {:task "Test task"})
    (let [status (silence/status)]
      (is (true? (:exploring? status)) "Should be exploring")
      (is (= "Test task" (:task status)) "Task is captured")
      (is (zero? (:files-read status)) "No files read yet")
      (is (false? (:paused? status)) "Not paused initially"))))

(deftest test-end-exploration-session
  (testing "end-exploration! returns summary and clears session"
    (silence/start-exploration! {:task "Explore module"})
    (silence/record-file-read! "/src/foo.clj" {:lines 100})
    (silence/record-file-read! "/src/bar.clj" {:lines 50})
    
    (let [summary (silence/end-exploration!)]
      (is (map? summary) "Returns summary map")
      (is (= 2 (:files-read summary)) "Counted files")
      (is (= 150 (:total-lines summary)) "Summed lines")
      (is (inst? (:started-at summary)) "Has start time")
      (is (inst? (:ended-at summary)) "Has end time"))
    
    (let [status (silence/status)]
      (is (false? (:exploring? status)) "No longer exploring"))))

;;; =============================================================================
;;; Test: File Read Tracking
;;; =============================================================================

(deftest test-record-file-read
  (testing "record-file-read! tracks files explored"
    (silence/start-exploration! {:task "Test"})
    (silence/record-file-read! "/src/core.clj" {:lines 200 :hash "abc123"})
    
    (let [files (silence/get-files-read)]
      (is (= 1 (count files)))
      (is (= "/src/core.clj" (:path (first files))))
      (is (= 200 (:lines (first files))))
      (is (= "abc123" (:hash (first files))))
      (is (inst? (:read-at (first files)))))))

(deftest test-duplicate-file-reads-tracked
  (testing "Multiple reads of same file are tracked (for re-read detection)"
    (silence/start-exploration! {:task "Test"})
    (silence/record-file-read! "/src/core.clj" {:lines 200})
    (silence/record-file-read! "/src/core.clj" {:lines 200})
    
    (let [files (silence/get-files-read)]
      (is (= 2 (count files)) "Both reads recorded"))))

(deftest test-record-discovery
  (testing "record-discovery! captures key findings"
    (silence/start-exploration! {:task "Test"})
    (silence/record-discovery! {:type :pattern
                                :description "Found singleton pattern in auth.clj"
                                :file "/src/auth.clj"
                                :line 42})
    
    (let [discoveries (silence/get-discoveries)]
      (is (= 1 (count discoveries)))
      (is (= :pattern (:type (first discoveries))))
      (is (= 42 (:line (first discoveries)))))))

;;; =============================================================================
;;; Test: Hot-Reload Integration
;;; =============================================================================

(deftest test-pause-on-hot-reload-start
  (testing "on-reload-start pauses exploration"
    (silence/start-exploration! {:task "Test"})
    (is (false? (:paused? (silence/status))) "Not paused initially")
    
    (silence/on-reload-start!)
    
    (is (true? (:paused? (silence/status))) "Paused after reload start")))

(deftest test-resume-on-hot-reload-success
  (testing "on-reload-success resumes exploration"
    (silence/start-exploration! {:task "Test"})
    (silence/on-reload-start!)
    (is (true? (:paused? (silence/status))))
    
    (silence/on-reload-success!)
    
    (is (false? (:paused? (silence/status))) "Resumed after success")))

(deftest test-file-read-blocked-when-paused
  (testing "record-file-read! returns :paused when exploration is paused"
    (silence/start-exploration! {:task "Test"})
    (silence/on-reload-start!)
    
    (let [result (silence/record-file-read! "/src/foo.clj" {:lines 100})]
      (is (= :paused result) "Returns :paused when hot-reload in progress"))
    
    (is (zero? (:files-read (silence/status))) "File not recorded")))

(deftest test-exploration-tracks-reload-count
  (testing "Exploration summary includes reload count"
    (silence/start-exploration! {:task "Test"})
    (silence/on-reload-start!)
    (silence/on-reload-success!)
    (silence/on-reload-start!)
    (silence/on-reload-success!)
    
    (let [summary (silence/end-exploration!)]
      (is (= 2 (:hot-reload-count summary)) "Counted hot-reloads"))))

;;; =============================================================================
;;; Test: No Active Session
;;; =============================================================================

(deftest test-operations-without-session
  (testing "Operations return appropriate errors without active session"
    (is (= :no-session (silence/record-file-read! "/foo.clj" {})))
    (is (= :no-session (silence/record-discovery! {:type :test})))
    (is (nil? (silence/end-exploration!)))))

;;; =============================================================================
;;; Test: Grounding Support
;;; =============================================================================

(deftest test-grounding-metadata-captured
  (testing "File reads capture grounding metadata for KG integration"
    (silence/start-exploration! {:task "Test" :agent-id "ling-1"})
    (silence/record-file-read! "/src/auth.clj" 
                               {:lines 200 
                                :hash "sha256-abc"
                                :namespace 'my.auth})
    
    (let [files (silence/get-files-read)
          f (first files)]
      (is (= "sha256-abc" (:hash f)) "Hash captured for grounding")
      (is (= 'my.auth (:namespace f)) "Namespace captured"))))

(deftest test-exploration-summary-for-kg
  (testing "end-exploration! returns KG-compatible grounding data"
    (silence/start-exploration! {:task "Analyze auth" :agent-id "ling-1"})
    (silence/record-file-read! "/src/auth.clj" {:lines 100 :hash "abc"})
    (silence/record-discovery! {:type :pattern :description "Found issue"})
    
    (let [summary (silence/end-exploration!)]
      (is (contains? summary :grounding) "Has grounding section")
      (is (contains? (:grounding summary) :files-read) "Grounding has files")
      (is (= 1 (count (get-in summary [:grounding :files-read])))))))

;;; =============================================================================
;;; Test: hive-hot Registration
;;; =============================================================================

(deftest test-register-with-hive-hot
  (testing "register-with-hive-hot! returns status"
    ;; In test environment, hive-hot may not be available
    (let [result (silence/register-with-hive-hot!)]
      (is (contains? #{:registered :not-available} result)))))
