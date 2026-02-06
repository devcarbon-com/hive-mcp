(ns hive-mcp.tools.catchup.format-test
  "Tests for catchup formatting and rendering functions.
   Extracted from catchup.clj in Sprint 1 refactoring."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.tools.catchup.format :as fmt]))

;; =============================================================================
;; Entry Metadata Transform Tests
;; =============================================================================

(deftest entry->catchup-meta-test
  (testing "basic entry conversion with string content"
    (let [entry {:id "test-1" :type "decision" :content "This is a test decision" :tags ["foo" "bar"]}
          result (fmt/entry->catchup-meta entry 80)]
      (is (= "test-1" (:id result)))
      (is (= "decision" (:type result)))
      (is (= "This is a test decision" (:preview result)))
      (is (= ["foo" "bar"] (:tags result)))))

  (testing "truncation at preview-len"
    (let [long-content (apply str (repeat 200 "x"))
          entry {:id "test-2" :type "note" :content long-content :tags []}
          result (fmt/entry->catchup-meta entry 50)]
      (is (= 50 (count (:preview result))))))

  (testing "nil preview-len defaults to 80"
    (let [long-content (apply str (repeat 200 "x"))
          entry {:id "test-3" :type "note" :content long-content :tags []}
          result (fmt/entry->catchup-meta entry nil)]
      (is (= 80 (count (:preview result))))))

  (testing "non-string content is converted"
    (let [entry {:id "test-4" :type "note" :content 42 :tags []}
          result (fmt/entry->catchup-meta entry 80)]
      (is (= "42" (:preview result)))))

  (testing "nil type defaults to 'note'"
    (let [entry {:id "test-5" :content "test" :tags []}
          result (fmt/entry->catchup-meta entry 80)]
      (is (= "note" (:type result)))))

  (testing "nil tags defaults to empty vector"
    (let [entry {:id "test-6" :type "note" :content "test"}
          result (fmt/entry->catchup-meta entry 80)]
      (is (= [] (:tags result))))))

(deftest entry->axiom-meta-test
  (testing "axiom metadata with full content"
    (let [entry {:id "ax-1" :tags ["axiom" "critical"] :content "Never do X"}
          result (fmt/entry->axiom-meta entry)]
      (is (= "ax-1" (:id result)))
      (is (= "axiom" (:type result)))
      (is (= "INVIOLABLE" (:severity result)))
      (is (= "Never do X" (:content result)))
      (is (= ["axiom" "critical"] (:tags result)))))

  (testing "nil tags default to empty vector"
    (let [entry {:id "ax-2" :content "Rule"}
          result (fmt/entry->axiom-meta entry)]
      (is (= [] (:tags result))))))

(deftest entry->priority-meta-test
  (testing "priority convention with full content"
    (let [entry {:id "prio-1" :tags ["catchup-priority"] :content "Always do Y"}
          result (fmt/entry->priority-meta entry)]
      (is (= "prio-1" (:id result)))
      (is (= "convention" (:type result)))
      (is (= "Always do Y" (:content result)))
      (is (= ["catchup-priority"] (:tags result))))))

;; =============================================================================
;; Response Builder Tests
;; =============================================================================

(deftest build-catchup-response-test
  (testing "builds valid JSON response structure"
    (let [resp (fmt/build-catchup-response
                {:project-name "test-proj" :project-id "test-proj"
                 :scopes ["scope:project:test-proj"]
                 :git-info {:branch "main" :uncommitted false}
                 :permeation {:permeated 0 :agents []}
                 :axioms-meta [{:id "a1"}]
                 :priority-meta []
                 :sessions-meta [{:id "s1"} {:id "s2"}]
                 :decisions-meta [{:id "d1"}]
                 :conventions-meta []
                 :snippets-meta []
                 :expiring-meta [{:id "e1"}]
                 :kg-insights {:edge-count 5}
                 :project-tree-scan {:scanned true}
                 :disc-decay {:updated 3 :skipped 0}})
          parsed (json/read-str (:text resp) :key-fn keyword)]
      (is (= "text" (:type resp)))
      (is (true? (:success parsed)))
      (is (= "test-proj" (:project parsed)))
      (is (= 1 (get-in parsed [:counts :axioms])))
      (is (= 2 (get-in parsed [:counts :sessions])))
      (is (= 1 (get-in parsed [:counts :decisions])))
      (is (= 1 (get-in parsed [:counts :expiring])))
      (is (= 5 (get-in parsed [:kg-insights :edge-count])))
      (is (= 3 (get-in parsed [:disc-decay :updated])))
      (is (string? (:hint parsed))))))

(deftest chroma-not-configured-error-test
  (testing "returns error with isError flag"
    (let [resp (fmt/chroma-not-configured-error)
          parsed (json/read-str (:text resp) :key-fn keyword)]
      (is (true? (:isError resp)))
      (is (false? (:success parsed)))
      (is (= "Chroma not configured" (:error parsed))))))

(deftest catchup-error-test
  (testing "returns error from exception"
    (let [resp (fmt/catchup-error (Exception. "test failure"))
          parsed (json/read-str (:text resp) :key-fn keyword)]
      (is (false? (:success parsed)))
      (is (= "test failure" (:error parsed))))))

;; =============================================================================
;; Spawn Context Markdown Formatter Tests
;; =============================================================================

(deftest format-spawn-axioms-test
  (testing "formats numbered axiom list"
    (let [axioms [{:content "Rule one"} {:content "Rule two"}]
          result (fmt/format-spawn-axioms axioms)]
      (is (string? result))
      (is (.contains result "### Axioms"))
      (is (.contains result "1. Rule one"))
      (is (.contains result "2. Rule two"))))

  (testing "returns nil for empty axioms"
    (is (nil? (fmt/format-spawn-axioms [])))))

(deftest format-spawn-priorities-test
  (testing "formats numbered priority list"
    (let [convs [{:content "Conv one"}]
          result (fmt/format-spawn-priorities convs)]
      (is (.contains result "### Priority Conventions"))
      (is (.contains result "1. Conv one"))))

  (testing "returns nil for empty"
    (is (nil? (fmt/format-spawn-priorities [])))))

(deftest format-spawn-decisions-test
  (testing "formats bulleted decision list"
    (let [decisions [{:preview "Decision A"} {:preview "Decision B"}]
          result (fmt/format-spawn-decisions decisions)]
      (is (.contains result "### Active Decisions"))
      (is (.contains result "- Decision A"))
      (is (.contains result "- Decision B"))))

  (testing "returns nil for empty"
    (is (nil? (fmt/format-spawn-decisions [])))))

(deftest format-spawn-git-test
  (testing "formats git info"
    (let [result (fmt/format-spawn-git {:branch "main" :uncommitted true :last-commit "abc - fix"})]
      (is (.contains result "### Git Status"))
      (is (.contains result "**Branch**: main"))
      (is (.contains result "Uncommitted changes"))
      (is (.contains result "abc - fix"))))

  (testing "returns nil for nil git-info"
    (is (nil? (fmt/format-spawn-git nil)))))

(deftest format-spawn-stale-files-test
  (testing "formats stale file list"
    (let [files [{:path "src/foo.clj" :score 0.8 :days-since-read 5 :hash-mismatch? true}]
          result (fmt/format-spawn-stale-files files)]
      (is (.contains result "### Files Needing Re-Grounding"))
      (is (.contains result "`src/foo.clj`"))
      (is (.contains result "content changed"))))

  (testing "returns nil for empty"
    (is (nil? (fmt/format-spawn-stale-files [])))))

(deftest serialize-spawn-context-test
  (testing "assembles full spawn context markdown"
    (let [result (fmt/serialize-spawn-context
                  {:axioms [{:content "Axiom 1"}]
                   :priority-conventions [{:content "Conv 1"}]
                   :decisions [{:preview "Dec 1"}]
                   :git-info {:branch "dev" :uncommitted false :last-commit "xyz"}
                   :project-name "test-proj"
                   :stale-files []})]
      (is (.contains result "## Project Context (Auto-Injected)"))
      (is (.contains result "**Project**: test-proj"))
      (is (.contains result "Axiom 1"))
      (is (.contains result "Conv 1"))
      (is (.contains result "Dec 1"))
      (is (.contains result "**Branch**: dev")))))

(deftest max-spawn-context-chars-test
  (testing "constant is reasonable (~3K tokens)"
    (is (= 12000 fmt/max-spawn-context-chars))))
