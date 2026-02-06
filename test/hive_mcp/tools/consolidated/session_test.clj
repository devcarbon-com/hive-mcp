(ns hive-mcp.tools.consolidated.session-test
  "Tests for consolidated session CLI tool.

   Test Coverage:
   1. Backward compat - existing commands (complete, wrap, whoami, help)
   2. Catchup subcommand - delegation to handle-native-catchup
   3. Tool definition - schema includes catchup
   4. Handler map completeness"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [hive-mcp.tools.consolidated.session :as session]
            [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.chroma :as chroma]))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn parse-response
  "Parse JSON response from handler."
  [result]
  (when (and result (not (:isError result)) (:text result))
    (try
      (json/read-str (:text result) :key-fn keyword)
      (catch Exception _ nil))))

;; =============================================================================
;; Handler Map Tests
;; =============================================================================

(deftest test-handlers-map-completeness
  (testing "all handlers are registered"
    (is (contains? session/handlers :complete))
    (is (contains? session/handlers :wrap))
    (is (contains? session/handlers :whoami))
    (is (contains? session/handlers :catchup))))

(deftest test-handlers-are-functions
  (testing "all handlers are functions"
    (doseq [[k v] session/handlers]
      (is (fn? v) (str "Handler " k " should be a function")))))

;; =============================================================================
;; CLI Handler Tests (backward compat)
;; =============================================================================

(deftest test-cli-handler-help-command
  (testing "help command returns help text with all commands"
    (let [result (session/handle-session {:command "help"})]
      (is (not (:isError result)))
      (is (= "text" (:type result)))
      (is (str/includes? (:text result) "Available commands"))
      (is (str/includes? (:text result) "complete"))
      (is (str/includes? (:text result) "wrap"))
      (is (str/includes? (:text result) "whoami"))
      (is (str/includes? (:text result) "catchup")))))

(deftest test-cli-handler-unknown-command
  (testing "unknown command returns error"
    (let [result (session/handle-session {:command "bogus"})]
      (is (:isError result))
      (is (str/includes? (:text result) "Unknown command")))))

;; =============================================================================
;; Whoami Handler Tests (backward compat)
;; =============================================================================

(deftest test-whoami-returns-identity
  (testing "whoami returns agent identity context"
    (let [result (session/handle-session {:command "whoami"
                                          :agent_id "test-agent"
                                          :directory "/tmp/test"})]
      (is (not (:isError result)))
      (let [parsed (parse-response result)]
        (is (= "test-agent" (:agent-id parsed)))
        (is (string? (:cwd parsed)))))))

;; =============================================================================
;; Catchup Handler Tests
;; =============================================================================

(deftest test-catchup-delegates-to-native-catchup
  (testing "catchup handler delegates to handle-native-catchup"
    ;; Mock Chroma to avoid real DB calls
    (with-redefs [catchup/handle-native-catchup
                  (fn [args]
                    {:type "text"
                     :text (json/write-str {:catchup true
                                            :directory (:directory args)})})]
      (let [result (session/handle-session {:command "catchup"
                                            :directory "/tmp/project"})]
        (is (not (:isError result)))
        (let [parsed (parse-response result)]
          (is (:catchup parsed) "should have called catchup handler")
          (is (= "/tmp/project" (:directory parsed))))))))

(deftest test-catchup-handles-exception
  (testing "catchup handler catches exceptions gracefully"
    (with-redefs [catchup/handle-native-catchup
                  (fn [_] (throw (ex-info "Chroma down" {})))]
      (let [result (session/handle-session {:command "catchup"
                                            :directory "/tmp"})]
        (is (:isError result))
        (is (str/includes? (:text result) "Catchup failed"))))))

(deftest test-catchup-without-chroma
  (testing "catchup when Chroma not configured returns error"
    ;; When Chroma is not configured, handle-native-catchup returns error
    (with-redefs [chroma/embedding-configured? (constantly false)
                  catchup/handle-native-catchup
                  (fn [_]
                    {:type "text"
                     :text "Chroma not configured"
                     :isError true})]
      (let [result (session/handle-session {:command "catchup"})]
        (is (:isError result))))))

(deftest test-catchup-passes-all-params
  (testing "catchup passes full params map to delegate"
    (let [received (atom nil)]
      (with-redefs [catchup/handle-native-catchup
                    (fn [args]
                      (reset! received args)
                      {:type "text" :text "{}"})]
        (session/handle-session {:command "catchup"
                                 :directory "/my/project"
                                 :agent_id "my-agent"})
        (is (= "/my/project" (:directory @received)))
        ;; Full params map is passed through
        (is (= "catchup" (:command @received)))))))

;; =============================================================================
;; Tool Definition Tests
;; =============================================================================

(deftest test-tool-definition-structure
  (testing "tool-def has required fields"
    (is (= "session" (:name session/tool-def)))
    (is (string? (:description session/tool-def)))
    (is (map? (:inputSchema session/tool-def)))
    (is (fn? (:handler session/tool-def)))))

(deftest test-tool-definition-includes-catchup
  (testing "tool-def enum includes catchup"
    (let [enum (get-in session/tool-def [:inputSchema :properties "command" :enum])]
      (is (some #(= "catchup" %) enum) "enum should include catchup")
      (is (some #(= "complete" %) enum) "enum should include complete")
      (is (some #(= "wrap" %) enum) "enum should include wrap")
      (is (some #(= "whoami" %) enum) "enum should include whoami")
      (is (some #(= "help" %) enum) "enum should include help"))))

(deftest test-tool-description-mentions-catchup
  (testing "tool description mentions catchup"
    (is (str/includes? (:description session/tool-def) "catchup"))))

(deftest test-tools-vector
  (testing "tools vector contains tool-def"
    (is (= 1 (count session/tools)))
    (is (= session/tool-def (first session/tools)))))
