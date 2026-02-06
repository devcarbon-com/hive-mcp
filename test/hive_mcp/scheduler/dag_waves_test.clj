(ns hive-mcp.scheduler.dag-waves-test
  "Tests for the DAGWave scheduler.
   
   Tests the core scheduling logic with mocked dependencies:
   - Kanban queries mocked via with-redefs
   - KG edge queries mocked via with-redefs
   - Ling spawning mocked (no actual processes)
   - Channel subscription mocked (no pub/sub)
   
   Test categories:
   1. find-ready-tasks (pure logic)
   2. dispatch-wave! (stateful but mocked)
   3. on-ling-complete (event handler)
   4. start-dag!/stop-dag! (lifecycle)
   5. dag-status (query)
   6. Edge cases (cycles, failures, dry-run)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.scheduler.dag-waves :as dag]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.channel :as channel]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.tools.memory.scope :as scope]
            [clojure.data.json :as json]
            [clojure.core.async :as async]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-dag-state-fixture
  "Reset dag-state before each test."
  [f]
  (reset! dag/dag-state
          {:active    false
           :plan-id   nil
           :max-slots 5
           :wave-log  []
           :dispatched {}
           :completed  #{}
           :failed     #{}
           :opts       {}})
  (f)
  ;; Cleanup after test
  (reset! dag/dag-state
          {:active    false
           :plan-id   nil
           :max-slots 5
           :wave-log  []
           :dispatched {}
           :completed  #{}
           :failed     #{}
           :opts       {}}))

(use-fixtures :each reset-dag-state-fixture)

;; =============================================================================
;; Test Data
;; =============================================================================

(def test-directory "/tmp/test-project")
(def test-project-id "test-project")

;; Simulated kanban tasks
(def task-a {:id "task-a" :title "Setup database" :status "todo" :priority "high"})
(def task-b {:id "task-b" :title "Write API" :status "todo" :priority "medium"})
(def task-c {:id "task-c" :title "Write tests" :status "todo" :priority "medium"})
(def task-d {:id "task-d" :title "Deploy" :status "todo" :priority "low"})

;; DAG: A -> B -> D, A -> C -> D
;; Wave 0: A
;; Wave 1: B, C (both depend on A)
;; Wave 2: D (depends on B and C)

(def mock-edges
  "KG edges representing task dependencies.
   task-B --depends-on--> task-A
   task-C --depends-on--> task-A
   task-D --depends-on--> task-B
   task-D --depends-on--> task-C"
  {"task-a" []  ; A has no outgoing depends-on
   "task-b" [{:kg-edge/from "task-b" :kg-edge/to "task-a" :kg-edge/relation :depends-on}]
   "task-c" [{:kg-edge/from "task-c" :kg-edge/to "task-a" :kg-edge/relation :depends-on}]
   "task-d" [{:kg-edge/from "task-d" :kg-edge/to "task-b" :kg-edge/relation :depends-on}
             {:kg-edge/from "task-d" :kg-edge/to "task-c" :kg-edge/relation :depends-on}]})

;; Chroma entry existence tracker (for done detection)
(def ^:private *chroma-entries (atom {"task-a" task-a
                                       "task-b" task-b
                                       "task-c" task-c
                                       "task-d" task-d}))

;; =============================================================================
;; Mock Helpers
;; =============================================================================

(defn mock-kanban-list-slim
  "Mock kanban list that returns tasks from *chroma-entries."
  [{:keys [status directory]}]
  (let [entries (vals @*chroma-entries)
        filtered (if status
                   (filter #(= status (:status %)) entries)
                   entries)]
    {:type "text" :text (json/write-str filtered)}))

(defn mock-get-edges-from
  "Mock KG edge query."
  ([task-id] (get mock-edges task-id []))
  ([task-id _scope] (get mock-edges task-id [])))

(defn mock-get-entry-by-id
  "Mock Chroma get-entry-by-id. Returns nil for 'done' (deleted) tasks."
  [task-id]
  (get @*chroma-entries task-id))

(defn mock-kanban-move
  "Mock kanban move. When moving to 'done', removes from *chroma-entries."
  [{:keys [task_id new_status]}]
  (when (= new_status "done")
    (swap! *chroma-entries dissoc task_id))
  {:type "text" :text (json/write-str {:id task_id :status new_status})})

(def ^:private *spawned-lings (atom []))

(defn mock-create-ling!
  "Mock ling creation. Records spawn without starting actual processes."
  [id opts]
  (swap! *spawned-lings conj {:id id :opts opts})
  id)

(defn mock-shout!
  "Mock hivemind shout. No-op."
  [& _args]
  true)

(defn mock-subscribe!
  "Mock channel subscribe. Returns a channel that won't receive events."
  [_event-type]
  (async/chan 1))

(defn mock-unsubscribe!
  "Mock channel unsubscribe."
  [_event-type _ch]
  nil)

(defn mock-get-scope
  "Mock scope resolution."
  [_dir]
  test-project-id)

(defn mock-get-slave
  "Mock DataScript slave query."
  [agent-id]
  (let [spawned (first (filter #(= agent-id (:id %)) @*spawned-lings))]
    (when spawned
      {:slave/id agent-id
       :slave/kanban-task-id (get-in spawned [:opts :kanban-task-id])
       :slave/cwd test-directory})))

;; =============================================================================
;; Macro for test isolation
;; =============================================================================

(defmacro with-dag-mocks
  "Run body with all DAG dependencies mocked."
  [& body]
  `(do
     ;; Reset mutable test state
     (reset! *chroma-entries {"task-a" task-a
                              "task-b" task-b
                              "task-c" task-c
                              "task-d" task-d})
     (reset! *spawned-lings [])
     (with-redefs [mem-kanban/handle-mem-kanban-list-slim mock-kanban-list-slim
                   mem-kanban/handle-mem-kanban-move mock-kanban-move
                   kg-edges/get-edges-from mock-get-edges-from
                   chroma/get-entry-by-id mock-get-entry-by-id
                   ling/create-ling! mock-create-ling!
                   hivemind/shout! mock-shout!
                   channel/subscribe! mock-subscribe!
                   channel/unsubscribe! mock-unsubscribe!
                   scope/get-current-project-id mock-get-scope
                   ds-queries/get-slave mock-get-slave]
       ~@body)))

;; =============================================================================
;; Tests: find-ready-tasks
;; =============================================================================

(deftest find-ready-tasks-wave-0
  (testing "Tasks with no dependencies are immediately ready"
    (with-dag-mocks
      (let [ready (dag/find-ready-tasks test-directory #{} {} #{})]
        (is (= 1 (count ready)) "Only task-a has no dependencies")
        (is (= "task-a" (:task-id (first ready))))
        (is (= 0 (:dep-count (first ready))))))))

(deftest find-ready-tasks-wave-1
  (testing "Tasks become ready when dependencies complete"
    (with-dag-mocks
      ;; After A completes, B and C should be ready
      (let [ready (dag/find-ready-tasks test-directory #{"task-a"} {} #{})]
        (is (= 2 (count ready)) "Both B and C depend only on A")
        (is (= #{"task-b" "task-c"} (set (map :task-id ready))))))))

(deftest find-ready-tasks-wave-2
  (testing "Tasks with multiple deps wait for all"
    (with-dag-mocks
      ;; After A and B complete, D is not ready (still needs C)
      (let [ready (dag/find-ready-tasks test-directory #{"task-a" "task-b"} {} #{})]
        (is (= 1 (count ready)) "Only C is ready, D still needs C")
        (is (= "task-c" (:task-id (first ready)))))
      
      ;; After A, B, C complete, D is ready
      (let [ready (dag/find-ready-tasks test-directory #{"task-a" "task-b" "task-c"} {} #{})]
        (is (= 1 (count ready)) "D is now ready")
        (is (= "task-d" (:task-id (first ready))))))))

(deftest find-ready-tasks-excludes-dispatched
  (testing "Already dispatched tasks are excluded"
    (with-dag-mocks
      (let [ready (dag/find-ready-tasks test-directory #{} {"task-a" "ling-1"} #{})]
        (is (= 0 (count ready)) "task-a is already dispatched")))))

(deftest find-ready-tasks-excludes-failed
  (testing "Failed tasks are excluded"
    (with-dag-mocks
      (let [ready (dag/find-ready-tasks test-directory #{} {} #{"task-a"})]
        (is (= 0 (count ready)) "task-a is in failed set")))))

(deftest find-ready-tasks-empty-dag
  (testing "No tasks returns empty vector"
    (with-redefs [mem-kanban/handle-mem-kanban-list-slim
                  (fn [_] {:type "text" :text "[]"})
                  kg-edges/get-edges-from (fn [& _] [])
                  chroma/get-entry-by-id (fn [_] nil)]
      (let [ready (dag/find-ready-tasks test-directory #{} {} #{})]
        (is (= 0 (count ready)))))))

;; =============================================================================
;; Tests: dispatch-wave!
;; =============================================================================

(deftest dispatch-wave-basic
  (testing "Dispatches lings for ready tasks"
    (with-dag-mocks
      ;; Initialize state
      (reset! dag/dag-state (assoc @dag/dag-state :active true))
      
      (let [ready [{:task-id "task-a" :title "Setup database" :deps #{} :dep-count 0}]
            result (dag/dispatch-wave! ready 5
                                        {:cwd test-directory
                                         :presets ["ling"]
                                         :project-id test-project-id})]
        (is (= 1 (:dispatched-count result)))
        (is (= 1 (count @*spawned-lings)))
        (is (= "task-a" (get-in (first @*spawned-lings) [:opts :kanban-task-id])))
        (is (contains? (:dispatched @dag/dag-state) "task-a"))))))

(deftest dispatch-wave-respects-slots
  (testing "Only dispatches up to available slots"
    (with-dag-mocks
      (reset! dag/dag-state (assoc @dag/dag-state
                                   :active true
                                   :dispatched {"existing-1" "ling-x"
                                                "existing-2" "ling-y"}))
      
      (let [ready [{:task-id "task-a" :title "A" :deps #{} :dep-count 0}
                    {:task-id "task-b" :title "B" :deps #{} :dep-count 0}
                    {:task-id "task-c" :title "C" :deps #{} :dep-count 0}]
            ;; max-slots=3 but 2 already dispatched = 1 available
            result (dag/dispatch-wave! ready 3
                                        {:cwd test-directory
                                         :presets ["ling"]
                                         :project-id test-project-id})]
        (is (= 1 (:dispatched-count result)))
        (is (= 2 (:skipped-count result)))))))

(deftest dispatch-wave-zero-slots-dry-run
  (testing "max-slots=0 dispatches nothing (dry-run)"
    (with-dag-mocks
      (reset! dag/dag-state (assoc @dag/dag-state :active true))
      
      (let [ready [{:task-id "task-a" :title "A" :deps #{} :dep-count 0}]
            result (dag/dispatch-wave! ready 0
                                        {:cwd test-directory
                                         :presets ["ling"]
                                         :project-id test-project-id})]
        (is (= 0 (:dispatched-count result)))
        (is (= 0 (count @*spawned-lings)))))))

;; =============================================================================
;; Tests: on-ling-complete
;; =============================================================================

(deftest on-ling-complete-success
  (testing "Successful completion marks task done and dispatches next wave"
    (with-dag-mocks
      ;; Setup: A is dispatched
      (reset! dag/dag-state
              {:active true
               :plan-id "test-plan"
               :max-slots 5
               :wave-log []
               :dispatched {"task-a" "swarm-dag-test-123"}
               :completed #{}
               :failed #{}
               :opts {:cwd test-directory
                      :presets ["ling"]
                      :project-id test-project-id}})
      ;; Mock the slave lookup to return kanban-task-id
      (with-redefs [ds-queries/get-slave
                    (fn [agent-id]
                      (when (= agent-id "swarm-dag-test-123")
                        {:slave/id "swarm-dag-test-123"
                         :slave/kanban-task-id "task-a"
                         :slave/cwd test-directory}))]
        
        (dag/on-ling-complete {:agent-id "swarm-dag-test-123"
                               :project-id test-project-id
                               :data {:result "success"}})
        
        ;; Verify state updates
        (is (contains? (:completed @dag/dag-state) "task-a"))
        (is (not (contains? (:dispatched @dag/dag-state) "task-a")))
        ;; A was deleted from chroma (moved to done)
        (is (nil? (get @*chroma-entries "task-a")))
        ;; B and C should now be dispatched (they depend only on A)
        (is (= 2 (count @*spawned-lings)) "B and C should be auto-dispatched")))))

(deftest on-ling-complete-failure
  (testing "Failed ling moves task to failed set"
    (with-dag-mocks
      (reset! dag/dag-state
              {:active true
               :plan-id "test-plan"
               :max-slots 5
               :wave-log []
               :dispatched {"task-a" "swarm-dag-fail-123"}
               :completed #{}
               :failed #{}
               :opts {:cwd test-directory
                      :presets ["ling"]
                      :project-id test-project-id}})
      
      (with-redefs [ds-queries/get-slave
                    (fn [agent-id]
                      (when (= agent-id "swarm-dag-fail-123")
                        {:slave/id "swarm-dag-fail-123"
                         :slave/kanban-task-id "task-a"
                         :slave/cwd test-directory}))]
        
        (dag/on-ling-complete {:agent-id "swarm-dag-fail-123"
                               :project-id test-project-id
                               :data {:result "failure"}})
        
        ;; Task should be in failed, not completed
        (is (contains? (:failed @dag/dag-state) "task-a"))
        (is (not (contains? (:completed @dag/dag-state) "task-a")))
        ;; Task should NOT be deleted from chroma
        (is (some? (get @*chroma-entries "task-a")))))))

(deftest on-ling-complete-ignores-non-dag-lings
  (testing "Lings without kanban-task-id are ignored"
    (with-dag-mocks
      (reset! dag/dag-state (assoc @dag/dag-state :active true))
      
      (with-redefs [ds-queries/get-slave
                    (fn [_] {:slave/id "random-ling" :slave/kanban-task-id nil})]
        ;; Should not throw
        (dag/on-ling-complete {:agent-id "random-ling"
                               :project-id test-project-id
                               :data {}})
        (is (= 0 (count (:completed @dag/dag-state))))))))

(deftest on-ling-complete-ignores-when-inactive
  (testing "Events are ignored when DAG is not active"
    (with-dag-mocks
      (reset! dag/dag-state (assoc @dag/dag-state :active false))
      
      ;; Should not throw or modify state
      (dag/on-ling-complete {:agent-id "test"
                             :project-id test-project-id
                             :data {}})
      (is (= 0 (count (:completed @dag/dag-state)))))))

;; =============================================================================
;; Tests: Lifecycle (start-dag!/stop-dag!)
;; =============================================================================

(deftest start-dag-initializes-state
  (testing "start-dag! initializes state and dispatches first wave"
    (with-dag-mocks
      (let [result (dag/start-dag! "test-plan-001"
                                    {:cwd test-directory
                                     :max-slots 3
                                     :presets ["ling"]})]
        (is (:started result))
        (is (= "test-plan-001" (:plan-id result)))
        (is (= 3 (:max-slots result)))
        ;; Task A should be ready (wave 0)
        (is (= 1 (:ready-count result)))
        ;; Task A should have been dispatched
        (is (= 1 (get-in result [:initial-dispatch :dispatched-count])))
        ;; State should reflect dispatch
        (is (:active @dag/dag-state))
        (is (= "test-plan-001" (:plan-id @dag/dag-state)))))))

(deftest start-dag-rejects-when-active
  (testing "start-dag! throws when already active"
    (with-dag-mocks
      (dag/start-dag! "plan-1" {:cwd test-directory})
      (is (thrown? clojure.lang.ExceptionInfo
                   (dag/start-dag! "plan-2" {:cwd test-directory}))))))

(deftest stop-dag-returns-stats
  (testing "stop-dag! returns final statistics"
    (with-dag-mocks
      (dag/start-dag! "test-plan" {:cwd test-directory})
      ;; Simulate some progress
      (swap! dag/dag-state update :completed conj "fake-task")
      
      (let [result (dag/stop-dag!)]
        (is (:stopped result))
        (is (= "test-plan" (:plan-id result)))
        (is (= 1 (:completed-count result)))
        (is (not (:active @dag/dag-state)))))))

;; =============================================================================
;; Tests: dag-status
;; =============================================================================

(deftest dag-status-when-inactive
  (testing "dag-status returns inactive state"
    (let [status (dag/dag-status)]
      (is (not (:active status)))
      (is (nil? (:plan-id status))))))

(deftest dag-status-when-active
  (testing "dag-status returns full progress"
    (with-dag-mocks
      (dag/start-dag! "status-test" {:cwd test-directory :max-slots 5})
      
      (let [status (dag/dag-status)]
        (is (:active status))
        (is (= "status-test" (:plan-id status)))
        (is (= 5 (:max-slots status)))
        ;; task-a was dispatched
        (is (= 1 (:dispatched status)))
        (is (= 0 (:completed status)))
        (is (= 0 (:failed status)))))))

;; =============================================================================
;; Tests: Full DAG Execution (Integration-style)
;; =============================================================================

(deftest full-dag-execution
  (testing "Complete DAG execution: A -> B,C -> D"
    (with-dag-mocks
      ;; Start the DAG
      (dag/start-dag! "full-test" {:cwd test-directory :max-slots 5})
      
      ;; Wave 0: A dispatched
      (is (= 1 (count @*spawned-lings)))
      (let [a-ling-id (:id (first @*spawned-lings))]
        
        ;; Simulate A completing
        (with-redefs [ds-queries/get-slave
                      (fn [agent-id]
                        (when (= agent-id a-ling-id)
                          {:slave/id a-ling-id
                           :slave/kanban-task-id "task-a"
                           :slave/cwd test-directory}))]
          (dag/on-ling-complete {:agent-id a-ling-id
                                 :project-id test-project-id
                                 :data {:result "success"}})))
      
      ;; Wave 1: B and C should now be dispatched (total: 3 lings spawned)
      (is (= 3 (count @*spawned-lings)) "A + B + C = 3 lings spawned")
      (is (contains? (:completed @dag/dag-state) "task-a"))
      
      ;; Simulate B completing
      (let [b-ling (nth @*spawned-lings 1)
            b-ling-id (:id b-ling)]
        (with-redefs [ds-queries/get-slave
                      (fn [agent-id]
                        (when (= agent-id b-ling-id)
                          {:slave/id b-ling-id
                           :slave/kanban-task-id (get-in b-ling [:opts :kanban-task-id])
                           :slave/cwd test-directory}))]
          (dag/on-ling-complete {:agent-id b-ling-id
                                 :project-id test-project-id
                                 :data {:result "success"}})))
      
      ;; D still not ready (needs C too)
      (is (= 3 (count @*spawned-lings)) "D not yet dispatched, waiting on C")
      
      ;; Simulate C completing
      (let [c-ling (nth @*spawned-lings 2)
            c-ling-id (:id c-ling)]
        (with-redefs [ds-queries/get-slave
                      (fn [agent-id]
                        (when (= agent-id c-ling-id)
                          {:slave/id c-ling-id
                           :slave/kanban-task-id (get-in c-ling [:opts :kanban-task-id])
                           :slave/cwd test-directory}))]
          (dag/on-ling-complete {:agent-id c-ling-id
                                 :project-id test-project-id
                                 :data {:result "success"}})))
      
      ;; Wave 2: D should now be dispatched (total: 4 lings)
      (is (= 4 (count @*spawned-lings)) "A + B + C + D = 4 lings spawned")
      
      ;; Simulate D completing
      (let [d-ling (nth @*spawned-lings 3)
            d-ling-id (:id d-ling)]
        (with-redefs [ds-queries/get-slave
                      (fn [agent-id]
                        (when (= agent-id d-ling-id)
                          {:slave/id d-ling-id
                           :slave/kanban-task-id (get-in d-ling [:opts :kanban-task-id])
                           :slave/cwd test-directory}))]
          (dag/on-ling-complete {:agent-id d-ling-id
                                 :project-id test-project-id
                                 :data {:result "success"}})))
      
      ;; All tasks complete, DAG should auto-stop
      (is (= 4 (count (:completed @dag/dag-state))))
      (is (= 0 (count (:dispatched @dag/dag-state))))
      (is (not (:active @dag/dag-state)) "DAG should auto-stop when all tasks done"))))

;; =============================================================================
;; Tests: Wave Log
;; =============================================================================

(deftest wave-log-tracks-events
  (testing "Wave log records dispatch and completion events"
    (with-dag-mocks
      (dag/start-dag! "log-test" {:cwd test-directory})
      
      ;; Check dispatch was logged
      (let [log (:wave-log @dag/dag-state)]
        (is (= 1 (count log)))
        (is (= :dispatched (:event (first log))))
        (is (= "task-a" (:task-id (first log))))
        (is (some? (:timestamp (first log))))))))
