(ns hive-mcp.workflow.yaml-engine
  "YAML-based workflow engine implementing IWorkflowEngine.

   Parses YAML workflow definitions with:
   - Sequential and parallel step execution
   - Variable substitution ({{var}} syntax)
   - Step output propagation via context
   - Conditional step execution (:when clauses)
   - Dependency-based topological ordering

   YAML workflow format:
     name: my-workflow
     version: '1.0'
     description: Example workflow
     params:
       scope: default-scope
     steps:
       - id: step-1
         title: First Step
         action: echo
         args:
           message: 'Hello {{scope}}'
       - id: step-2
         title: Parallel Group
         parallel:
           - id: step-2a
             title: Branch A
             action: echo
             args:
               message: 'Branch A'
           - id: step-2b
             title: Branch B
             action: echo
             args:
               message: 'Branch B'
       - id: step-3
         title: Conditional Step
         action: echo
         when: '{{step-1.success}}'
         depends-on:
           - step-1
         args:
           message: 'Step 1 result: {{step-1.result.message}}'

   Built-in actions:
   - echo:      Returns args as result
   - transform: Applies a Clojure fn string to context
   - shell:     Executes a shell command (dry-run safe)
   - noop:      Does nothing (placeholder/debugging)

   SOLID-O: Open for extension via :action-registry.
   CLARITY-Y: Never throws - all errors returned in result maps.
   CLARITY-I: Full introspection via get-status."
  (:require [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hive-mcp.protocols.workflow :as wf]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Variable Substitution
;;; =============================================================================

(defn- resolve-var-path
  "Resolve a dotted variable path against context.
   E.g., 'step-1.result.message' -> (get-in ctx [:step-1 :result :message])"
  [ctx path-str]
  (let [parts (str/split path-str #"\.")
        ks    (mapv keyword parts)]
    (get-in ctx ks)))

(defn- substitute-vars
  "Replace all {{var}} placeholders in a string with values from context.
   Unresolved vars are left as-is."
  [s ctx]
  (if (string? s)
    (str/replace s #"\{\{([^}]+)\}\}"
                 (fn [[_match var-name]]
                   (let [v (or (get ctx (keyword var-name))
                               (resolve-var-path ctx var-name))]
                     (if (some? v) (str v) (str "{{" var-name "}}")))))
    s))

(defn- substitute-in-map
  "Deep-substitute all string values in a map using context."
  [m ctx]
  (walk/postwalk
   (fn [x]
     (if (string? x)
       (substitute-vars x ctx)
       x))
   m))

;;; =============================================================================
;;; YAML Parsing
;;; =============================================================================

(defn parse-yaml
  "Parse a YAML string into a workflow definition map.
   Returns {:parsed? true :data ...} or {:parsed? false :errors [...]}."
  [yaml-str]
  (try
    (let [data (yaml/parse-string yaml-str)]
      (if (map? data)
        {:parsed? true :data data}
        {:parsed? false :errors ["YAML did not parse to a map"]}))
    (catch Exception e
      {:parsed? false :errors [(str "YAML parse error: " (.getMessage e))]})))

(defn- normalize-step
  "Normalize a raw YAML step map to canonical form."
  [raw-step]
  (let [step (into {} (map (fn [[k v]] [(keyword k) v]) raw-step))]
    (cond-> {:step-id      (or (:id step) (str (gensym "step-")))
             :title        (or (:title step) "Untitled step")
             :description  (:description step)
             :action       (keyword (or (:action step) :noop))
             :args         (or (:args step) {})
             :dependencies (vec (or (:depends-on step) []))}
      (:when step)     (assoc :when-expr (:when step))
      (:parallel step) (assoc :parallel
                              (mapv normalize-step (:parallel step))))))

(defn- normalize-workflow
  "Normalize parsed YAML data into canonical workflow structure."
  [data]
  {:name        (or (:name data) "unnamed")
   :version     (or (:version data) "0.0.1")
   :description (or (:description data) "")
   :params      (or (:params data) {})
   :steps       (mapv normalize-step (or (:steps data) []))})

;;; =============================================================================
;;; Topological Sort (Dependency Resolution)
;;; =============================================================================

(defn- collect-step-ids
  "Collect all step IDs from steps (including nested parallel branches)."
  [steps]
  (reduce (fn [ids step]
            (let [ids (conj ids (:step-id step))]
              (if (:parallel step)
                (into ids (map :step-id (:parallel step)))
                ids)))
          #{}
          steps))

(defn- build-dep-graph
  "Build adjacency map {step-id -> set-of-deps} from steps."
  [steps]
  (reduce (fn [graph step]
            (let [sid  (:step-id step)
                  deps (set (:dependencies step))]
              (assoc graph sid deps)))
          {}
          steps))

(defn- topological-sort
  "Kahn's algorithm for topological sort.
   Returns {:sorted [...]} or {:cycle-detected true :nodes ...}."
  [graph all-ids]
  (let [in-degree (reduce (fn [deg id]
                            (reduce (fn [d dep] (update d dep (fnil identity 0)))
                                    (assoc deg id (count (get graph id #{})))
                                    (get graph id #{})))
                          {}
                          all-ids)
        in-degree (reduce (fn [d id] (update d id (fnil identity 0))) in-degree all-ids)]
    (loop [queue   (vec (filter #(zero? (get in-degree %)) all-ids))
           visited #{}
           sorted  []
           deg     in-degree]
      (if (empty? queue)
        (if (= (count sorted) (count all-ids))
          {:sorted sorted}
          {:cycle-detected true
           :nodes (remove (set sorted) all-ids)})
        (let [node  (first queue)
              queue (subvec queue 1)]
          (if (visited node)
            (recur queue visited sorted deg)
            (let [dependents (filter (fn [id]
                                      (contains? (get graph id #{}) node))
                                    all-ids)
                  new-deg    (reduce (fn [d dep-id]
                                      (update d dep-id dec))
                                    deg
                                    dependents)
                  new-ready  (filter #(and (not (visited %))
                                          (not= % node)
                                          (zero? (get new-deg %)))
                                    dependents)]
              (recur (into queue new-ready)
                     (conj visited node)
                     (conj sorted node)
                     new-deg))))))))

;;; =============================================================================
;;; Built-in Actions
;;; =============================================================================

(defmulti execute-action
  "Execute a workflow step action. Dispatches on :action keyword."
  (fn [action _args _ctx _opts] action))

(defmethod execute-action :echo
  [_ args ctx _opts]
  {:success? true
   :result   (substitute-in-map args ctx)})

(defmethod execute-action :noop
  [_ _args _ctx _opts]
  {:success? true
   :result   {}})

(defmethod execute-action :transform
  [_ args ctx _opts]
  (try
    (let [fn-str  (substitute-vars (or (:fn args) "identity") ctx)
          the-fn  (eval (read-string fn-str))
          input   (or (:input args) ctx)
          result  (the-fn input)]
      {:success? true
       :result   result})
    (catch Exception e
      {:success? false
       :result   nil
       :errors   [(str "Transform error: " (.getMessage e))]})))

(defmethod execute-action :shell
  [_ args ctx opts]
  (if (:dry-run? opts)
    {:success? true
     :result   {:dry-run true :command (substitute-vars (:command args) ctx)}}
    (try
      (let [cmd    (substitute-vars (:command args) ctx)
            proc   (-> (ProcessBuilder. ["sh" "-c" cmd])
                       (.redirectErrorStream true)
                       (.start))
            output (slurp (.getInputStream proc))
            exit   (.waitFor proc)]
        {:success? (zero? exit)
         :result   {:output (str/trim output) :exit-code exit :command cmd}
         :errors   (when-not (zero? exit)
                     [(str "Shell command exited with code " exit)])})
      (catch Exception e
        {:success? false
         :result   nil
         :errors   [(str "Shell error: " (.getMessage e))]}))))

(defmethod execute-action :default
  [action _args _ctx _opts]
  {:success? false
   :result   nil
   :errors   [(str "Unknown action: " (name action))]})

;;; =============================================================================
;;; Condition Evaluation
;;; =============================================================================

(defn- evaluate-when
  "Evaluate a :when expression against context."
  [when-expr ctx]
  (if (nil? when-expr)
    true
    (let [resolved (substitute-vars when-expr ctx)]
      (cond
        (= resolved "true")  true
        (= resolved "false") false
        (= resolved "")      false
        (nil? resolved)       false
        :else                 (boolean resolved)))))

;;; =============================================================================
;;; Execution State Management
;;; =============================================================================

(defonce ^:private executions (atom {}))

(defn- new-execution!
  "Create and register a new workflow execution state."
  [workflow-id workflow-name steps-total]
  (let [state {:workflow-id     workflow-id
               :name            workflow-name
               :status          :pending
               :current-step    nil
               :steps-completed 0
               :steps-total     steps-total
               :started-at      nil
               :completed-at    nil
               :errors          []
               :progress        0.0}]
    (swap! executions assoc workflow-id state)
    state))

(defn- update-execution!
  "Update an execution's state."
  [workflow-id updates]
  (swap! executions update workflow-id merge updates))

;;; =============================================================================
;;; Step Execution Helpers
;;; =============================================================================

(defn- count-all-steps
  "Count total steps including parallel branches."
  [steps]
  (reduce (fn [n step]
            (if (:parallel step)
              (+ n (count (:parallel step)))
              (inc n)))
          0
          steps))

(defn- execute-single-step
  "Execute a single (non-parallel) step."
  [step ctx opts]
  (let [start-ms  (System/currentTimeMillis)
        step-id   (:step-id step)
        when-expr (:when-expr step)]
    (if (not (evaluate-when when-expr ctx))
      {:success?    true
       :step-id     step-id
       :result      {:skipped true :reason "when-condition false"}
       :duration-ms 0
       :errors      []
       :context     ctx}
      (let [args          (substitute-in-map (:args step) ctx)
            action-result (execute-action (:action step) args ctx opts)
            duration      (- (System/currentTimeMillis) start-ms)
            success?      (:success? action-result)
            new-ctx       (-> ctx
                              (assoc-in [(keyword step-id) :success] success?)
                              (assoc-in [(keyword step-id) :result] (:result action-result)))]
        {:success?    success?
         :step-id     step-id
         :result      (:result action-result)
         :duration-ms duration
         :errors      (vec (or (:errors action-result) []))
         :context     new-ctx}))))

(defn- execute-parallel-group
  "Execute a parallel step group. All branches run concurrently via futures."
  [branches ctx opts]
  (let [futures (mapv (fn [branch]
                        (future (execute-single-step branch ctx opts)))
                      branches)
        results (mapv deref futures)
        all-ok  (every? :success? results)
        merged-ctx (reduce (fn [c r] (merge c (:context r))) ctx results)]
    {:success?    all-ok
     :results     results
     :context     merged-ctx
     :errors      (vec (mapcat :errors results))
     :duration-ms (apply max 0 (map :duration-ms results))}))

(defn- make-init-ctx
  "Build initial context from workflow params and opts."
  [workflow opts]
  (merge (into {} (map (fn [[k v]] [(keyword k) v])
                       (:params workflow)))
         (or (:context opts) {})))

(defn- find-step-by-id
  "Find a step by ID, searching top-level and parallel branches."
  [steps step-id]
  (or (first (filter #(= (:step-id %) step-id) steps))
      (first (mapcat (fn [s]
                       (when (:parallel s)
                         (filter #(= (:step-id %) step-id)
                                 (:parallel s))))
                     steps))))

;;; =============================================================================
;;; Workflow Validation Helper
;;; =============================================================================

(defn- validate-workflow*
  "Core validation logic for a workflow."
  [workflow]
  (let [steps      (:steps workflow)
        step-ids   (collect-step-ids steps)
        flat-steps (reduce (fn [acc step]
                             (if (:parallel step)
                               (into acc (:parallel step))
                               (conj acc step)))
                           []
                           steps)
        dep-graph  (build-dep-graph flat-steps)
        bad-deps   (reduce (fn [errs step]
                             (let [missing (remove step-ids (:dependencies step))]
                               (if (seq missing)
                                 (conj errs (str "Step '" (:step-id step)
                                                 "' references unknown deps: "
                                                 (str/join ", " missing)))
                                 errs)))
                           []
                           flat-steps)
        topo       (when (empty? bad-deps)
                     (topological-sort dep-graph step-ids))
        cycle?     (:cycle-detected topo)
        errors     (cond-> bad-deps
                     cycle? (conj (str "Dependency cycle detected involving: "
                                       (str/join ", " (:nodes topo)))))
        warnings   (cond-> []
                     (empty? steps)
                     (conj "Workflow has no steps")

                     (not (:loaded? workflow))
                     (conj "Workflow was not successfully loaded"))]
    {:valid?           (and (empty? errors) (:loaded? workflow))
     :errors           (vec errors)
     :warnings         (vec warnings)
     :dependency-order (or (:sorted topo) [])}))

;;; =============================================================================
;;; Workflow Execution Core (extracted for clarity)
;;; =============================================================================

(defn- handle-parallel-step
  "Handle a parallel group step within workflow execution."
  [step ctx opts wf-id start-ms total completed results errors fail-fast? on-complete]
  (let [par-result (execute-parallel-group (:parallel step) ctx opts)
        par-count  (count (:parallel step))
        new-results (reduce (fn [r sr] (assoc r (:step-id sr) sr))
                            results
                            (:results par-result))]
    (when on-complete
      (doseq [sr (:results par-result)]
        (on-complete sr)))
    (if (and (not (:success? par-result)) fail-fast?)
      (do
        (update-execution! wf-id {:status       :failed
                                  :completed-at (System/currentTimeMillis)
                                  :errors       (into errors (:errors par-result))})
        {:done    true
         :success false
         :result  {:success?        false
                   :workflow-id     wf-id
                   :steps-completed (+ completed par-count)
                   :steps-total     total
                   :results         new-results
                   :duration-ms     (- (System/currentTimeMillis) start-ms)
                   :errors          (into errors (:errors par-result))
                   :final-context   (:context par-result)}})
      {:done      false
       :ctx       (:context par-result)
       :results   new-results
       :completed (+ completed par-count)
       :errors    (into errors (:errors par-result))})))

(defn- handle-sequential-step
  "Handle a sequential step within workflow execution."
  [step ctx opts wf-id start-ms total completed results errors fail-fast? on-complete]
  (let [step-result (execute-single-step step ctx opts)]
    (when on-complete (on-complete step-result))
    (if (and (not (:success? step-result)) fail-fast?)
      (do
        (update-execution! wf-id {:status       :failed
                                  :completed-at (System/currentTimeMillis)
                                  :errors       (into errors (:errors step-result))})
        {:done    true
         :success false
         :result  {:success?        false
                   :workflow-id     wf-id
                   :steps-completed (inc completed)
                   :steps-total     total
                   :results         (assoc results (:step-id step-result) step-result)
                   :duration-ms     (- (System/currentTimeMillis) start-ms)
                   :errors          (into errors (:errors step-result))
                   :final-context   (:context step-result)}})
      {:done      false
       :ctx       (:context step-result)
       :results   (assoc results (:step-id step-result) step-result)
       :completed (inc completed)
       :errors    (into errors (:errors step-result))})))

(defn- execute-workflow-steps
  "Execute all steps in a validated workflow."
  [steps init-ctx opts wf-id total start-ms]
  (let [fail-fast?  (get opts :fail-fast? true)
        timeout-ms  (get opts :timeout-ms 300000)
        on-complete (:on-step-complete opts)]
    (loop [remaining steps
           ctx       init-ctx
           results   {}
           completed 0
           errors    []]
      (let [elapsed (- (System/currentTimeMillis) start-ms)]
        (cond
          ;; Timeout
          (> elapsed timeout-ms)
          (do
            (update-execution! wf-id {:status       :failed
                                      :completed-at (System/currentTimeMillis)
                                      :errors       (conj errors "Workflow timed out")})
            {:success?        false
             :workflow-id     wf-id
             :steps-completed completed
             :steps-total     total
             :results         results
             :duration-ms     elapsed
             :errors          (conj errors "Workflow timed out")
             :final-context   ctx})

          ;; Cancelled
          (= :cancelled (:status (get @executions wf-id)))
          {:success?        false
           :workflow-id     wf-id
           :steps-completed completed
           :steps-total     total
           :results         results
           :duration-ms     elapsed
           :errors          (conj errors "Workflow cancelled")
           :final-context   ctx}

          ;; Done
          (empty? remaining)
          (do
            (update-execution! wf-id {:status          :completed
                                      :completed-at    (System/currentTimeMillis)
                                      :steps-completed completed
                                      :progress        1.0})
            {:success?        (empty? errors)
             :workflow-id     wf-id
             :steps-completed completed
             :steps-total     total
             :results         results
             :duration-ms     (- (System/currentTimeMillis) start-ms)
             :errors          errors
             :final-context   ctx})

          ;; Execute next step
          :else
          (let [step (first remaining)
                _    (update-execution! wf-id
                                        {:current-step (:step-id step)
                                         :progress     (if (pos? total)
                                                         (double (/ completed total))
                                                         0.0)})
                outcome (if (:parallel step)
                          (handle-parallel-step step ctx opts wf-id start-ms total
                                                completed results errors fail-fast? on-complete)
                          (handle-sequential-step step ctx opts wf-id start-ms total
                                                  completed results errors fail-fast? on-complete))]
            (if (:done outcome)
              (:result outcome)
              (recur (rest remaining)
                     (:ctx outcome)
                     (:results outcome)
                     (:completed outcome)
                     (:errors outcome)))))))))

;;; =============================================================================
;;; YAMLWorkflowEngine Implementation
;;; =============================================================================

(defrecord YAMLWorkflowEngine [action-registry workflow-dir]
  wf/IWorkflowEngine

  (load-workflow [_this workflow-name opts]
    (try
      (let [yaml-str (cond
                       (:yaml opts)
                       (:yaml opts)

                       workflow-dir
                       (let [path (str workflow-dir "/" workflow-name ".yml")]
                         (slurp path))

                       :else
                       nil)
            _        (when-not yaml-str
                       (throw (ex-info "No workflow source"
                                       {:workflow-name workflow-name})))
            parsed   (parse-yaml yaml-str)]
        (if (:parsed? parsed)
          (let [wf-data (normalize-workflow (:data parsed))
                params  (merge (:params wf-data) (:params opts))
                wf-id   (str (:name wf-data) "-" (System/currentTimeMillis))
                steps   (:steps wf-data)]
            {:workflow-id wf-id
             :name        (:name wf-data)
             :steps       steps
             :params      params
             :metadata    {:engine      :yaml
                           :version     (:version wf-data)
                           :description (:description wf-data)
                           :source      workflow-name}
             :loaded?     true
             :errors      []})
          {:workflow-id (str "error-" workflow-name "-" (System/currentTimeMillis))
           :name        workflow-name
           :steps       []
           :metadata    {:engine :yaml}
           :loaded?     false
           :errors      (:errors parsed)}))
      (catch Exception e
        {:workflow-id (str "error-" workflow-name "-" (System/currentTimeMillis))
         :name        workflow-name
         :steps       []
         :metadata    {:engine :yaml}
         :loaded?     false
         :errors      [(str "Load error: " (.getMessage e))]})))

  (validate-workflow [_this workflow]
    (try
      (validate-workflow* workflow)
      (catch Exception e
        {:valid?           false
         :errors           [(str "Validation error: " (.getMessage e))]
         :warnings         []
         :dependency-order []})))

  (execute-step [_this workflow step-id opts]
    (try
      (let [steps (:steps workflow)
            step  (find-step-by-id steps step-id)
            ctx   (make-init-ctx workflow opts)]
        (if step
          (execute-single-step step ctx opts)
          {:success?    false
           :step-id     step-id
           :result      nil
           :duration-ms 0
           :errors      [(str "Step not found: " step-id)]
           :context     ctx}))
      (catch Exception e
        {:success?    false
         :step-id     step-id
         :result      nil
         :duration-ms 0
         :errors      [(str "Step execution error: " (.getMessage e))]
         :context     {}})))

  (execute-workflow [this workflow opts]
    (let [start-ms (System/currentTimeMillis)
          wf-id    (:workflow-id workflow)
          steps    (:steps workflow)
          total    (count-all-steps steps)
          init-ctx (make-init-ctx workflow opts)]
      ;; Register execution state
      (new-execution! wf-id (:name workflow) total)
      (update-execution! wf-id {:status     :running
                                :started-at (System/currentTimeMillis)})
      ;; Validate first
      (let [validation (wf/validate-workflow this workflow)]
        (if (not (:valid? validation))
          (do
            (update-execution! wf-id {:status       :failed
                                      :completed-at (System/currentTimeMillis)
                                      :errors       (:errors validation)})
            {:success?        false
             :workflow-id     wf-id
             :steps-completed 0
             :steps-total     total
             :results         {}
             :duration-ms     (- (System/currentTimeMillis) start-ms)
             :errors          (:errors validation)
             :final-context   init-ctx})
          (execute-workflow-steps steps init-ctx opts wf-id total start-ms)))))

  (get-status [_this workflow-id]
    (get @executions workflow-id))

  (cancel-workflow [_this workflow-id opts]
    (try
      (if-let [exec (get @executions workflow-id)]
        (if (#{:completed :cancelled :failed} (:status exec))
          {:success?        true
           :workflow-id     workflow-id
           :status          (:status exec)
           :steps-completed (:steps-completed exec)
           :errors          []}
          (do
            (update-execution! workflow-id
                               {:status       :cancelled
                                :completed-at (System/currentTimeMillis)
                                :errors       (conj (:errors exec)
                                                    (str "Cancelled: "
                                                         (or (:reason opts) "no reason")))})
            {:success?        true
             :workflow-id     workflow-id
             :status          :cancelled
             :steps-completed (:steps-completed exec)
             :errors          []}))
        {:success?        false
         :workflow-id     workflow-id
         :status          :unknown
         :steps-completed 0
         :errors          [(str "Unknown workflow: " workflow-id)]})
      (catch Exception e
        {:success?        false
         :workflow-id     workflow-id
         :status          :unknown
         :steps-completed 0
         :errors          [(str "Cancel error: " (.getMessage e))]}))))

;;; =============================================================================
;;; Constructor
;;; =============================================================================

(defn create-yaml-engine
  "Create a new YAMLWorkflowEngine.

   Options:
     :workflow-dir     - Directory to load .yml workflow files from
     :action-registry  - Additional action map (keyword -> fn)

   Usage:
     (create-yaml-engine {:workflow-dir \"workflows/\"})
     (set-workflow-engine! (create-yaml-engine {}))"
  ([]
   (create-yaml-engine {}))
  ([opts]
   (->YAMLWorkflowEngine
    (or (:action-registry opts) {})
    (:workflow-dir opts))))

;;; =============================================================================
;;; Utility: Clear execution state (for testing)
;;; =============================================================================

(defn clear-executions!
  "Clear all execution state. Primarily for testing."
  []
  (reset! executions {})
  nil)
