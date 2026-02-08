(ns hive-mcp.workflow.yaml.executor
  "Workflow execution engine for YAML workflows.

   Manages:
   - Execution state lifecycle (pending -> running -> completed/failed/cancelled)
   - Single step execution with condition evaluation
   - Parallel branch execution via futures
   - Full workflow execution loop with timeout, cancellation, fail-fast
   - Step output propagation via context

   CLARITY-I: Full introspection via execution state atom.
   CLARITY-Y: Never throws - all errors captured in result maps."
  (:require [hive-mcp.workflow.yaml.vars :as vars]
            [hive-mcp.workflow.yaml.actions :as actions]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Condition Evaluation
;;; =============================================================================

(defn evaluate-when
  "Evaluate a :when expression against context."
  [when-expr ctx]
  (if (nil? when-expr)
    true
    (let [resolved (vars/substitute-vars when-expr ctx)]
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

(defn get-execution
  "Get execution state for a workflow-id."
  [workflow-id]
  (get @executions workflow-id))

(defn new-execution!
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

(defn update-execution!
  "Update an execution's state."
  [workflow-id updates]
  (swap! executions update workflow-id merge updates))

(defn clear-executions!
  "Clear all execution state. Primarily for testing."
  []
  (reset! executions {})
  nil)

;;; =============================================================================
;;; Step Counting
;;; =============================================================================

(defn count-all-steps
  "Count total steps including parallel branches."
  [steps]
  (reduce (fn [n step]
            (if (:parallel step)
              (+ n (count (:parallel step)))
              (inc n)))
          0
          steps))

;;; =============================================================================
;;; Step Execution
;;; =============================================================================

(defn execute-single-step
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
      (let [args          (vars/substitute-in-map (:args step) ctx)
            action-result (actions/execute-action (:action step) args ctx opts)
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

(defn execute-parallel-group
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

;;; =============================================================================
;;; Context Helpers
;;; =============================================================================

(defn make-init-ctx
  "Build initial context from workflow params and opts."
  [workflow opts]
  (merge (into {} (map (fn [[k v]] [(keyword k) v])
                       (:params workflow)))
         (or (:context opts) {})))

(defn find-step-by-id
  "Find a step by ID, searching top-level and parallel branches."
  [steps step-id]
  (or (first (filter #(= (:step-id %) step-id) steps))
      (first (mapcat (fn [s]
                       (when (:parallel s)
                         (filter #(= (:step-id %) step-id)
                                 (:parallel s))))
                     steps))))

;;; =============================================================================
;;; Workflow Execution Loop
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

(defn execute-workflow-steps
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
          (= :cancelled (:status (get-execution wf-id)))
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
