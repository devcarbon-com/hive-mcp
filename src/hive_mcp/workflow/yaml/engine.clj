(ns hive-mcp.workflow.yaml.engine
  "YAMLWorkflowEngine record implementing IWorkflowEngine protocol.

   This is the top-level engine that wires together:
   - YAML parsing (parser ns)
   - Dependency validation (toposort ns)
   - Step/workflow execution (executor ns)

   SOLID-S: Engine record only handles protocol dispatch.
   SOLID-O: Open for extension via :action-registry."
  (:require [hive-mcp.protocols.workflow :as wf]
            [hive-mcp.workflow.yaml.parser :as parser]
            [hive-mcp.workflow.yaml.toposort :as toposort]
            [hive-mcp.workflow.yaml.executor :as executor]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
            parsed   (parser/parse-yaml yaml-str)]
        (if (:parsed? parsed)
          (let [wf-data (parser/normalize-workflow (:data parsed))
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
      (toposort/validate-dependencies workflow)
      (catch Exception e
        {:valid?           false
         :errors           [(str "Validation error: " (.getMessage e))]
         :warnings         []
         :dependency-order []})))

  (execute-step [_this workflow step-id opts]
    (try
      (let [steps (:steps workflow)
            step  (executor/find-step-by-id steps step-id)
            ctx   (executor/make-init-ctx workflow opts)]
        (if step
          (executor/execute-single-step step ctx opts)
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
          total    (executor/count-all-steps steps)
          init-ctx (executor/make-init-ctx workflow opts)]
      ;; Register execution state
      (executor/new-execution! wf-id (:name workflow) total)
      (executor/update-execution! wf-id {:status     :running
                                         :started-at (System/currentTimeMillis)})
      ;; Validate first
      (let [validation (wf/validate-workflow this workflow)]
        (if (not (:valid? validation))
          (do
            (executor/update-execution! wf-id {:status       :failed
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
          (executor/execute-workflow-steps steps init-ctx opts wf-id total start-ms)))))

  (get-status [_this workflow-id]
    (executor/get-execution workflow-id))

  (cancel-workflow [_this workflow-id opts]
    (try
      (if-let [exec (executor/get-execution workflow-id)]
        (if (#{:completed :cancelled :failed} (:status exec))
          {:success?        true
           :workflow-id     workflow-id
           :status          (:status exec)
           :steps-completed (:steps-completed exec)
           :errors          []}
          (do
            (executor/update-execution! workflow-id
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
