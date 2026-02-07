(ns hive-mcp.protocols.workflow
  "Protocol definitions for workflow engines.

   IWorkflowEngine defines the core interface for loading, validating,
   executing, and managing workflows. Workflows are multi-step processes
   that can be executed step-by-step or as a unit.

   Architecture:
   - IWorkflowEngine: Core workflow lifecycle (load, validate, execute, cancel)
   - IWorkflowPersistence: Optional extension for durable workflow state
   - NoopWorkflowEngine: No-op fallback when no engine is configured
   - Active engine atom with set!/get/clear pattern

   Relationship to existing infrastructure:
   - workflows/router.clj: Decides WHERE to execute (native vs elisp)
   - workflows/hooks.clj: Emits lifecycle events (before/after)
   - plan/schema.clj: Defines plan step structures
   - IWorkflowEngine: HOW to execute (the engine itself)

   Use Cases:
   - Catchup/wrap workflows with step-by-step execution
   - Plan execution with dependency resolution
   - Long-running workflows with cancellation support
   - Custom workflow engines (elisp-backed, native, distributed)

   Usage:
     ;; Set active engine
     (set-workflow-engine! (my-engine-impl))

     ;; Load and execute
     (let [engine (get-workflow-engine)
           wf     (load-workflow engine \"catchup\" {:scope \"hive-mcp\"})]
       (when (:valid? (validate-workflow engine wf))
         (execute-workflow engine wf {:agent-id \"ling-1\"})))

     ;; Check status / cancel
     (get-status engine workflow-id)
     (cancel-workflow engine workflow-id {:reason \"user-requested\"})

   SOLID-O: Open for extension via new engine implementations.
   SOLID-D: Depend on IWorkflowEngine abstraction, not concretions.
   CLARITY-L: Layers stay pure - protocol boundary between
              workflow domain logic and engine implementation.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; IWorkflowEngine Protocol
;;; =============================================================================

(defprotocol IWorkflowEngine
  "Protocol for workflow execution engines.

   A workflow engine manages the full lifecycle of workflows:
   loading definitions, validating them, executing steps individually
   or as a batch, querying status, and supporting cancellation.

   Implementations:
   - NoopWorkflowEngine: No-op fallback (always available)
   - NativeWorkflowEngine: Clojure-native execution
   - ElispWorkflowEngine: Delegates to Emacs elisp workflows
   - DistributedWorkflowEngine: Delegates steps to swarm agents

   Workflow lifecycle:
     load-workflow → validate-workflow → execute-workflow → get-status
                                       → execute-step (granular)
                                       → cancel-workflow (abort)"

  (load-workflow [this workflow-name opts]
    "Load a workflow definition by name.

     Arguments:
       workflow-name - String identifier for the workflow (e.g., \"catchup\", \"wrap\")
       opts          - Map with optional keys:
                       :scope     - Project scope for the workflow
                       :params    - Workflow-specific parameters
                       :version   - Specific workflow version (default: latest)

     Returns map with:
       :workflow-id  - Unique ID for this workflow instance
       :name         - Workflow name
       :steps        - Ordered sequence of step definitions, each with:
                       :step-id, :title, :description, :dependencies
       :metadata     - Workflow metadata (author, version, created-at)
       :loaded?      - Boolean indicating successful load
       :errors       - Vector of error messages (empty on success)

     CLARITY-Y: Must not throw - return :loaded? false on failure.")

  (validate-workflow [this workflow]
    "Validate a loaded workflow definition.

     Checks:
     - All required fields are present
     - Step dependencies form a DAG (no cycles)
     - Referenced resources exist
     - Parameters match expected schema

     Arguments:
       workflow - Workflow map as returned by load-workflow

     Returns map with:
       :valid?       - Boolean indicating validity
       :errors       - Vector of validation errors (empty if valid)
       :warnings     - Vector of non-fatal warnings
       :dependency-order - Topologically sorted step IDs (if valid)

     Should be called after load-workflow and before execution.")

  (execute-step [this workflow step-id opts]
    "Execute a single step within a workflow.

     Arguments:
       workflow - Workflow map (from load-workflow)
       step-id  - ID of the step to execute
       opts     - Execution options:
                  :agent-id   - Agent executing the step
                  :timeout-ms - Step timeout (default: 60000)
                  :dry-run?   - If true, simulate without side effects
                  :context    - Map of data from prior steps

     Returns map with:
       :success?    - Boolean indicating step success
       :step-id     - The executed step ID
       :result      - Step output data (step-specific)
       :duration-ms - Execution time in milliseconds
       :errors      - Vector of error messages (empty on success)
       :context     - Updated context map for subsequent steps

     CLARITY-Y: Must not throw - return :success? false on failure.
     Steps should be idempotent where possible.")

  (execute-workflow [this workflow opts]
    "Execute all steps in a workflow respecting dependency order.

     Arguments:
       workflow - Workflow map (from load-workflow)
       opts     - Execution options:
                  :agent-id     - Agent executing the workflow
                  :timeout-ms   - Total workflow timeout (default: 300000)
                  :dry-run?     - If true, simulate without side effects
                  :on-step-complete - Callback fn called after each step
                  :fail-fast?   - Stop on first step failure (default: true)
                  :context      - Initial context map

     Returns map with:
       :success?     - Boolean indicating overall success
       :workflow-id  - Workflow instance ID
       :steps-completed - Count of successfully completed steps
       :steps-total  - Total number of steps
       :results      - Map of step-id -> step result
       :duration-ms  - Total execution time
       :errors       - Vector of workflow-level errors
       :final-context - Accumulated context from all steps

     Executes steps in topological order (from validate-workflow).
     Respects step dependencies - a step only runs when all its
     dependencies have completed successfully.")

  (get-status [this workflow-id]
    "Get the current status of a workflow execution.

     Arguments:
       workflow-id - Unique workflow instance ID (from load-workflow)

     Returns map with:
       :workflow-id     - The queried workflow ID
       :name            - Workflow name
       :status          - Current status keyword:
                          :pending, :running, :completed, :failed, :cancelled
       :current-step    - ID of currently executing step (nil if not running)
       :steps-completed - Count of completed steps
       :steps-total     - Total step count
       :started-at      - Execution start timestamp (nil if pending)
       :completed-at    - Completion timestamp (nil if not done)
       :errors          - Vector of accumulated errors
       :progress        - Float 0.0-1.0 indicating progress

     Returns nil if workflow-id is unknown.")

  (cancel-workflow [this workflow-id opts]
    "Cancel a running workflow.

     Arguments:
       workflow-id - Unique workflow instance ID
       opts        - Cancellation options:
                     :reason    - Why the workflow is being cancelled
                     :force?    - Force immediate cancellation (default: false)
                     :cleanup?  - Run cleanup hooks (default: true)

     Returns map with:
       :success?       - Boolean indicating successful cancellation
       :workflow-id    - The cancelled workflow ID
       :status         - Final status after cancellation (:cancelled)
       :steps-completed - Steps that completed before cancellation
       :errors         - Vector of cancellation errors

     Idempotent - safe to call on already cancelled/completed workflows.
     CLARITY-Y: Must not throw - return :success? false on failure."))

;;; =============================================================================
;;; IWorkflowPersistence Protocol (Extension)
;;; =============================================================================

(defprotocol IWorkflowPersistence
  "Optional extension protocol for durable workflow state.

   Enables workflow engines to persist execution state for:
   - Resuming workflows after restart
   - Auditing workflow execution history
   - Querying past workflow runs

   Not all engines need this - in-memory engines can skip it."

  (save-state [this workflow-id state]
    "Persist current workflow execution state.

     Arguments:
       workflow-id - Workflow instance ID
       state       - Complete workflow state map

     Returns map with:
       :success? - Boolean indicating save success
       :errors   - Vector of error messages")

  (load-state [this workflow-id]
    "Load persisted workflow state.

     Arguments:
       workflow-id - Workflow instance ID

     Returns:
       Workflow state map, or nil if not found.")

  (list-workflows [this opts]
    "List workflow executions matching criteria.

     Arguments:
       opts - Filter options:
              :status  - Filter by status keyword
              :name    - Filter by workflow name
              :since   - Only workflows started after this timestamp
              :limit   - Maximum results (default: 50)

     Returns vector of workflow summary maps."))

;;; =============================================================================
;;; NoopWorkflowEngine (No-Op Fallback Implementation)
;;; =============================================================================

(defrecord NoopWorkflowEngine []
  IWorkflowEngine

  (load-workflow [_ workflow-name _opts]
    {:workflow-id (str "noop-" workflow-name "-" (System/currentTimeMillis))
     :name workflow-name
     :steps []
     :metadata {:engine :noop}
     :loaded? false
     :errors ["NoopWorkflowEngine: No workflow engine configured. Set one via set-workflow-engine!"]})

  (validate-workflow [_ _workflow]
    {:valid? false
     :errors ["NoopWorkflowEngine: No workflow engine configured."]
     :warnings []
     :dependency-order []})

  (execute-step [_ _workflow step-id _opts]
    {:success? false
     :step-id step-id
     :result nil
     :duration-ms 0
     :errors ["NoopWorkflowEngine: No workflow engine configured."]
     :context {}})

  (execute-workflow [_ workflow _opts]
    {:success? false
     :workflow-id (:workflow-id workflow)
     :steps-completed 0
     :steps-total (count (:steps workflow))
     :results {}
     :duration-ms 0
     :errors ["NoopWorkflowEngine: No workflow engine configured."]
     :final-context {}})

  (get-status [_ workflow-id]
    {:workflow-id workflow-id
     :name nil
     :status :unknown
     :current-step nil
     :steps-completed 0
     :steps-total 0
     :started-at nil
     :completed-at nil
     :errors ["NoopWorkflowEngine: No workflow engine configured."]
     :progress 0.0})

  (cancel-workflow [_ workflow-id _opts]
    {:success? false
     :workflow-id workflow-id
     :status :unknown
     :steps-completed 0
     :errors ["NoopWorkflowEngine: No workflow engine configured."]}))

;;; =============================================================================
;;; Active Engine Management
;;; =============================================================================

;; Atom holding the currently active IWorkflowEngine implementation.
;; Defaults to NoopWorkflowEngine (no-op fallback).
(defonce ^:private active-engine (atom nil))

(defn set-workflow-engine!
  "Set the active workflow engine implementation.

   Arguments:
     engine - Implementation of IWorkflowEngine protocol

   Returns:
     The engine.

   Throws:
     AssertionError if engine doesn't satisfy protocol."
  [engine]
  {:pre [(satisfies? IWorkflowEngine engine)]}
  (reset! active-engine engine)
  engine)

(defn get-workflow-engine
  "Get the active workflow engine implementation.

   Returns NoopWorkflowEngine if no engine is set.
   This ensures hive-mcp always works, with or without a real engine."
  []
  (or @active-engine
      (->NoopWorkflowEngine)))

(defn workflow-engine-set?
  "Check if an active workflow engine is configured.

   Returns:
     true if set-workflow-engine! has been called."
  []
  (some? @active-engine))

(defn clear-workflow-engine!
  "Clear the active workflow engine. Used for testing.

   Returns nil."
  []
  (reset! active-engine nil)
  nil)

;;; =============================================================================
;;; Utility Functions
;;; =============================================================================

(defn workflow-engine?
  "Check if object implements IWorkflowEngine protocol."
  [x]
  (satisfies? IWorkflowEngine x))

(defn persistent-engine?
  "Check if workflow engine supports persistence."
  [x]
  (satisfies? IWorkflowPersistence x))

(defn enhanced?
  "Check if enhanced workflow capabilities are available.

   Returns:
     true if a non-noop IWorkflowEngine implementation is active."
  []
  (and (workflow-engine-set?)
       (not (instance? NoopWorkflowEngine @active-engine))))

(defn capabilities
  "Get a summary of available workflow capabilities.

   Returns:
     Map describing what features are available."
  []
  (let [engine (get-workflow-engine)]
    {:engine-type (if (enhanced?)
                    (-> engine class .getSimpleName)
                    :noop)
     :enhanced? (enhanced?)
     :load? true            ;; Always available (may be no-op)
     :validate? true
     :execute-step? true
     :execute-workflow? true
     :get-status? true
     :cancel? true
     :persistence? (persistent-engine? engine)}))
