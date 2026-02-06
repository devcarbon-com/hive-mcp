(ns hive-mcp.agent.ling
  "Ling agent implementation - Claude Code instances with tool chaining.

   Lings are persistent agents that:
   - Run as Claude Code subprocesses
   - Can chain multiple tool calls
   - Maintain session context
   - Coordinate via hivemind
   - Delegate to drones for file mutations

   Supports two spawn modes:
   - :vterm   (default) - Spawned inside Emacs vterm buffer (visual, interactive)
   - :headless          - Spawned as OS process without Emacs (stdout ring buffer)

   Implements IAgent protocol for unified lifecycle management."
  (:require [hive-mcp.agent.protocol :refer [IAgent]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.swarm.datascript.claims :as ds-claims]
            [hive-mcp.swarm.datascript.schema :as schema]
            [hive-mcp.tools.swarm.core :as swarm-core]
            [hive-mcp.emacsclient :as ec]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Ling Record - IAgent Implementation
;;; =============================================================================

;; Forward declare for use in spawn! method (creates new ling for task dispatch)
(declare ->ling)

;;; =============================================================================
;;; Ling Record - IAgent Implementation (Dual-Mode: vterm + headless)
;;; =============================================================================

(defrecord Ling [id cwd presets project-id spawn-mode model]
  IAgent

  (spawn! [this opts]
    "Spawn a ling in the configured spawn-mode.

     :vterm mode (default):
       Uses emacsclient to invoke Emacs-side spawn function.
       Creates a new Claude Code process inside a vterm buffer.
       Only available for claude model (default).

     :headless mode:
       Uses ProcessBuilder to spawn `claude` CLI as a child process.
       Stdout/stderr captured to ring buffers. No Emacs required.
       When model is non-claude (e.g., OpenRouter model), automatically
       forces headless mode since vterm requires Claude Code CLI.

     Options:
       :task      - Initial task to dispatch (optional)
       :presets   - Override presets (optional)
       :depth     - Hierarchy depth (default: 1)
       :parent    - Parent slave-id (optional)
       :spawn-mode - Override spawn mode (optional)
       :model     - Override model (optional)"
    (let [;; Non-claude models must spawn headless (no vterm support)
          effective-model (or (:model opts) model)
          non-claude? (and effective-model
                           (not (schema/claude-model? effective-model)))
          mode (if non-claude?
                 :headless
                 (or (:spawn-mode opts) spawn-mode :vterm))]
      (case mode
        ;; === HEADLESS MODE: ProcessBuilder-based spawn ===
        :headless
        (let [{:keys [task depth parent kanban-task-id]
               :or {depth 1}} opts
              result (headless/spawn-headless! id {:cwd cwd
                                                   :task task
                                                   :presets (or (:presets opts) presets)
                                                   :model effective-model
                                                   :buffer-capacity (or (:buffer-capacity opts) 5000)})]
          (log/info "Ling spawned headless" {:id id :pid (:pid result) :cwd cwd
                                             :model (or effective-model "claude")})
          ;; Register in DataScript with headless metadata
          (ds-lings/add-slave! id {:status :idle
                                   :depth depth
                                   :parent parent
                                   :presets (or (:presets opts) presets)
                                   :cwd cwd
                                   :project-id project-id
                                   :kanban-task-id kanban-task-id})
          ;; Store headless-specific attributes + model
          (ds-lings/update-slave! id (cond-> {:ling/spawn-mode :headless
                                              :ling/process-pid (:pid result)
                                              :ling/process-alive? true}
                                       effective-model
                                       (assoc :ling/model effective-model)))
          ;; Dispatch initial task if provided
          (when task
            (let [headless-ling (->ling id {:cwd cwd
                                            :presets (or (:presets opts) presets)
                                            :project-id project-id
                                            :spawn-mode :headless
                                            :model effective-model})]
              (.dispatch! headless-ling {:task task})))
          id)

        ;; === VTERM MODE: Emacs-based spawn (existing behavior) ===
        ;; Vterm only supports Claude Code CLI (model must be claude/nil)
        ;; :vterm or default
        (let [{:keys [task depth parent kanban-task-id terminal]
               :or {depth 1}} opts
              preset-list (or (:presets opts) presets)
              preset-str (or (swarm-core/format-elisp-list preset-list pr-str) "nil")
              elisp-code (format "(hive-mcp-swarm-api-spawn \"%s\" %s %s %s %s nil)"
                                 id
                                 preset-str
                                 (if cwd (format "\"%s\"" cwd) "nil")
                                 (if terminal (format "\"%s\"" terminal) "nil")
                                 (if kanban-task-id (format "\"%s\"" kanban-task-id) "nil"))
              result (ec/eval-elisp-with-timeout elisp-code 10000)]
          (if (:success result)
            (let [elisp-slave-id (str/trim (or (:result result) id))]
              (log/info "Ling spawned via elisp" {:requested-id id
                                                  :elisp-slave-id elisp-slave-id})
              (ds-lings/add-slave! elisp-slave-id {:status :idle
                                                   :depth depth
                                                   :parent parent
                                                   :presets (or (:presets opts) presets)
                                                   :cwd cwd
                                                   :project-id project-id
                                                   :kanban-task-id kanban-task-id
                                                   :requested-id id})
              ;; Store vterm spawn mode + model (always claude for vterm)
              (ds-lings/update-slave! elisp-slave-id {:ling/spawn-mode :vterm
                                                      :ling/model (or effective-model "claude")})
              ;; Dispatch initial task if provided
              (when task
                (let [elisp-ling (->ling elisp-slave-id {:cwd cwd
                                                         :presets (or (:presets opts) presets)
                                                         :project-id project-id
                                                         :model effective-model})]
                  (.dispatch! elisp-ling {:task task})))
              elisp-slave-id)
            (do
              (log/error "Failed to spawn ling via elisp" {:id id :error (:error result)})
              (throw (ex-info "Failed to spawn ling"
                              {:id id :error (:error result)}))))))))

  (dispatch! [this task-opts]
    "Dispatch a task to this ling.

     Headless mode: writes to stdin pipe.
     Vterm mode: sends via emacsclient elisp.

     Options:
       :task       - Task description (required)
       :files      - Files to claim (optional)
       :priority   - Task priority (default: :normal)
       :timeout-ms - Dispatch timeout (default: 60000)"
    (let [{:keys [task files timeout-ms]
           :or {timeout-ms 60000}} task-opts
          task-id (str "task-" (System/currentTimeMillis) "-" (subs id 0 (min 8 (count id))))
          mode (or spawn-mode
                   ;; Check DataScript for spawn mode if not on record
                   (when-let [slave (ds-queries/get-slave id)]
                     (:ling/spawn-mode slave))
                   :vterm)]
      ;; Common: Update status and register task
      (ds-lings/update-slave! id {:slave/status :working})
      (ds-lings/add-task! task-id id {:status :dispatched
                                      :prompt task
                                      :files files})
      (when (seq files)
        (.claim-files! this files task-id))

      (case mode
        ;; === HEADLESS: dispatch via stdin pipe ===
        :headless
        (try
          (headless/dispatch-via-stdin! id task)
          (log/info "Task dispatched to headless ling via stdin"
                    {:ling-id id :task-id task-id :files files})
          task-id
          (catch Exception e
            (log/error "Failed to dispatch to headless ling"
                       {:ling-id id :task-id task-id :error (ex-message e)})
            (ds-lings/update-task! task-id {:status :failed
                                            :error (ex-message e)})
            (throw (ex-info "Failed to dispatch to headless ling"
                            {:ling-id id :task-id task-id :error (ex-message e)}
                            e))))

        ;; === VTERM: dispatch via elisp (existing behavior) ===
        ;; :vterm or default
        (let [escaped-prompt (-> task
                                 (str/replace "\\" "\\\\")
                                 (str/replace "\"" "\\\"")
                                 (str/replace "\n" "\\n"))
              elisp-code (format "(hive-mcp-swarm-api-dispatch \"%s\" \"%s\" %d)"
                                 id
                                 escaped-prompt
                                 timeout-ms)
              result (ec/eval-elisp-with-timeout elisp-code timeout-ms)]
          (if (:success result)
            (do
              (log/info "Task dispatched to ling via elisp"
                        {:ling-id id :task-id task-id :files files})
              task-id)
            (do
              (log/error "Failed to dispatch to ling via elisp"
                         {:ling-id id :task-id task-id :error (:error result)})
              (ds-lings/update-task! task-id {:status :failed
                                              :error (:error result)})
              (throw (ex-info "Failed to dispatch to ling"
                              {:ling-id id
                               :task-id task-id
                               :error (:error result)}))))))))

  (status [this]
    "Get current ling status from DataScript with mode-appropriate liveness check.

     Headless: checks process registry for liveness.
     Vterm: checks elisp for liveness (existing behavior)."
    (let [ds-status (ds-queries/get-slave id)
          mode (or spawn-mode
                   (:ling/spawn-mode ds-status)
                   :vterm)]
      (case mode
        ;; === HEADLESS: check process registry ===
        :headless
        (let [headless-info (headless/headless-status id)]
          (if ds-status
            (cond-> ds-status
              headless-info (assoc :headless-alive? (:alive? headless-info)
                                   :headless-pid (:pid headless-info)
                                   :headless-uptime-ms (:uptime-ms headless-info)
                                   :headless-stdout (:stdout headless-info)
                                   :headless-stderr (:stderr headless-info)))
            ;; Fallback to headless-only status
            (when headless-info
              {:slave/id id
               :slave/status (if (:alive? headless-info) :idle :dead)
               :ling/spawn-mode :headless
               :headless-alive? (:alive? headless-info)
               :headless-pid (:pid headless-info)})))

        ;; === VTERM: elisp fallback (existing behavior) ===
        (let [elisp-result (when (or (nil? ds-status)
                                     (= :unknown (:slave/status ds-status)))
                             (ec/eval-elisp-with-timeout
                              (format "(hive-mcp-swarm-get-slave-status \"%s\")" id)
                              3000))]
          (if ds-status
            (cond-> ds-status
              (and (:success elisp-result)
                   (not= (:result elisp-result) "nil"))
              (assoc :elisp-alive? true))
            (when (:success elisp-result)
              {:slave/id id
               :slave/status (if (= (:result elisp-result) "nil")
                               :dead
                               :unknown)
               :elisp-raw (:result elisp-result)}))))))

  (kill! [this]
    "Terminate the ling and release resources.

     Headless: kills OS process via Process.destroy.
     Vterm: kills via emacsclient elisp.

     Both modes check critical operations before killing."
    (let [{:keys [can-kill? blocking-ops]} (ds-lings/can-kill? id)
          mode (or spawn-mode
                   (when-let [slave (ds-queries/get-slave id)]
                     (:ling/spawn-mode slave))
                   :vterm)]
      (if can-kill?
        (do
          ;; Release claims first (common to both modes)
          (.release-claims! this)
          (case mode
            ;; === HEADLESS: kill OS process ===
            :headless
            (try
              (let [result (headless/kill-headless! id)]
                ;; Remove from DataScript
                (ds-lings/remove-slave! id)
                (log/info "Headless ling killed" {:id id :pid (:pid result)})
                {:killed? true :id id :pid (:pid result)})
              (catch Exception e
                ;; Process might already be dead - still clean up DataScript
                (log/warn "Headless kill exception, cleaning up DataScript"
                          {:id id :error (ex-message e)})
                (ds-lings/remove-slave! id)
                {:killed? true :id id :reason :process-already-dead}))

            ;; === VTERM: kill via elisp (existing behavior) ===
            (let [elisp-result (ec/eval-elisp-with-timeout
                                (format "(hive-mcp-swarm-slaves-kill \"%s\")" id)
                                5000)
                  kill-succeeded? (and (:success elisp-result)
                                       (not (nil? (:result elisp-result)))
                                       (not= "nil" (:result elisp-result)))]
              (if kill-succeeded?
                (do
                  (ds-lings/remove-slave! id)
                  (log/info "Ling killed" {:id id})
                  {:killed? true :id id})
                (do
                  (log/warn "Elisp kill failed - NOT removing from DataScript"
                            {:id id :elisp-result elisp-result})
                  {:killed? false :id id :reason :elisp-kill-failed})))))
        (do
          (log/warn "Cannot kill ling - critical ops in progress"
                    {:id id :blocking-ops blocking-ops})
          {:killed? false
           :reason :critical-ops-blocking
           :blocking-ops blocking-ops}))))

  (agent-type [_]
    :ling)

  (can-chain-tools? [_]
    "Lings can chain multiple tool calls in a single turn."
    true)

  (claims [this]
    "Get list of files currently claimed by this ling.

     Queries DataScript for all claims where :claim/slave = this ling."
    (let [all-claims (ds-queries/get-all-claims)]
      (->> all-claims
           (filter #(= id (:slave-id %)))
           (map :file)
           vec)))

  (claim-files! [this files task-id]
    "Claim files for exclusive access during task.

     Claims prevent other agents from modifying the same files.
     Uses DataScript claims with TTL for stale detection."
    (when (seq files)
      (doseq [f files]
        ;; Check for conflicts first
        (let [{:keys [conflict? held-by]} (ds-queries/has-conflict? f id)]
          (if conflict?
            (do
              (log/warn "File already claimed by another agent"
                        {:file f :held-by held-by :requesting id})
              ;; Add to wait queue instead of claiming
              (ds-claims/add-to-wait-queue! id f))
            ;; No conflict - claim the file
            (ds-lings/claim-file! f id task-id))))
      (log/info "Files claimed" {:ling-id id :count (count files)})))

  (release-claims! [this]
    "Release all file claims held by this ling."
    (let [released-count (ds-lings/release-claims-for-slave! id)]
      (log/info "Released claims" {:ling-id id :count released-count})
      released-count))

  (upgrade! [_]
    "No-op for lings - they already have full capabilities."
    nil))

;;; =============================================================================
;;; Factory Functions
;;; =============================================================================

(defn ->ling
  "Create a new Ling agent instance.

   Arguments:
     id   - Unique identifier for this ling
     opts - Map with optional keys:
            :cwd        - Working directory
            :presets    - Collection of preset names
            :project-id - Project ID for scoping
            :spawn-mode - :vterm (default) or :headless
            :model      - Model identifier (default: 'claude' = Claude Code CLI)
                          Non-claude models (e.g., 'deepseek/deepseek-v3.2')
                          automatically force :headless spawn-mode.

   Returns:
     Ling record implementing IAgent protocol

   Example:
     (->ling \"ling-123\" {:cwd \"/project\"
                          :presets [\"coordinator\"]
                          :project-id \"hive-mcp\"})

     ;; Headless mode (no Emacs required):
     (->ling \"ling-456\" {:cwd \"/project\"
                          :spawn-mode :headless})

     ;; OpenRouter model (automatically headless):
     (->ling \"ling-789\" {:cwd \"/project\"
                          :model \"deepseek/deepseek-v3.2\"})"
  [id opts]
  (let [model-val (:model opts)
        ;; Non-claude models force headless mode
        effective-spawn-mode (if (and model-val (not (schema/claude-model? model-val)))
                               :headless
                               (:spawn-mode opts :vterm))]
    (map->Ling {:id id
                :cwd (:cwd opts)
                :presets (:presets opts [])
                :project-id (:project-id opts)
                :spawn-mode effective-spawn-mode
                :model model-val})))

(defn create-ling!
  "Create and spawn a new ling agent.

   Convenience function that creates the Ling record and spawns it.

   Arguments:
     id   - Unique identifier
     opts - Spawn options (see spawn! and ->ling)

   Returns:
     The ling ID on success, throws on failure"
  [id opts]
  (let [ling (->ling id opts)]
    (.spawn! ling opts)))

;;; =============================================================================
;;; Ling Query Functions
;;; =============================================================================

(defn get-ling
  "Get a ling by ID as a Ling record.

   Reconstitutes the Ling record from DataScript state,
   including spawn-mode and model for proper dispatch routing.

   Returns:
     Ling record or nil if not found"
  [id]
  (when-let [slave (ds-queries/get-slave id)]
    (->ling id {:cwd (:slave/cwd slave)
                :presets (:slave/presets slave)
                :project-id (:slave/project-id slave)
                :spawn-mode (or (:ling/spawn-mode slave) :vterm)
                :model (:ling/model slave)})))

(defn list-lings
  "List all lings, optionally filtered by project-id.

   Arguments:
     project-id - Optional project ID filter

   Returns:
     Seq of Ling records"
  [& [project-id]]
  (let [slaves (if project-id
                 (ds-queries/get-slaves-by-project project-id)
                 (ds-queries/get-all-slaves))]
    (->> slaves
         ;; Filter to depth 1 (lings, not drones)
         (filter #(= 1 (:slave/depth %)))
         (map (fn [s]
                (->ling (:slave/id s)
                        {:cwd (:slave/cwd s)
                         :presets (:slave/presets s)
                         :project-id (:slave/project-id s)
                         :spawn-mode (or (:ling/spawn-mode s) :vterm)
                         :model (:ling/model s)}))))))

(defn get-ling-for-task
  "Get the ling assigned to a kanban task.

   Arguments:
     kanban-task-id - Kanban task ID

   Returns:
     Ling record or nil"
  [kanban-task-id]
  (when-let [slave (ds-queries/get-slave-by-kanban-task kanban-task-id)]
    (->ling (:slave/id slave)
            {:cwd (:slave/cwd slave)
             :presets (:slave/presets slave)
             :project-id (:slave/project-id slave)
             :spawn-mode (or (:ling/spawn-mode slave) :vterm)
             :model (:ling/model slave)})))

;;; =============================================================================
;;; Critical Operations Guard (Delegating to ds-lings)
;;; =============================================================================

(defn with-critical-op
  "Execute body while holding a critical operation guard.

   Prevents swarm_kill from terminating the ling during critical ops.
   Wraps ds-lings/with-critical-op.

   Usage:
     (with-critical-op ling-id :wrap
       (do-wrap-stuff))"
  [ling-id op-type body-fn]
  (ds-lings/with-critical-op ling-id op-type
    (body-fn)))

(comment
  ;; Usage examples

  ;; === Vterm mode (default, requires Emacs) ===
  (def my-ling (->ling "ling-001" {:cwd "/home/user/project"
                                   :presets ["coordinator"]
                                   :project-id "hive-mcp"}))

  ;; Spawn it (requires Emacs running)
  ;; (.spawn! my-ling {:task "Explore the codebase"})

  ;; === Headless mode (no Emacs required) ===
  (def headless-ling (->ling "ling-002" {:cwd "/home/user/project"
                                         :presets ["worker"]
                                         :project-id "hive-mcp"
                                         :spawn-mode :headless}))

  ;; Spawn headless (just needs `claude` CLI on PATH)
  ;; (.spawn! headless-ling {:task "Explore the codebase"})

  ;; === Multi-model mode (OpenRouter via headless) ===
  (def deepseek-ling (->ling "ling-003" {:cwd "/home/user/project"
                                         :presets ["worker"]
                                         :project-id "hive-mcp"
                                         :model "deepseek/deepseek-v3.2"}))
  ;; Non-claude models automatically use :headless spawn-mode
  ;; (.spawn! deepseek-ling {:task "Analyze the architecture"})

  ;; Check status (works for both modes)
  ;; (.status my-ling)
  ;; (.status headless-ling)

  ;; Dispatch a task
  ;; (.dispatch! my-ling {:task "Find all test files" :files ["test/"]})
  ;; (.dispatch! headless-ling {:task "Find all test files"})

  ;; Get claims (mode-independent)
  ;; (.claims my-ling)

  ;; Kill when done (mode-appropriate cleanup)
  ;; (.kill! my-ling)
  ;; (.kill! headless-ling)

  ;; Query functions (spawn-mode preserved)
  ;; (get-ling "ling-001")
  ;; (list-lings "hive-mcp")
  )
