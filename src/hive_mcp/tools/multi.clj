(ns hive-mcp.tools.multi
  "Bridge layer between cross-tool batch multiplexer and MCP tool dispatch.

   Resolves consolidated tool names to handlers, executes operations with error
   isolation, and orchestrates wave-based parallel/sequential execution.

   Architecture:
     consolidated/multi.clj (MCP facade)
       -> {command: 'batch', operations: [...]}
       -> tools/multi.clj (THIS — bridge layer)
       -> resolve consolidated handler per op
       -> wave-based execution with dependency ordering

   The compilation logic (validate, topo-sort, wave assignment) lives here.
   Pure orchestration (defmulti event dispatch) lives in events/multi.clj.

   Design decision: 20260207194224-3b674f5d"
  (:require [taoensso.timbre :as log]
            [clojure.data.json :as json]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; FX Effect Handlers (Step 4 of plan-20260207-multi-tool)
;; =============================================================================
;;
;; Registers :multi/wave-complete and :multi/op-error FX handlers into the
;; hive.events.fx global registry. Uses defonce for idempotent self-contained
;; registration at require time.
;;
;; These effects are emitted by the multi-op pipeline during wave execution
;; and consumed by the hive-events FX system (hive.events.fx/do-fx-seq).

(defn- resolve-agent-id
  "Resolve the current agent-id from agent context or environment.
   Returns agent-id string or nil if not in agent context."
  []
  (try
    (when-let [ctx-fn (requiring-resolve 'hive-mcp.agent.context/current-agent-id)]
      (ctx-fn))
    (catch Exception _
      (System/getenv "CLAUDE_SWARM_SLAVE_ID"))))

(defn- handle-wave-complete
  "FX handler for :multi/wave-complete — log wave completion and optionally
   emit hivemind progress shout when inside agent context.

   Expected data shape:
   {:wave-num      1              ;; Wave number that completed
    :op-count      3              ;; Number of ops in this wave
    :success-count 2              ;; How many succeeded
    :failed-count  1              ;; How many failed
    :total-waves   4}             ;; Total waves in the multi-op plan

   Example effect emission:
   [:multi/wave-complete {:wave-num 2 :op-count 5 :success-count 5
                          :failed-count 0 :total-waves 3}]"
  [{:keys [wave-num op-count success-count failed-count total-waves]}]
  (let [msg (str "[multi] Wave " wave-num "/" total-waves " complete: "
                 success-count "/" op-count " succeeded"
                 (when (pos? (or failed-count 0))
                   (str ", " failed-count " failed")))]
    ;; Always log
    (log/info msg)

    ;; Optionally emit hivemind progress shout when inside agent context
    (when-let [agent-id (resolve-agent-id)]
      (try
        (when-let [shout-fn (requiring-resolve 'hive-mcp.hivemind/shout!)]
          (shout-fn agent-id :progress
                    {:task "multi-op"
                     :message msg}))
        (catch Exception e
          (log/debug "[multi] Hivemind shout failed (non-fatal):" (ex-message e)))))))

(defn- handle-op-error
  "FX handler for :multi/op-error — log per-op errors with structured data.

   Expected data shape:
   {:op-id    \"op-1\"                ;; Operation ID that failed
    :tool     \"memory\"              ;; Tool that was called
    :command  \"add\"                 ;; Command within the tool
    :error    \"Connection refused\"  ;; Error message
    :wave-num 2}                     ;; Which wave this op was in

   Example effect emission:
   [:multi/op-error {:op-id \"op-3\" :tool \"kg\" :command \"edge\"
                     :error \"Node not found\" :wave-num 1}]"
  [{:keys [op-id tool command error wave-num]}]
  (log/error "[multi] Op failed"
             {:op-id   op-id
              :tool    tool
              :command command
              :wave    wave-num
              :error   error}))

;; Self-contained FX registration. Runs once at require time.
;; Registers :multi/wave-complete and :multi/op-error into the
;; hive.events.fx global registry.
(defonce ^:private *fx-registered
  (try
    (when-let [reg-fx (requiring-resolve 'hive.events.fx/reg-fx)]
      (reg-fx :multi/wave-complete handle-wave-complete)
      (reg-fx :multi/op-error handle-op-error)
      (log/info "[multi] FX effects registered: :multi/wave-complete :multi/op-error")
      true)
    (catch Exception e
      (log/warn "[multi] FX registration deferred (hive.events.fx not available):"
                (ex-message e))
      false)))

(defn register-fx!
  "Explicitly register multi FX handlers. Called during server init if
   defonce registration was deferred (e.g., hive.events.fx wasn't loaded yet).

   Safe to call multiple times — re-registers without harm."
  []
  (when-let [reg-fx (requiring-resolve 'hive.events.fx/reg-fx)]
    (reg-fx :multi/wave-complete handle-wave-complete)
    (reg-fx :multi/op-error handle-op-error)
    (log/debug "[multi] FX effects (re-)registered")
    true))

;; =============================================================================
;; Tool Resolution (requiring-resolve to avoid circular deps)
;; =============================================================================

(defn resolve-consolidated-handler
  "Resolve a consolidated tool name to its handler function.
   Uses requiring-resolve to avoid circular dependency with consolidated/multi.

   Tool names are consolidated names like 'memory', 'agent', 'kg' — NOT flat
   tool names like 'mcp_memory_add'. Each consolidated handler accepts
   {:command \"add\" ...params} and dispatches internally.

   Returns handler fn or nil if tool not found."
  [tool-name]
  (try
    (let [get-handler (requiring-resolve 'hive-mcp.tools.consolidated.multi/get-tool-handler)]
      (get-handler tool-name))
    (catch Exception e
      (log/error {:event :tool-resolve-error
                  :tool  tool-name
                  :error (ex-message e)})
      nil)))

(defn resolve-tool-handler
  "Resolve a tool name to its handler function.
   Tries consolidated handlers first (preferred), then falls back to
   flat tool resolution via hive-mcp.tools/get-tool-by-name.

   Returns handler fn or nil if tool not found."
  [tool-name]
  (or (resolve-consolidated-handler tool-name)
      ;; Fallback: resolve flat tool name (e.g., for non-consolidated tools)
      (try
        (let [get-tool-fn (requiring-resolve 'hive-mcp.tools/get-tool-by-name)
              tool-def    (get-tool-fn tool-name)]
          (:handler tool-def))
        (catch Exception e
          (log/error {:event :tool-resolve-error
                      :tool  tool-name
                      :error (ex-message e)})
          nil))))

;; =============================================================================
;; Operation Normalization
;; =============================================================================

(defn normalize-op
  "Normalize a single operation map from MCP JSON format.
   Converts string keys to keywords. Ensures :id and :tool are present."
  [op]
  (let [normalized (into {} (map (fn [[k v]] [(keyword k) v]) op))]
    (cond-> normalized
      ;; Auto-generate ID if missing
      (str/blank? (:id normalized))
      (assoc :id (str "op-" (java.util.UUID/randomUUID)))

      ;; Normalize depends_on to vector of strings
      (:depends_on normalized)
      (update :depends_on (fn [deps]
                            (cond
                              (string? deps) [deps]
                              (sequential? deps) (vec deps)
                              :else []))))))

;; =============================================================================
;; Operation Validation
;; =============================================================================

(defn validate-ops
  "Validate an operations vector. Returns {:valid true} or {:valid false :errors [...]}.

   Checks:
   - All ops have :id and :tool
   - IDs are unique
   - :depends_on references exist
   - No circular dependencies (Kahn's algorithm)
   - No self-dependencies"
  [ops]
  (let [errors    (atom [])
        add-error (fn [msg] (swap! errors conj msg))
        id-set    (set (map :id ops))
        id-counts (frequencies (map :id ops))]

    ;; Check required fields
    (doseq [{:keys [id tool] :as op} ops]
      (when (str/blank? id)
        (add-error (str "Operation missing :id — " (pr-str (select-keys op [:tool :command])))))
      (when (str/blank? tool)
        (add-error (str "Operation '" id "' missing :tool"))))

    ;; Check duplicate IDs
    (doseq [[id cnt] id-counts]
      (when (> cnt 1)
        (add-error (str "Duplicate operation ID: '" id "' (appears " cnt " times)"))))

    ;; Check dependency references and self-deps
    (doseq [{:keys [id depends_on]} ops]
      (when (seq depends_on)
        (doseq [dep depends_on]
          (when (= dep id)
            (add-error (str "Operation '" id "' depends on itself")))
          (when-not (contains? id-set dep)
            (add-error (str "Operation '" id "' depends on non-existent '" dep "'"))))))

    ;; Check circular dependencies via Kahn's algorithm
    (when (empty? @errors)
      (let [in-degree (reduce (fn [m {:keys [id depends_on]}]
                                (reduce (fn [m' _dep]
                                          (update m' id (fnil inc 0)))
                                        (update m id (fnil identity 0))
                                        depends_on))
                              {}
                              ops)
            queue     (into clojure.lang.PersistentQueue/EMPTY
                            (keep (fn [[id deg]] (when (zero? deg) id)) in-degree))
            ;; Build forward adjacency: dep -> [dependents]
            fwd       (reduce (fn [m {:keys [id depends_on]}]
                                (reduce (fn [m' dep]
                                          (update m' dep (fnil conj []) id))
                                        m
                                        depends_on))
                              {}
                              ops)]
        (loop [q      queue
               sorted []
               deg    in-degree]
          (if (empty? q)
            (when (< (count sorted) (count ops))
              (add-error (str "Circular dependency detected among: "
                              (str/join ", " (remove (set sorted) (map :id ops))))))
            (let [node     (peek q)
                  q'       (pop q)
                  sorted'  (conj sorted node)
                  children (get fwd node [])
                  [q'' deg'] (reduce (fn [[q d] child]
                                       (let [new-deg (dec (get d child))]
                                         [(if (zero? new-deg) (conj q child) q)
                                          (assoc d child new-deg)]))
                                     [q' deg]
                                     children)]
              (recur q'' sorted' deg'))))))

    (if (seq @errors)
      {:valid false :errors @errors}
      {:valid true})))

;; =============================================================================
;; Wave Assignment (topological sort into dependency-ordered groups)
;; =============================================================================

(defn assign-waves
  "Assign operations to execution waves based on dependencies.
   Independent ops run in the same wave (parallel). Dependent ops
   are in later waves.

   Returns ops with :wave key added, sorted by wave number.
   Uses Kahn's topological sort to determine wave ordering."
  [ops]
  (let [ops-by-id (into {} (map (juxt :id identity) ops))
        ;; Build in-degree map
        in-degree (reduce (fn [m {:keys [id depends_on]}]
                            (assoc m id (count (or depends_on []))))
                          {}
                          ops)
        ;; Build forward adjacency: dep -> [dependents]
        fwd       (reduce (fn [m {:keys [id depends_on]}]
                            (reduce (fn [m' dep]
                                      (update m' dep (fnil conj []) id))
                                    m
                                    (or depends_on [])))
                          {}
                          ops)]
    (loop [deg       in-degree
           wave-num  1
           result    []]
      (let [ready (keep (fn [[id d]] (when (zero? d) id)) deg)]
        (if (empty? ready)
          result  ;; All assigned
          (let [;; Assign current wave
                wave-ops (mapv (fn [id] (assoc (get ops-by-id id) :wave wave-num))
                               ready)
                ;; Remove processed nodes, decrement dependents
                remaining-deg (reduce (fn [d id] (dissoc d id)) deg ready)
                updated-deg   (reduce (fn [d ready-id]
                                        (reduce (fn [d' child]
                                                  (if (contains? d' child)
                                                    (update d' child dec)
                                                    d'))
                                                d
                                                (get fwd ready-id [])))
                                      remaining-deg
                                      ready)]
            (recur updated-deg (inc wave-num) (into result wave-ops))))))))

;; =============================================================================
;; Single Operation Execution (error-isolated)
;; =============================================================================

(defn execute-op
  "Execute a single operation with error isolation.
   Resolves consolidated tool handler, merges command + params, invokes handler.

   For consolidated tools (e.g., :tool 'memory'), forwards {:command 'add' ...params}
   to the consolidated handler which dispatches internally.

   Returns {:id op-id :success bool :result map} or {:id op-id :success false :error string}."
  [{:keys [id tool command] :as op}]
  (try
    (let [handler (resolve-tool-handler tool)]
      (if-not handler
        {:id id :success false :error (str "Tool not found: " tool)}
        (let [;; Build handler args: all params except multi-meta keys
              meta-keys #{:id :tool :depends_on :wave}
              handler-args (-> (apply dissoc op meta-keys)
                               ;; Ensure command is string for CLI dispatch
                               (update :command #(if (keyword? %) (name %) %)))
              result (handler handler-args)]
          {:id id :success true :result result})))
    (catch Exception e
      (log/error {:event :op-execution-error
                  :op-id id
                  :tool  tool
                  :error (ex-message e)})
      {:id id :success false :error (ex-message e)})))

;; =============================================================================
;; Wave Execution Engine
;; =============================================================================

(defn- execute-wave
  "Execute all operations in a single wave.
   Operations within a wave run in parallel (pmap).
   Returns vector of op results."
  [wave-ops]
  (let [n (count wave-ops)]
    (if (<= n 1)
      (mapv execute-op wave-ops)
      ;; Parallel execution for multi-op waves
      (vec (pmap execute-op wave-ops)))))

(defn- check-deps-satisfied
  "Check if all dependencies for an op have succeeded.
   Returns {:ok true} or {:ok false :failed-deps [ids]}."
  [{:keys [depends_on]} results-by-id]
  (if (empty? depends_on)
    {:ok true}
    (let [failed (filterv (fn [dep-id]
                            (let [r (get results-by-id dep-id)]
                              (or (nil? r) (not (:success r)))))
                          depends_on)]
      (if (empty? failed)
        {:ok true}
        {:ok false :failed-deps failed}))))

;; =============================================================================
;; FX Emission Helpers
;; =============================================================================

(defn- fire-fx!
  "Fire a single FX effect through the hive.events.fx registry.
   No-op if fx registry is not available (graceful degradation).
   Used to emit :multi/wave-complete and :multi/op-error during execution."
  [fx-id fx-data]
  (try
    (when-let [get-fx (requiring-resolve 'hive.events.fx/get-fx)]
      (when-let [handler (get-fx fx-id)]
        (handler fx-data)))
    (catch Exception e
      (log/debug "[multi] FX emission" fx-id "failed (non-fatal):" (ex-message e)))))

(defn- emit-wave-complete!
  "Emit :multi/wave-complete FX after a wave finishes execution."
  [wave-num wave-results total-waves]
  (let [op-count (count wave-results)
        success-count (count (filter :success wave-results))
        failed-count (- op-count success-count)]
    (fire-fx! :multi/wave-complete
              {:wave-num      wave-num
               :op-count      op-count
               :success-count success-count
               :failed-count  failed-count
               :total-waves   total-waves})))

(defn- emit-op-errors!
  "Emit :multi/op-error FX for each failed op in wave results."
  [wave-results wave-num]
  (doseq [{:keys [id error] :as r} wave-results
          :when (and (not (:success r)) error)]
    ;; Extract tool/command from op metadata if available
    (fire-fx! :multi/op-error
              {:op-id    id
               :tool     (:tool r)
               :command  (:command r)
               :error    error
               :wave-num wave-num})))

;; =============================================================================
;; Multi-Operation Runner (main entry point)
;; =============================================================================

(defn run-multi
  "Execute a vector of cross-tool operations with dependency ordering.

   Pipeline: normalize → validate → assign-waves → execute-per-wave (parallel within wave)

   Options:
   - :dry-run  — validate and plan only, don't execute

   Returns:
   {:success bool
    :waves   {1 {:ops [...] :results [...]}
              2 {:ops [...] :results [...]}}
    :summary {:total N :success M :failed F :waves W}
    :errors  [...] (validation errors if any)}"
  [ops & {:keys [dry-run]}]
  (let [;; Step 0: Normalize operations
        normalized-ops (mapv normalize-op ops)
        ;; Step 1: Validate
        validation (validate-ops normalized-ops)]
    (if-not (:valid validation)
      {:success false
       :errors  (:errors validation)
       :summary {:total (count ops) :success 0 :failed 0 :waves 0}}

      ;; Step 2: Assign waves
      (let [waved-ops   (assign-waves normalized-ops)
            wave-groups (group-by :wave waved-ops)
            wave-count  (count wave-groups)]

        (if dry-run
          ;; Dry-run: return plan without executing
          {:success true
           :dry-run true
           :waves   (into (sorted-map)
                          (map (fn [[w ops]]
                                 [w {:ops (mapv #(select-keys % [:id :tool :command :depends_on]) ops)}])
                               wave-groups))
           :summary {:total (count ops) :success 0 :failed 0 :waves wave-count}}

          ;; Step 3: Execute wave by wave
          (let [all-results (atom {})  ;; id -> result
                wave-results (atom (sorted-map))]

            (doseq [wave-num (sort (keys wave-groups))]
              (let [wave-ops (get wave-groups wave-num)
                    ;; Filter out ops whose deps failed (skip with dep-failure error)
                    {executable true skipped false}
                    (group-by (fn [op]
                                (:ok (check-deps-satisfied op @all-results)))
                              wave-ops)
                    ;; Mark skipped ops
                    skip-results (mapv (fn [op]
                                         (let [{:keys [failed-deps]} (check-deps-satisfied op @all-results)]
                                           {:id (:id op) :success false
                                            :error (str "Skipped: dependencies failed — " (str/join ", " failed-deps))}))
                                       (or skipped []))
                    ;; Execute remaining
                    exec-results (execute-wave (or executable []))
                    wave-all     (into skip-results exec-results)]

                ;; Store results
                (doseq [r wave-all]
                  (swap! all-results assoc (:id r) r))
                (swap! wave-results assoc wave-num
                       {:ops     (mapv #(select-keys % [:id :tool :command]) wave-ops)
                        :results wave-all})

                ;; Emit FX effects for observability
                (emit-wave-complete! wave-num wave-all wave-count)
                (emit-op-errors! wave-all wave-num)))

            (let [results     (vals @all-results)
                  success-cnt (count (filter :success results))
                  failed-cnt  (count (remove :success results))]
              {:success (zero? failed-cnt)
               :waves   @wave-results
               :summary {:total   (count ops)
                         :success success-cnt
                         :failed  failed-cnt
                         :waves   wave-count}})))))))

;; =============================================================================
;; Result Formatting
;; =============================================================================

(defn format-results
  "Format multi-execution results for MCP response.
   Returns a JSON-serializable map suitable for {:type \"text\" :text ...}."
  [{:keys [success waves summary errors dry-run] :as _results}]
  (let [output (cond-> {:success success
                        :summary summary}
                 dry-run (assoc :dry_run true
                                :plan (into {}
                                            (map (fn [[w {:keys [ops]}]]
                                                   [(str "wave_" w) ops])
                                                 waves)))
                 (not dry-run)
                 (assoc :waves (into {}
                                     (map (fn [[w {:keys [results]}]]
                                            [(str "wave_" w)
                                             (mapv (fn [{:keys [id success result error]}]
                                                     (cond-> {:id id :success success}
                                                       result (assoc :result
                                                                     (if (string? result)
                                                                       result
                                                                       (try (json/write-str result)
                                                                            (catch Exception _ (pr-str result)))))
                                                       error  (assoc :error error)))
                                                   results)])
                                          waves)))
                 errors (assoc :errors errors))]
    {:type "text"
     :text (json/write-str output)}))

;; =============================================================================
;; MCP Handler (called from consolidated/multi.clj)
;; =============================================================================

(defn handle-batch
  "Handle a batch of cross-tool operations from the MCP multi tool.

   Expects params:
   - :operations  — vector of operation maps, each with :tool, :command, + params
                    Optional: :id (auto-generated if missing), :depends_on [ids]
   - :dry_run     — (optional) validate and plan without executing
   - :parallel    — (optional) hint, ignored since wave engine handles parallelism

   Each operation has the shape:
   {:id         'op-1'           ;; unique within batch (auto-gen if missing)
    :tool       'memory'         ;; consolidated tool name
    :command    'add'            ;; command for the tool
    :depends_on ['op-0']         ;; (optional) dependency ordering
    ...params}                   ;; tool-specific params forwarded

   Example:
   {:operations [{:id 'op-1' :tool 'memory' :command 'add' :content 'hello' :type 'note'}
                 {:id 'op-2' :tool 'kg' :command 'edge' :from 'a' :to 'b' :relation 'implements'
                  :depends_on ['op-1']}]}"
  [params]
  (let [{:keys [operations dry_run]} params]
    (cond
      ;; Validate operations present
      (or (nil? operations) (empty? operations))
      {:isError true
       :text "Batch requires non-empty 'operations' array. Each op: {tool, command, ...params}"}

      (not (sequential? operations))
      {:isError true
       :text "operations must be an array of operation objects"}

      :else
      (let [result (run-multi operations :dry-run (boolean dry_run))]
        (format-results result)))))
