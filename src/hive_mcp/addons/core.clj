(ns hive-mcp.addons.core
  "Plugin architecture for hive-mcp addons (SAA strategy).

   Defines the IAddon protocol that allows third-party extensions to register:
   - MCP tools (tool definitions with handlers)
   - Memory stores (IMemoryStore implementations)
   - KG backends (IKGStore implementations)
   - Channel integrations (IChannel implementations)
   - Custom capabilities (arbitrary extension points)

   Architecture:
   ```
   hive-mcp (core) ← IAddon protocol ← addon implementations
                          ↓
                    addon-registry (atom)
                          ↓
              tools aggregated into tools.clj
              stores registered in protocols/memory
              backends registered in protocols/kg
              channels registered in protocols/channel
   ```

   Pattern follows existing hive-mcp protocol conventions:
   - defprotocol + defonce registry atom + register!/get/list
   - NoopAddon fallback implementation
   - Lifecycle management (init!/shutdown!)
   - Thread-safe registry operations

   SOLID-O: Open for extension via new IAddon implementations.
   SOLID-D: Depend on IAddon abstraction, not concrete addons.
   SOLID-I: IAddon is focused on addon lifecycle only.
   CLARITY-L: Layers stay pure - protocol is the boundary between
              core hive-mcp and third-party extensions.
   CLARITY-Y: Yield safe failure - init!/shutdown! never throw."
  (:require [clojure.set]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IAddon Protocol (Core Addon Interface)
;;; ============================================================================

(defprotocol IAddon
  "Protocol for hive-mcp addons (plugins).

   An addon is a self-contained extension that can contribute:
   - MCP tools (tool definitions with handlers)
   - Memory store implementations
   - KG backend implementations
   - Channel integrations
   - Custom capabilities

   Lifecycle:
     register-addon! → init! → [active use] → shutdown! → unregister-addon!

   Implementations must be:
   - Thread-safe (registry is concurrent)
   - Idempotent (init!/shutdown! safe to call multiple times)
   - Fail-safe (never throw, return result maps)

   Example addon:
   ```clojure
   (defrecord MyAddon [state]
     IAddon
     (addon-name [_] :my-addon)
     (addon-version [_] \"1.0.0\")
     (addon-info [_] {:name :my-addon :version \"1.0.0\" ...})
     (init! [_ opts] {:success? true})
     (shutdown! [_] {:success? true})
     (addon-tools [_] [{:name \"my_tool\" :handler my-handler ...}])
     (addon-capabilities [_] #{:tools}))
   ```"

  (addon-name [this]
    "Return unique keyword identifier for this addon.
     Used for registry lookup, logging, and dependency resolution.
     Example: :slack-connector, :redis-memory, :neo4j-kg")

  (addon-version [this]
    "Return semantic version string for this addon.
     Example: \"1.0.0\", \"2.3.1-SNAPSHOT\"")

  (addon-info [this]
    "Return metadata about this addon.
     Returns map with:
       :name         - Keyword identifier (same as addon-name)
       :version      - Semantic version string
       :description  - Human-readable description
       :author       - Author name/email
       :license      - License identifier (e.g., \"AGPL-3.0\", \"MIT\")
       :url          - Project URL (optional)
       :dependencies - Set of addon keywords this addon depends on (optional)
       :capabilities - Set of capability keywords (same as addon-capabilities)")

  (init! [this opts]
    "Initialize the addon.

     Called after registration, before the addon is active.
     Should set up internal state, connections, caches, etc.

     Arguments:
       opts - Addon-specific configuration map:
              :config    - User-provided configuration
              :directory - Working directory context
              :store     - Active memory store (if addon needs it)
              :kg-store  - Active KG store (if addon needs it)

     Returns map with:
       :success?    - Boolean indicating initialization success
       :errors      - Vector of error messages (empty on success)
       :metadata    - Addon-specific init metadata

     CLARITY-Y: Must not throw - return :success? false on failure.
     Idempotent: safe to call when already initialized.")

  (shutdown! [this]
    "Shutdown the addon.

     Called before unregistration or system shutdown.
     Should release resources, close connections, flush caches.

     Returns map with:
       :success?  - Boolean indicating clean shutdown
       :errors    - Vector of error messages (empty on success)

     CLARITY-Y: Must not throw - return :success? false on failure.
     Idempotent: safe to call when already shut down.")

  (addon-tools [this]
    "Return MCP tool definitions contributed by this addon.

     Returns vector of tool definition maps, each with:
       :name        - Tool name string (must be unique across all addons)
       :description - Tool description for LLM discovery
       :inputSchema - JSON Schema for tool parameters
       :handler     - Handler function (fn [params] -> response-map)

     Returns empty vector if addon contributes no tools.

     Tool names should be prefixed with addon name to avoid collisions:
     e.g., :my-addon contributes \"my_addon_search\", \"my_addon_index\"")

  (addon-capabilities [this]
    "Return set of capability keywords this addon provides.

     Standard capabilities:
       :tools          - Contributes MCP tools (via addon-tools)
       :memory-store   - Provides IMemoryStore implementation
       :kg-store       - Provides IKGStore implementation
       :channel        - Provides IChannel implementation
       :connector      - Provides IConnector implementation
       :workflow-engine - Provides IWorkflowEngine implementation

     Custom capabilities are allowed (e.g., :search, :analytics).

     Returns set of keywords."))

;;; ============================================================================
;;; Addon Registry
;;; ============================================================================

;; Thread-safe atom holding registered addons.
;; Key: addon-name keyword, Value: {:addon <IAddon> :state :registered|:active|:error :init-time <Instant>}
(defonce ^:private addon-registry (atom {}))

(defn register-addon!
  "Register an addon in the global registry.

   Does NOT call init! - caller is responsible for lifecycle:
     (register-addon! my-addon)
     (init-addon! :my-addon opts)

   Arguments:
     addon - Implementation of IAddon protocol

   Returns:
     Map with :success? and :addon-name on success.
     Returns :success? false if addon name is already registered.

   Throws:
     AssertionError if addon doesn't satisfy IAddon protocol."
  [addon]
  {:pre [(satisfies? IAddon addon)]}
  (let [name-kw (addon-name addon)]
    (if (contains? @addon-registry name-kw)
      (do
        (log/warn "Addon already registered" {:addon name-kw})
        {:success? false
         :addon-name name-kw
         :errors [(str "Addon " name-kw " is already registered")]})
      (do
        (swap! addon-registry assoc name-kw
               {:addon addon
                :state :registered
                :registered-at (java.time.Instant/now)
                :init-time nil
                :init-result nil})
        (log/info "Addon registered" {:addon name-kw
                                      :version (addon-version addon)
                                      :capabilities (addon-capabilities addon)})
        {:success? true
         :addon-name name-kw}))))

(defn get-addon
  "Get addon by name from the registry.

   Arguments:
     name-kw - Addon keyword (e.g., :slack-connector)

   Returns:
     The IAddon implementation, or nil if not found."
  [name-kw]
  (get-in @addon-registry [name-kw :addon]))

(defn get-addon-entry
  "Get full addon registry entry (addon + state metadata).

   Arguments:
     name-kw - Addon keyword

   Returns:
     Map with :addon, :state, :registered-at, :init-time, :init-result.
     Or nil if not found."
  [name-kw]
  (get @addon-registry name-kw))

(defn addon-registered?
  "Check if an addon is registered.

   Arguments:
     name-kw - Addon keyword

   Returns boolean."
  [name-kw]
  (contains? @addon-registry name-kw))

(defn list-addons
  "List all registered addons with their state.

   Returns vector of maps with:
     :name         - Addon keyword
     :version      - Version string
     :state        - :registered, :active, or :error
     :capabilities - Set of capability keywords
     :info         - Full addon-info map"
  []
  (->> @addon-registry
       (mapv (fn [[name-kw {:keys [addon state registered-at init-time]}]]
               {:name name-kw
                :version (addon-version addon)
                :state state
                :registered-at registered-at
                :init-time init-time
                :capabilities (addon-capabilities addon)
                :info (addon-info addon)}))))

(defn unregister-addon!
  "Unregister an addon. Calls shutdown! first if addon is active.

   Arguments:
     name-kw - Addon keyword

   Returns:
     Map with :success? true if addon was removed, false if not found."
  [name-kw]
  (if-let [{:keys [addon state]} (get-addon-entry name-kw)]
    (do
      ;; Shutdown if active
      (when (= state :active)
        (try
          (let [result (shutdown! addon)]
            (when-not (:success? result)
              (log/warn "Addon shutdown had errors during unregister"
                        {:addon name-kw :errors (:errors result)})))
          (catch Exception e
            (log/error e "Addon shutdown failed during unregister" {:addon name-kw}))))
      (swap! addon-registry dissoc name-kw)
      (log/info "Addon unregistered" {:addon name-kw})
      {:success? true :addon-name name-kw})
    (do
      (log/warn "Addon not found for unregister" {:addon name-kw})
      {:success? false
       :addon-name name-kw
       :errors [(str "Addon " name-kw " is not registered")]})))

;;; ============================================================================
;;; Addon Lifecycle Management
;;; ============================================================================

(defn init-addon!
  "Initialize a registered addon.

   Arguments:
     name-kw - Addon keyword
     opts    - Init options (passed to addon's init!)

   Returns:
     Init result map from the addon's init! method.
     Returns error map if addon not found or already active."
  [name-kw & [opts]]
  (if-let [{:keys [addon state]} (get-addon-entry name-kw)]
    (if (= state :active)
      (do
        (log/info "Addon already active, skipping init" {:addon name-kw})
        {:success? true :addon-name name-kw :already-active? true})
      (try
        (let [start-time (System/nanoTime)
              result (init! addon (or opts {}))
              elapsed-ms (/ (- (System/nanoTime) start-time) 1e6)]
          (if (:success? result)
            (do
              (swap! addon-registry assoc-in [name-kw :state] :active)
              (swap! addon-registry assoc-in [name-kw :init-time]
                     (java.time.Instant/now))
              (swap! addon-registry assoc-in [name-kw :init-result] result)
              (log/info "Addon initialized" {:addon name-kw
                                             :elapsed-ms elapsed-ms})
              (assoc result :addon-name name-kw :elapsed-ms elapsed-ms))
            (do
              (swap! addon-registry assoc-in [name-kw :state] :error)
              (swap! addon-registry assoc-in [name-kw :init-result] result)
              (log/warn "Addon init failed" {:addon name-kw
                                             :errors (:errors result)})
              (assoc result :addon-name name-kw))))
        (catch Exception e
          (swap! addon-registry assoc-in [name-kw :state] :error)
          (log/error e "Addon init threw exception" {:addon name-kw})
          {:success? false
           :addon-name name-kw
           :errors [(.getMessage e)]})))
    {:success? false
     :addon-name name-kw
     :errors [(str "Addon " name-kw " is not registered")]}))

(defn shutdown-addon!
  "Shutdown an active addon.

   Arguments:
     name-kw - Addon keyword

   Returns:
     Shutdown result map from the addon's shutdown! method.
     Returns error map if addon not found."
  [name-kw]
  (if-let [{:keys [addon state]} (get-addon-entry name-kw)]
    (if (not= state :active)
      (do
        (log/info "Addon not active, skipping shutdown" {:addon name-kw :state state})
        {:success? true :addon-name name-kw :already-inactive? true})
      (try
        (let [result (shutdown! addon)]
          (swap! addon-registry assoc-in [name-kw :state] :registered)
          (swap! addon-registry assoc-in [name-kw :init-time] nil)
          (log/info "Addon shut down" {:addon name-kw})
          (assoc result :addon-name name-kw))
        (catch Exception e
          (swap! addon-registry assoc-in [name-kw :state] :error)
          (log/error e "Addon shutdown threw exception" {:addon name-kw})
          {:success? false
           :addon-name name-kw
           :errors [(.getMessage e)]})))
    {:success? false
     :addon-name name-kw
     :errors [(str "Addon " name-kw " is not registered")]}))

(defn init-all!
  "Initialize all registered addons that are not yet active.

   Arguments:
     opts - Init options passed to each addon (optional)

   Returns:
     Map of addon-name -> init result."
  [& [opts]]
  (->> @addon-registry
       (filter (fn [[_name {:keys [state]}]] (not= state :active)))
       (map (fn [[name-kw _]]
              [name-kw (init-addon! name-kw opts)]))
       (into {})))

(defn shutdown-all!
  "Shutdown all active addons.

   Returns:
     Map of addon-name -> shutdown result."
  []
  (->> @addon-registry
       (filter (fn [[_name {:keys [state]}]] (= state :active)))
       (map (fn [[name-kw _]]
              [name-kw (shutdown-addon! name-kw)]))
       (into {})))

;;; ============================================================================
;;; Addon Tool Aggregation
;;; ============================================================================

(defn active-addon-tools
  "Get all MCP tools from active addons.

   Returns vector of tool definition maps from all active addons.
   Only includes tools from addons in :active state with :tools capability.

   Each tool is annotated with :addon-source metadata for traceability."
  []
  (->> @addon-registry
       (filter (fn [[_name {:keys [state addon]}]]
                 (and (= state :active)
                      (contains? (addon-capabilities addon) :tools))))
       (mapcat (fn [[name-kw {:keys [addon]}]]
                 (->> (addon-tools addon)
                      (map #(assoc % :addon-source name-kw)))))
       vec))

(defn addon-tools-by-name
  "Get MCP tools contributed by a specific addon.

   Arguments:
     name-kw - Addon keyword

   Returns:
     Vector of tool definition maps, or empty vector if addon
     not found / not active / has no tools."
  [name-kw]
  (if-let [{:keys [addon state]} (get-addon-entry name-kw)]
    (if (and (= state :active)
             (contains? (addon-capabilities addon) :tools))
      (vec (addon-tools addon))
      [])
    []))

;;; ============================================================================
;;; Capability Queries
;;; ============================================================================

(defn addons-with-capability
  "Find all active addons that provide a specific capability.

   Arguments:
     capability - Capability keyword (e.g., :tools, :memory-store, :kg-store)

   Returns:
     Vector of addon-name keywords."
  [capability]
  (->> @addon-registry
       (filter (fn [[_name {:keys [state addon]}]]
                 (and (= state :active)
                      (contains? (addon-capabilities addon) capability))))
       (mapv first)))

(defn all-capabilities
  "Get a summary of all capabilities provided by active addons.

   Returns:
     Map of capability keyword -> vector of addon names providing it."
  []
  (let [active (->> @addon-registry
                    (filter (fn [[_name {:keys [state]}]] (= state :active))))]
    (->> active
         (mapcat (fn [[name-kw {:keys [addon]}]]
                   (map (fn [cap] [cap name-kw]) (addon-capabilities addon))))
         (reduce (fn [acc [cap name-kw]]
                   (update acc cap (fnil conj []) name-kw))
                 {}))))

;;; ============================================================================
;;; Dependency Resolution
;;; ============================================================================

(defn check-dependencies
  "Check if all dependencies for an addon are satisfied.

   Arguments:
     name-kw - Addon keyword to check

   Returns:
     Map with:
       :satisfied?   - Boolean, all deps met
       :missing      - Set of missing addon keywords
       :available    - Set of available dependency keywords"
  [name-kw]
  (if-let [addon (get-addon name-kw)]
    (let [deps (or (:dependencies (addon-info addon)) #{})
          available (set (keys @addon-registry))
          missing (clojure.set/difference deps available)]
      {:satisfied? (empty? missing)
       :missing missing
       :available (clojure.set/intersection deps available)})
    {:satisfied? false
     :missing #{name-kw}
     :available #{}}))

;;; ============================================================================
;;; Registry Management
;;; ============================================================================

(defn reset-registry!
  "Reset the addon registry to empty state.
   Calls shutdown! on all active addons first.

   WARNING: Destructive operation.
   Used for testing and system reinitialization.

   Returns:
     Map of addon-name -> shutdown result (empty if no active addons)."
  []
  (let [shutdown-results (shutdown-all!)]
    (reset! addon-registry {})
    (log/info "Addon registry reset")
    shutdown-results))

(defn registry-status
  "Get comprehensive status of the addon registry.

   Returns:
     Map with:
       :total         - Total registered addons
       :active        - Count of active addons
       :registered    - Count of registered (not yet init'd) addons
       :error         - Count of addons in error state
       :tool-count    - Total tools from active addons
       :capabilities  - Aggregated capability map
       :addons        - Vector of addon summaries"
  []
  (let [entries (vals @addon-registry)
        by-state (group-by :state entries)]
    {:total (count entries)
     :active (count (:active by-state))
     :registered (count (:registered by-state))
     :error (count (:error by-state))
     :tool-count (count (active-addon-tools))
     :capabilities (all-capabilities)
     :addons (list-addons)}))

;;; ============================================================================
;;; NoopAddon (Fallback / Example Implementation)
;;; ============================================================================

(defrecord NoopAddon [id version-str]
  IAddon

  (addon-name [_] id)

  (addon-version [_] version-str)

  (addon-info [_]
    {:name id
     :version version-str
     :description "No-operation addon for testing and development"
     :author "hive-mcp"
     :license "AGPL-3.0-or-later"
     :dependencies #{}
     :capabilities #{}})

  (init! [_ _opts]
    {:success? true
     :errors []
     :metadata {:noop true}})

  (shutdown! [_]
    {:success? true
     :errors []})

  (addon-tools [_]
    [])

  (addon-capabilities [_]
    #{}))

(defn ->noop-addon
  "Create a NoopAddon for testing.

   Arguments:
     id      - Keyword identifier (default: :noop)
     version - Version string (default: \"0.0.0\")"
  ([] (->noop-addon :noop "0.0.0"))
  ([id] (->noop-addon id "0.0.0"))
  ([id version] (->NoopAddon id version)))

;;; ============================================================================
;;; ExampleAddon (Demonstrates Tools Capability)
;;; ============================================================================

(defrecord ExampleAddon [id state]
  IAddon

  (addon-name [_] id)

  (addon-version [_] "1.0.0")

  (addon-info [_]
    {:name id
     :version "1.0.0"
     :description "Example addon demonstrating tool contribution"
     :author "hive-mcp"
     :license "AGPL-3.0-or-later"
     :dependencies #{}
     :capabilities #{:tools}})

  (init! [_ opts]
    (reset! state {:initialized true :config (:config opts)})
    {:success? true
     :errors []
     :metadata {:config-keys (keys (:config opts))}})

  (shutdown! [_]
    (reset! state nil)
    {:success? true
     :errors []})

  (addon-tools [_]
    [{:name (str (name id) "_ping")
      :description (str "Ping tool from " (name id) " addon")
      :inputSchema {:type "object"
                    :properties {"message" {:type "string"
                                            :description "Message to echo"}}
                    :required ["message"]}
      :handler (fn [{:keys [message]}]
                 {:type "text"
                  :text (str "pong: " message " (from " (name id) ")")})}])

  (addon-capabilities [_]
    #{:tools}))

(defn ->example-addon
  "Create an ExampleAddon for testing tool contribution.

   Arguments:
     id - Keyword identifier (default: :example)"
  ([] (->example-addon :example))
  ([id] (->ExampleAddon id (atom nil))))
