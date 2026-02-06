(ns hive-mcp.protocols.memory
  "Protocol definition for Memory storage backends.

   Abstracts the memory storage interface so that different backends
   can be used interchangeably:
   - Chroma (vector DB with semantic search)
   - DataScript (in-memory, for testing)
   - Future: Datalevin, Datomic, etc.

   SOLID-I: Interface segregation - memory operations only.
   SOLID-D: Depend on abstraction, not Chroma directly.
   CLARITY-L: Layers stay pure - protocol is the boundary between
              memory domain logic and storage implementation.
   DDD: Repository pattern for memory entity lifecycle management."
  (:require [clojure.string]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IMemoryStore Protocol (Core CRUD + Search)
;;; ============================================================================

(defprotocol IMemoryStore
  "Storage backend protocol for Memory entries.

   All memory modules (crud, search, lifecycle) should use this protocol
   instead of calling chroma namespace directly.

   Implementations:
   - ChromaStore: Vector DB with semantic search (production default)
   - DataScriptStore: In-memory, fast, used for tests
   - AtomStore: Simple atom-based, for minimal testing

   Entry shape:
   {:id           \"20260205120000-abc123\"
    :type         \"note\" | \"snippet\" | \"convention\" | \"decision\" | \"axiom\"
    :content      \"...\"
    :tags         [\"tag1\" \"tag2\"]
    :duration     \"ephemeral\" | \"short\" | \"medium\" | \"long\" | \"permanent\"
    :expires      \"2026-05-05T...\" | nil
    :project-id   \"hive-mcp\" | \"global\"
    :created      \"2026-02-05T...\"
    :updated      \"2026-02-05T...\"
    :content-hash \"sha256...\"
    ;; Analytics
    :access-count    0
    :helpful-count   0
    :unhelpful-count 0
    ;; Knowledge Graph edges (IDs, not full edges)
    :kg-outgoing  [\"edge-id-1\"]
    :kg-incoming  [\"edge-id-2\"]
    ;; Abstraction/Grounding (Korzybski Structural Differential)
    :abstraction-level 1-4
    :grounded-at       \"2026-02-05T...\"
    :grounded-from     \"disc-entity-id\"
    :source-hash       \"sha256...\"
    :source-file       \"path/to/file.clj\"
    :knowledge-gaps    [\"gap1\" \"gap2\"]
    ;; Staleness tracking (Bayesian Beta model)
    :staleness-alpha  1
    :staleness-beta   1
    :staleness-source :hash-mismatch | :git-commit | :time-decay | :transitive
    :staleness-depth  0}"

  ;;; =========================================================================
  ;;; Connection Lifecycle
  ;;; =========================================================================

  (connect! [this config]
    "Initialize connection to the storage backend.

     Data-oriented: returns a result map, never throws.
     Idempotent: safe to call when already connected (returns current state).

     Arguments:
       config - Backend-specific configuration map:
                Chroma:     {:host \"localhost\" :port 8000
                             :collection-name \"hive-mcp-memory\"}
                DataScript: {:schema <datascript-schema>}  ; or {} for defaults
                Atom:       {}  ; no config needed

     Returns map:
       {:success?   true/false
        :backend    \"chroma\" | \"datascript\" | \"atom\"
        :errors     []          ; vector of error strings (empty on success)
        :metadata   {...}}      ; backend-specific connection metadata
                                ; e.g. Chroma: {:host \"...\" :port N :collection \"...\"}
                                ;      DataScript: {:schema-attrs N}

     CLARITY-Y: Yield safe failure - returns :success? false, never throws.")

  (disconnect! [this]
    "Close connection and release backend resources.

     Flushes pending writes, closes connections, clears caches.
     No-op for in-memory backends (Atom, DataScript).
     Idempotent: safe to call when already disconnected.

     Returns map:
       {:success?  true/false
        :errors    []}")

  (connected? [this]
    "Check if this store has an active connection.

     Lightweight state check - does NOT verify backend reachability.
     Use health-check for active verification.

     Returns boolean.")

  (health-check [this]
    "Actively verify backend health and reachability.

     For Chroma: pings server, verifies collection exists.
     For DataScript: checks connection atom is non-nil.
     For Atom: always healthy.

     Returns map:
       {:healthy?    true/false
        :latency-ms  <integer>  ; round-trip time in ms (nil if unhealthy)
        :backend     \"chroma\" | \"datascript\" | \"atom\"
        :entry-count <integer>  ; approximate count (nil if unhealthy)
        :errors      []         ; vector of error strings
        :checked-at  \"ISO-8601-timestamp\"}

     CLARITY-Y: Yield safe failure - returns :healthy? false, never throws.")

  ;;; =========================================================================
  ;;; CRUD Operations
  ;;; =========================================================================

  (add-entry! [this entry]
    "Add a new memory entry to the store.

     Arguments:
       entry - Map with required keys:
               :type    - Entry type string
               :content - Entry content (string or map)
               Optional keys:
               :id         - Auto-generated if nil
               :tags       - Vector of tag strings
               :duration   - TTL category (default: \"long\")
               :project-id - Project scope (default: \"global\")
               :kg-*       - Knowledge graph relationships
               :abstraction-level - 1-4

     Returns:
       The entry ID (string) on success.

     Throws:
       ex-info on validation failure or storage error.")

  (get-entry [this id]
    "Get a memory entry by ID.

     Arguments:
       id - Entry identifier string

     Returns:
       Full entry map or nil if not found.")

  (update-entry! [this id updates]
    "Update an existing entry's attributes.

     Arguments:
       id      - Entry to update
       updates - Map of attributes to merge (partial update)

     Returns:
       Updated entry map on success, nil if not found.

     Note: Automatically updates :updated timestamp.")

  (delete-entry! [this id]
    "Delete an entry from the store.

     Arguments:
       id - Entry to delete

     Returns:
       true on success, false if not found.")

  (query-entries [this opts]
    "Query entries with filtering.

     Arguments:
       opts - Map with optional keys:
              :type           - Filter by type
              :project-id     - Filter by project
              :tags           - Filter by tags (all must match)
              :duration       - Filter by duration category
              :limit          - Max results (default: 100)
              :include-expired? - Include expired entries (default: false)

     Returns:
       Seq of entry maps, sorted by :created descending.")

  ;;; =========================================================================
  ;;; Semantic Search (Vector-based)
  ;;; =========================================================================

  (search-similar [this query-text opts]
    "Semantic similarity search.

     Arguments:
       query-text - Natural language query string
       opts       - Map with optional keys:
                    :limit      - Max results (default: 10)
                    :type       - Filter by type
                    :project-id - Filter by project
                    :threshold  - Min similarity score (0.0-1.0)

     Returns:
       Seq of maps with :entry and :score keys, sorted by score descending.
       Returns empty seq if semantic search not supported by backend.")

  (supports-semantic-search? [this]
    "Check if this store supports semantic/vector search.

     Returns:
       true if search-similar is implemented, false otherwise.
       Non-vector backends (DataScript, Atom) return false.")

  ;;; =========================================================================
  ;;; Expiration Management
  ;;; =========================================================================

  (cleanup-expired! [this]
    "Delete all expired entries from the store.

     Returns:
       Map with :count (number deleted) and :deleted-ids (for KG cleanup).")

  (entries-expiring-soon [this days opts]
    "Get entries expiring within the given number of days.

     Arguments:
       days - Number of days to look ahead
       opts - Map with optional :project-id filter

     Returns:
       Seq of entry maps that will expire within the window.")

  ;;; =========================================================================
  ;;; Duplicate Detection
  ;;; =========================================================================

  (find-duplicate [this type content-hash opts]
    "Find entry with matching content-hash in the given type.

     Arguments:
       type         - Entry type to search within
       content-hash - SHA-256 hash of normalized content
       opts         - Map with optional :project-id filter

     Returns:
       Existing entry map or nil if no duplicate found.")

  ;;; =========================================================================
  ;;; Store Management
  ;;; =========================================================================

  (store-status [this]
    "Get store status and configuration info.

     Returns:
       Map with:
       :backend        - Backend name (\"chroma\", \"datascript\", etc.)
       :configured?    - Whether store is properly configured
       :entry-count    - Total number of entries (approximate)
       :supports-search? - Whether semantic search is available")

  (reset-store! [this]
    "Reset the store to empty state.

     WARNING: Destructive operation - deletes all entries.
     Used for testing and state clearing.

     Returns:
       true on success."))

;;; ============================================================================
;;; Active Store Management
;;; ============================================================================

;; Atom holding the currently active IMemoryStore implementation.
(defonce ^:private active-store (atom nil))

(defn set-store!
  "Set the active memory store implementation.
   Called during system initialization.

   Does NOT call connect! - caller is responsible for lifecycle:
     (def store (->ChromaStore ...))
     (connect! store {:host \"localhost\" :port 8000})
     (set-store! store)

   Arguments:
     store - Implementation of IMemoryStore protocol

   Returns:
     The store.

   Throws:
     AssertionError if store doesn't satisfy protocol."
  [store]
  {:pre [(satisfies? IMemoryStore store)]}
  (reset! active-store store)
  store)

(defn get-store
  "Get the active memory store.

   Returns:
     The current IMemoryStore implementation.

   Throws:
     ex-info if no store has been set."
  []
  (or @active-store
      (throw (ex-info "No memory store configured. Call set-store! first."
                      {:hint "Initialize with chroma-store or datascript-store"}))))

(defn store-set?
  "Check if a memory store has been configured.

   Returns:
     true if set-store! has been called, false otherwise."
  []
  (some? @active-store))

(defn reset-active-store!
  "Reset the active store atom to nil.
   Calls disconnect! on the current store if one is set.
   Used for testing and reinitialization."
  []
  (when-let [store @active-store]
    (try
      (disconnect! store)
      (catch Exception _)))
  (reset! active-store nil))

;;; ============================================================================
;;; Lifecycle Convenience Functions
;;; ============================================================================

(defn connect-active-store!
  "Connect the active store with the given config.
   Convenience wrapper around (connect! (get-store) config).

   Arguments:
     config - Backend-specific configuration map

   Returns:
     connect! result map {:success? ... :backend ... :errors ... :metadata ...}

   Throws:
     ex-info if no store has been set."
  [config]
  (connect! (get-store) config))

(defn active-store-healthy?
  "Check if the active store is connected and healthy.
   Returns false if no store is set.

   Returns boolean."
  []
  (when (store-set?)
    (try
      (:healthy? (health-check @active-store))
      (catch Exception _ false))))

(defn active-store-status
  "Get comprehensive status of the active store.
   Combines store-status with health-check for full picture.

   Returns map or nil if no store is set."
  []
  (when (store-set?)
    (let [store @active-store]
      (merge (store-status store)
             (try (health-check store)
                  (catch Exception e
                    {:healthy? false :errors [(.getMessage e)]}))))))

;;; ============================================================================
;;; IMemoryStoreWithAnalytics Protocol (Optional Extension)
;;; ============================================================================

(defprotocol IMemoryStoreWithAnalytics
  "Extended protocol for stores that support analytics tracking.

   Not all backends need to implement this - it's optional.
   Check with (satisfies? IMemoryStoreWithAnalytics store) before using."

  (log-access! [this id]
    "Log an access event for an entry.
     Increments :access-count.

     Arguments:
       id - Entry ID

     Returns:
       Updated entry or nil if not found.")

  (record-feedback! [this id feedback]
    "Record helpfulness feedback for an entry.

     Arguments:
       id       - Entry ID
       feedback - :helpful or :unhelpful

     Returns:
       Updated entry or nil if not found.")

  (get-helpfulness-ratio [this id]
    "Calculate helpfulness ratio for an entry.

     Arguments:
       id - Entry ID

     Returns:
       Map with :helpful-count, :unhelpful-count, :ratio (0.0-1.0), :total
       or nil if entry not found."))

(defn analytics-store?
  "Check if the store supports analytics tracking.

   Arguments:
     store - IMemoryStore implementation

   Returns:
     true if store implements IMemoryStoreWithAnalytics."
  [store]
  (satisfies? IMemoryStoreWithAnalytics store))

;;; ============================================================================
;;; IMemoryStoreWithStaleness Protocol (Optional Extension)
;;; ============================================================================

(defprotocol IMemoryStoreWithStaleness
  "Extended protocol for stores that support staleness tracking.

   Implements Bayesian Beta model for freshness evidence.
   Used for L1 Phase 2 transitive staleness propagation."

  (update-staleness! [this id staleness-opts]
    "Update staleness tracking fields for an entry.

     Arguments:
       id            - Entry ID
       staleness-opts - Map with optional keys:
                        :alpha  - Bayesian α parameter (freshness evidence)
                        :beta   - Bayesian β parameter (staleness evidence)
                        :source - Staleness source keyword
                        :depth  - Propagation depth

     Returns:
       Updated entry or nil if not found.")

  (get-stale-entries [this threshold opts]
    "Get entries with staleness probability above threshold.

     Arguments:
       threshold - Staleness probability threshold (0.0-1.0)
       opts      - Map with optional :project-id, :type filters

     Returns:
       Seq of entries where staleness-beta / (staleness-alpha + staleness-beta) > threshold.")

  (propagate-staleness! [this source-id depth]
    "Propagate staleness from source entry to dependent entries.

     Arguments:
       source-id - Entry that became stale
       depth     - Current propagation depth

     Returns:
       Count of entries updated via transitive propagation."))

(defn staleness-store?
  "Check if the store supports staleness tracking.

   Arguments:
     store - IMemoryStore implementation

   Returns:
     true if store implements IMemoryStoreWithStaleness."
  [store]
  (satisfies? IMemoryStoreWithStaleness store))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defn content-hash
  "Compute SHA-256 hash of content for deduplication.
   Normalizes content (trim, collapse whitespace) before hashing.

   Arguments:
     content - String or map content

   Returns:
     64-character hex string (SHA-256 hash)."
  [content]
  (let [content-str (if (string? content) content (pr-str content))
        normalized (-> content-str
                       clojure.string/trim
                       (clojure.string/replace #"[ \t]+" " ")
                       (clojure.string/replace #"\n+" "\n"))
        md (java.security.MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest md (.getBytes normalized "UTF-8"))]
    (apply str (map #(format "%02x" %) hash-bytes))))

(defn generate-id
  "Generate a unique ID for memory entries.
   Format: YYYYMMDDHHmmss-XXXXXXXX (timestamp + random hex).

   Returns:
     String ID."
  []
  (let [ts (java.time.LocalDateTime/now)
        fmt (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss")
        random-hex (format "%08x" (rand-int Integer/MAX_VALUE))]
    (str (.format ts fmt) "-" random-hex)))

(defn iso-timestamp
  "Return current ISO 8601 timestamp with timezone.

   Returns:
     String timestamp."
  []
  (str (java.time.ZonedDateTime/now
        (java.time.ZoneId/systemDefault))))
