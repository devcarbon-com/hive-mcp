(ns hive-mcp.protocols.kg
  "Protocol definition for Knowledge Graph storage backends.

   Abstracts the Datalog store interface so that different backends
   can be used interchangeably:
   - DataScript (in-memory, for testing and default)
   - Datalevin (persistent via LMDB, for production)
   - Datahike (temporal, for audit trails)

   SOLID-I: Interface segregation - KG storage operations only.
   SOLID-D: Depend on abstraction, not specific datalog implementations.
   CLARITY-L: Layers stay pure - protocol is the boundary between
              KG domain logic and storage implementation.
   DDD: Repository pattern for knowledge graph entity lifecycle.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IKGStore Protocol (Core Graph Operations)
;;; ============================================================================

(defprotocol IKGStore
  "Storage backend protocol for Knowledge Graph.

   All KG modules (edges, queries, traversal) should use this protocol
   instead of calling datascript.core, datalevin.core, or datahike.api directly.

   Implementations:
   - DataScriptStore: In-memory, fast, used for tests and default
   - DatalevinStore: Persistent to disk via LMDB, used for production
   - DatahikeStore: Temporal with history, for audit trails

   Entity shape:
   {:db/id           123
    :kg-edge/id      \"edge-abc123\"
    :kg-edge/from    \"mem-123\"
    :kg-edge/to      \"mem-456\"
    :kg-edge/relation :implements
    :kg-edge/confidence 0.9
    :kg-edge/created  #inst \"...\"}"

  (ensure-conn! [this]
    "Ensure the connection is initialized. Creates if nil.
     Returns the raw connection object (backend-specific).
     Thread-safe: multiple calls are idempotent.")

  (transact! [this tx-data]
    "Transact data into the store.
     tx-data is a vector of maps or transaction commands.
     Returns the transaction report {:db-before :db-after :tx-data}.")

  (query [this q] [this q inputs]
    "Execute a Datalog query against the current DB snapshot.
     q - query form (vector or map)
     inputs - additional query inputs after the DB
     Returns query results (set of tuples).")

  (entity [this eid]
    "Get an entity by its entity ID.
     Returns the entity map or nil if not found.")

  (entid [this lookup-ref]
    "Resolve a lookup ref to an entity ID.
     lookup-ref - e.g. [:kg-edge/id \"some-id\"]
     Returns entity ID (integer) or nil.")

  (pull-entity [this pattern eid]
    "Pull an entity with a pull pattern.
     pattern - pull pattern e.g. '[*]'
     eid - entity ID (integer)
     Returns pulled entity map with resolved refs.")

  (db-snapshot [this]
    "Get the current database snapshot value.
     Returns the immutable DB value for querying.")

  (reset-conn! [this]
    "Reset the connection to a fresh/empty database.
     Used for testing and state clearing.
     Returns the new connection.")

  (close! [this]
    "Close the connection and release resources.
     No-op for in-memory backends (DataScript).
     Required for Datalevin/Datahike to flush to disk."))

;;; ============================================================================
;;; Active Store Management
;;; ============================================================================

;; Atom holding the currently active IKGStore implementation.
;; Set during system initialization via set-store!
(defonce ^:private active-store (atom nil))

(defn set-store!
  "Set the active graph store implementation.
   Called during system initialization.
   
   Validates that store implements IKGStore protocol.
   Returns the store."
  [store]
  {:pre [(satisfies? IKGStore store)]}
  (reset! active-store store)
  store)

(defn get-store
  "Get the active graph store.
   Throws if no store has been set.
   
   Use store-set? to check before calling if initialization
   state is uncertain."
  []
  (or @active-store
      (throw (ex-info "No graph store configured. Call set-store! first."
                      {:hint "Initialize with datascript-store, datalevin-store, or datahike-store"}))))

(defn store-set?
  "Check if a store has been configured.
   Returns true if set-store! has been called, false otherwise."
  []
  (some? @active-store))

(defn clear-store!
  "Clear the active store. Calls close! on the current store first.
   Used for testing and reinitialization.
   Returns nil."
  []
  (when-let [store @active-store]
    (try
      (close! store)
      (catch Exception _)))
  (reset! active-store nil))

;;; ============================================================================
;;; ITemporalKGStore Protocol (Optional Extension)
;;; ============================================================================

(defprotocol ITemporalKGStore
  "Extended protocol for stores that support temporal queries.

   Datahike implements this protocol for time-travel capabilities.
   DataScript and Datalevin do NOT support this and should return nil.

   Temporal queries enable:
   - Auditing: See what was known at a point in time
   - Debugging: Understand how knowledge evolved
   - Rollback: Query past states without data loss
   - Compliance: Maintain audit trails for decisions"

  (history-db [this]
    "Get a database containing all historical facts.
     Returns a DB value that includes retracted datoms, enabling
     queries over the complete history of the store.
     Returns nil if not supported by backend.")

  (as-of-db [this tx-or-time]
    "Get the database as of a specific point in time.
     tx-or-time can be:
       - A transaction ID (integer)
       - A java.util.Date timestamp
     Returns the DB snapshot at that point, nil if not supported.")

  (since-db [this tx-or-time]
    "Get a database containing only facts added since a point in time.
     tx-or-time can be:
       - A transaction ID (integer)
       - A java.util.Date timestamp
     Returns a filtered DB, nil if not supported."))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defn temporal-store?
  "Check if the given store supports temporal queries.
   Returns true if the store implements ITemporalKGStore."
  [store]
  (satisfies? ITemporalKGStore store))

(defn active-temporal?
  "Check if the currently active store supports temporal queries.
   Returns false if no store is set."
  []
  (and (store-set?)
       (temporal-store? @active-store)))

(defn kg-store?
  "Check if the given object implements IKGStore.
   Returns true if it satisfies the protocol."
  [x]
  (satisfies? IKGStore x))

;;; ============================================================================
;;; NoopKGStore (No-op Fallback)
;;; ============================================================================

(defrecord NoopKGStore []
  IKGStore
  (ensure-conn! [_this] nil)
  (transact! [_this _tx-data] nil)
  (query [_this _q] #{})
  (query [_this _q _inputs] #{})
  (entity [_this _eid] nil)
  (entid [_this _lookup-ref] nil)
  (pull-entity [_this _pattern _eid] nil)
  (db-snapshot [_this] nil)
  (reset-conn! [_this] nil)
  (close! [_this] nil))

(defn noop-store
  "Create a no-op KG store that silently ignores all operations.
   Useful as a fallback when no real backend is configured.
   Returns an IKGStore implementation where:
   - Writes are silently dropped
   - Queries return empty sets
   - Lookups return nil"
  []
  (->NoopKGStore))
