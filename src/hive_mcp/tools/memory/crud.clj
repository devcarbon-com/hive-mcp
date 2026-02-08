(ns hive-mcp.tools.memory.crud
  "CRUD facade for memory operations - thin re-export namespace.

   Extracted into focused submodules for SRP compliance:
   - crud.write:    add entry with KG edge creation
   - crud.query:    query with scope hierarchy filtering
   - crud.retrieve: get-full, batch-get, check-duplicate, update-tags

   Intelligence submodules (unchanged):
   - classify.clj: Abstraction level auto-classification (P1.5)
   - gaps.clj: Knowledge gap auto-detection (P2.8)

   This namespace re-exports all handler fns for backward compatibility.
   Callers (memory.clj, drone/feedback.clj, memory_kanban.clj) continue
   to require hive-mcp.tools.memory.crud without changes."
  (:require [hive-mcp.tools.memory.crud.write :as write]
            [hive-mcp.tools.memory.crud.query :as query]
            [hive-mcp.tools.memory.crud.retrieve :as retrieve]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Write Operations (crud.write)
;; ============================================================

(def handle-add
  "Add an entry to project memory with optional KG edge creation."
  write/handle-add)

;; ============================================================
;; Query Operations (crud.query)
;; ============================================================

(def handle-query
  "Query project memory by type with scope filtering."
  query/handle-query)

(def handle-query-metadata
  "DEPRECATED: Use handle-query with verbosity='metadata'."
  query/handle-query-metadata)

(def apply-auto-scope-filter
  "Filter entries for auto-scope mode (nil scope).
   Re-exported for HCR test compatibility (resolved via @(resolve ...))."
  query/apply-auto-scope-filter)

;; ============================================================
;; Retrieve Operations (crud.retrieve)
;; ============================================================

(def handle-get-full
  "Get full content of a memory entry by ID with KG edges."
  retrieve/handle-get-full)

(def handle-batch-get
  "Get multiple memory entries by IDs in a single call."
  retrieve/handle-batch-get)

(def handle-check-duplicate
  "Check if content already exists in memory."
  retrieve/handle-check-duplicate)

(def handle-update-tags
  "Update tags on an existing memory entry."
  retrieve/handle-update-tags)
