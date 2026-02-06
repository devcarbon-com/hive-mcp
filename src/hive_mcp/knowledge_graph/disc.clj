(ns hive-mcp.knowledge-graph.disc
  "Disc entity management for L1 (file) abstraction level.

   Disc entities track the actual state of files on disk, enabling:
   - Grounding verification without re-reading files
   - Change detection via content hash comparison
   - Git commit tracking for provenance
   - Bayesian certainty tracking with automatic event wiring

   FACADE: This namespace delegates to sub-namespaces:
   - disc.hash       — Content hashing (pure)
   - disc.volatility — Volatility classification, Bayesian certainty, staleness scoring (pure)
   - disc.crud       — DataScript CRUD operations
   - disc.staleness  — Staleness surfacing + KG-first context
   - disc.propagation — Certainty wiring + transitive staleness propagation

   CLARITY-Y: Graceful failure with status codes instead of exceptions."
  (:require [hive-mcp.knowledge-graph.disc.hash :as hash]
            [hive-mcp.knowledge-graph.disc.volatility :as vol]
            [hive-mcp.knowledge-graph.disc.crud :as crud]
            [hive-mcp.knowledge-graph.disc.staleness :as staleness]
            [hive-mcp.knowledge-graph.disc.propagation :as propagation]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exports: Hash (disc.hash)
;; =============================================================================

(def compute-hash
  "Compute SHA-256 hash of content string. Returns hex string."
  hash/compute-hash)

(def file-content-hash
  "Read file and compute content hash.
   Returns {:hash \"..\" :exists? true} or {:exists? false}."
  hash/file-content-hash)

;; =============================================================================
;; Re-exports: Volatility & Constants (disc.volatility)
;; =============================================================================

(def volatility-patterns
  "File patterns for volatility classification."
  vol/volatility-patterns)

(def classify-volatility
  "Classify file volatility based on path patterns.
   Returns :stable, :moderate, or :volatile"
  vol/classify-volatility)

(def decay-rates
  "Daily certainty decay rates by volatility class."
  vol/decay-rates)

(def propagation-relations
  "KG edge types that should propagate staleness transitively."
  vol/propagation-relations)

(def staleness-decay-factor
  "Decay factor per hop in staleness propagation."
  vol/staleness-decay-factor)

(def staleness-min-threshold
  "Minimum staleness to propagate (stop propagation below this)."
  vol/staleness-min-threshold)

(def staleness-max-depth
  "Maximum depth for staleness propagation."
  vol/staleness-max-depth)

(def base-staleness-values
  "Base staleness values by source event type."
  vol/base-staleness-values)

;; Bayesian certainty (pure)
(def current-certainty
  "Expected certainty: alpha / (alpha + beta)."
  vol/current-certainty)

(def beta-lower-bound
  "Lower bound of 95% credible interval for certainty."
  vol/beta-lower-bound)

(def needs-read?
  "True if certainty below threshold or credible interval too wide."
  vol/needs-read?)

(def update-certainty
  "Update certainty based on observation event (pure, does not persist)."
  vol/update-certainty)

(def apply-time-decay
  "Apply time-based decay to disc certainty (pure, does not persist)."
  vol/apply-time-decay)

;; Entry staleness (pure)
(def entry-staleness-score
  "Compute staleness score for a Chroma entry."
  vol/entry-staleness-score)

(def entry-staleness-report
  "Generate staleness report for a Chroma entry."
  vol/entry-staleness-report)

;; =============================================================================
;; Re-exports: CRUD (disc.crud)
;; =============================================================================

(def add-disc!
  "Create or update a disc entity for a file path."
  crud/add-disc!)

(def get-disc
  "Get disc entity by file path."
  crud/get-disc)

(def get-disc-by-id
  "Get disc entity by entity ID."
  crud/get-disc-by-id)

(def disc-exists?
  "Check if a disc entity exists for the given path."
  crud/disc-exists?)

(def update-disc!
  "Update a disc entity."
  crud/update-disc!)

(def remove-disc!
  "Remove a disc entity by path."
  crud/remove-disc!)

(def get-all-discs
  "Get all disc entities. Optional project-id filter."
  crud/get-all-discs)

(def get-stale-discs
  "Get disc entities with content hash mismatch."
  crud/get-stale-discs)

(def refresh-disc!
  "Refresh a disc entity by re-reading the file."
  crud/refresh-disc!)

(def touch-disc!
  "Record that a file was read by an agent."
  crud/touch-disc!)

(def disc-stats
  "Get statistics about disc entities."
  crud/disc-stats)

;; =============================================================================
;; Re-exports: Staleness Surfacing (disc.staleness)
;; =============================================================================

(def staleness-score
  "Compute staleness score for a disc entity (impure, reads file)."
  staleness/staleness-score)

(def staleness-report
  "Compute staleness score and diagnostic info in one pass."
  staleness/staleness-report)

(def staleness-warnings
  "Generate staleness warnings for a collection of file paths."
  staleness/staleness-warnings)

(def format-staleness-warnings
  "Format staleness warnings as a text block for injection into task prompts."
  vol/format-staleness-warnings)

(def top-stale-files
  "Query top-N most stale disc entities."
  staleness/top-stale-files)

(def kg-first-context
  "Consult the KG before file reads — the Structural Differential principle."
  staleness/kg-first-context)

;; =============================================================================
;; Re-exports: Propagation & Certainty Wiring (disc.propagation)
;; =============================================================================

(def update-disc-certainty!
  "Update certainty for a disc entity and persist."
  propagation/update-disc-certainty!)

(def reground-disc!
  "Re-ground a disc entity by verifying against the actual file."
  propagation/reground-disc!)

(def on-git-commit-touched
  "Called when a git commit affects a disc's file."
  propagation/on-git-commit-touched)

(def reground-stale-discs!
  "Re-ground all discs that need verification."
  propagation/reground-stale-discs!)

(def apply-time-decay-to-all-discs!
  "Apply time decay to all disc entities in DataScript."
  propagation/apply-time-decay-to-all-discs!)

(def apply-transitive-staleness!
  "Apply staleness to a single Chroma entry with decay based on depth."
  propagation/apply-transitive-staleness!)

(def propagate-staleness!
  "Propagate staleness from a disc to dependent labels via KG edges."
  propagation/propagate-staleness!)

(def stale-entries-report
  "Query Chroma for entries with staleness above threshold."
  propagation/stale-entries-report)
