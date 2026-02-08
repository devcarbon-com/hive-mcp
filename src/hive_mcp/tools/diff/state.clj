(ns hive-mcp.tools.diff.state
  "Diff state management: atoms, error helpers, TDD status.

   SOLID: SRP - Centralizes mutable state for diff lifecycle.
   DDD: Domain state - pending diffs and auto-approve rules."
  (:require [hive-mcp.guards :as guards]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Helpers
;; =============================================================================

(defn mcp-error-json
  "Create an error MCP response with JSON-encoded error message."
  [error-message]
  {:type "text"
   :text (json/write-str {:error error-message})
   :isError true})

;; =============================================================================
;; Domain: Pending Diffs State
;; =============================================================================

;; Atom storing pending diff proposals. Map of diff-id -> diff-data.
(defonce pending-diffs (atom {}))

;; =============================================================================
;; Auto-Approve Rules Configuration
;; =============================================================================

(def default-auto-approve-rules
  "Default rules for auto-approving diff proposals.

   A diff is auto-approved only if ALL conditions are met:
   - max-lines-changed: Maximum total lines added + deleted
   - no-deletions-only: Reject changes that only delete code
   - require-description: Require non-empty description
   - allowed-path-patterns: Regex patterns for allowed file paths"
  {:max-lines-changed 100
   :no-deletions-only true
   :require-description true
   :allowed-path-patterns [#".*\.clj[sx]?$"    ; Clojure files
                           #".*\.edn$"         ; EDN config
                           #".*\.md$"          ; Markdown docs
                           #".*\.json$"]})     ; JSON config

(defonce auto-approve-rules (atom default-auto-approve-rules))

(defn clear-pending-diffs!
  "Clear pending diffs. GUARDED - no-op if coordinator running.

   CLARITY-Y: Yield safe failure - prevents test fixtures from
   corrupting production diff state."
  []
  (guards/when-not-coordinator
   "clear-pending-diffs! called"
   (reset! pending-diffs {})))

;; =============================================================================
;; TDD Status Integration (ADR 20260125002853)
;; =============================================================================

(defn update-diff-tdd-status!
  "Update a diff's TDD status after drone runs tests/lint.

   Arguments:
     diff-id    - ID of the diff to update
     tdd-status - Map with :kondo and/or :tests keys:
                  {:kondo {:clean true/false :errors [...]}
                   :tests {:passed true/false :count N :duration-ms N}}

   Returns updated diff or nil if not found.

   Usage by drones:
   1. Propose diff -> get diff-id
   2. Run kondo lint -> update-diff-tdd-status! with :kondo results
   3. Run tests -> update-diff-tdd-status! with :tests results
   4. Ling reviews -> sees TDD status in list_proposed_diffs"
  [diff-id tdd-status]
  (when (contains? @pending-diffs diff-id)
    (swap! pending-diffs update diff-id
           (fn [diff]
             (update diff :tdd-status
                     (fn [existing]
                       (merge existing tdd-status)))))
    (log/info "Updated diff TDD status" {:diff-id diff-id :tdd-status tdd-status})
    (get @pending-diffs diff-id)))
