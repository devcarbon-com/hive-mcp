(ns hive-mcp.tools.diff.auto-approve
  "Auto-approve rules and validation for diff proposals.

   Evaluates diffs against configurable safety criteria:
   - max-lines-changed
   - no-deletions-only
   - require-description
   - allowed-path-patterns

   SOLID: SRP - Approval policy logic separated from state and handlers."
  (:require [hive-mcp.tools.diff.state :as state]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Auto-Approve Validation
;; =============================================================================

(defn- count-line-changes
  "Count total lines changed (added + deleted) in a diff."
  [old-content new-content]
  (let [old-lines (count (str/split-lines (or old-content "")))
        new-lines (count (str/split-lines (or new-content "")))]
    (+ (max 0 (- old-lines new-lines))  ; deleted lines
       (max 0 (- new-lines old-lines))))) ; added lines

(defn- deletions-only?
  "Check if the change only deletes content without adding anything."
  [old-content new-content]
  (and (not (str/blank? old-content))
       (or (str/blank? new-content)
           (< (count new-content) (/ (count old-content) 2)))))

(defn- path-matches-patterns?
  "Check if file path matches any of the allowed patterns."
  [file-path patterns]
  (if (empty? patterns)
    true  ; No patterns = allow all
    (some #(re-matches % file-path) patterns)))

(defn auto-approve-diff?
  "Check if a diff meets auto-approve criteria.

   Arguments:
     diff  - Diff proposal map
     rules - Optional rules (defaults to @auto-approve-rules)

   Returns {:approved true} or {:approved false :reason \"...\"}."
  ([diff] (auto-approve-diff? diff @state/auto-approve-rules))
  ([{:keys [old-content new-content file-path description]} rules]
   (let [{:keys [max-lines-changed no-deletions-only
                 require-description allowed-path-patterns]} rules
         line-changes (count-line-changes old-content new-content)]
     (cond
       ;; Check line count
       (and max-lines-changed (> line-changes max-lines-changed))
       {:approved false
        :reason (str "Too many lines changed: " line-changes " > " max-lines-changed)}

       ;; Check deletions-only
       (and no-deletions-only (deletions-only? old-content new-content))
       {:approved false
        :reason "Change only deletes content - requires manual review"}

       ;; Check description
       (and require-description (str/blank? description))
       {:approved false
        :reason "Missing description - requires manual review"}

       ;; Check path pattern
       (and (seq allowed-path-patterns)
            (not (path-matches-patterns? file-path allowed-path-patterns)))
       {:approved false
        :reason (str "File path not in allowed patterns: " file-path)}

       ;; All checks passed
       :else
       {:approved true}))))

(defn get-auto-approve-rules
  "Get current auto-approve rules with descriptions.

   Returns the rules map with human-readable format."
  []
  (let [rules @state/auto-approve-rules]
    {:max-lines-changed (:max-lines-changed rules)
     :must-pass-lint false  ; Not implemented yet - future enhancement
     :no-deletions-only (:no-deletions-only rules)
     :require-description (:require-description rules)
     :allowed-path-patterns (mapv str (:allowed-path-patterns rules))}))

(defn safe-to-auto-approve?
  "Check if diff meets auto-approve criteria.

   Alias for auto-approve-diff? with more descriptive name.
   Returns true if diff can be safely auto-approved."
  ([diff] (safe-to-auto-approve? diff @state/auto-approve-rules))
  ([diff rules]
   (:approved (auto-approve-diff? diff rules))))
