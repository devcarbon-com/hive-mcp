(ns hive-mcp.tools.diff.handlers
  "Core MCP handlers for diff operations.

   Handles propose, list, apply, reject, and details operations.
   Wave and batch handlers are in separate modules.

   Token-Efficient Three-Tier API (ADR 20260125002853):
   - Tier 1: list_proposed_diffs -> metadata + metrics only (~200 tokens/diff)
   - Tier 2: get_diff_details -> formatted hunks (~500 tokens/diff)
   - Tier 3: apply_diff -> uses stored full content internally

   SOLID: SRP - Core CRUD handlers only."
  (:require [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.tools.diff.state :as state :refer [mcp-error-json]]
            [hive-mcp.tools.diff.compute :as compute]
            [hive-mcp.tools.diff.validation :as validation]
            [hive-mcp.tools.diff.auto-approve :as auto-approve]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Application: Handlers
;; =============================================================================

(defn handle-propose-diff
  "Handle propose_diff tool call.
   Stores a proposed diff for review by the hivemind.
   Translates sandbox paths and validates file paths.

   BUG FIX: Uses directory parameter or ctx/current-directory as project root
   for path validation. This prevents 'Path escapes project directory' errors
   when drones work on projects outside the MCP server's working directory."
  [{:keys [file_path _old_content _new_content _description drone_id directory] :as params}]
  ;; CRITICAL FIX: Get project root from context or explicit parameter
  ;; Without this, path validation uses MCP server's cwd, not drone's project
  (let [project-root (or directory
                         (ctx/current-directory)
                         (validation/get-project-root))]
    (log/debug "propose_diff called" {:file file_path :drone drone_id :project-root project-root})
    (if-let [error (validation/validate-propose-params params)]
      (do
        (log/warn "propose_diff validation failed" {:error error})
        (mcp-error-json error))
      ;; Translate sandbox paths before validation
      (let [translated-path (validation/translate-sandbox-path file_path)
            _ (when (not= translated-path file_path)
                (log/info "Translated sandbox path" {:from file_path :to translated-path}))
            ;; Use project-root from context for path validation
            path-result (validation/validate-diff-path translated-path project-root)]
        (if-not (:valid path-result)
          (do
            (log/warn "propose_diff path validation failed"
                      {:file translated-path :original file_path :error (:error path-result) :drone drone_id})
            (mcp-error-json (:error path-result)))
          (try
            ;; Use the resolved path for the proposal
            (let [resolved-path (:resolved-path path-result)
                  proposal (compute/create-diff-proposal (assoc params :file_path resolved-path))]
              (swap! state/pending-diffs assoc (:id proposal) proposal)
              (log/info "Diff proposed" {:id (:id proposal)
                                         :file resolved-path
                                         :original-path file_path
                                         :drone drone_id})
              (mcp-json {:id (:id proposal)
                         :status "pending"
                         :file-path resolved-path
                         :original-path (when (not= file_path resolved-path) file_path)
                         :description (:description proposal)
                         :message "Diff proposed for review. Hivemind will apply or reject."}))
            (catch Exception e
              (log/error e "Failed to propose diff")
              (mcp-error-json (str "Failed to propose diff: " (.getMessage e))))))))))

(defn handle-list-proposed-diffs
  "Handle list_proposed_diffs tool call.
   Returns all pending diffs, optionally filtered by drone_id.

   Token-Efficient Tier 1 Response (ADR 20260125002853):
   Returns ONLY metadata: id, file-path, description, drone-id, wave-id,
   status, created-at, metrics, tdd-status. (~200 tokens/diff)

   Does NOT return: old-content, new-content, hunks, unified-diff."
  [{:keys [drone_id]}]
  (log/debug "list_proposed_diffs called" {:drone_id drone_id})
  (try
    (let [all-diffs (vals @state/pending-diffs)
          filtered (if (str/blank? drone_id)
                     all-diffs
                     (filter #(= drone_id (:drone-id %)) all-diffs))
          ;; Tier 1: Metadata only - dissoc content AND hunks
          safe-diffs (map (fn [d]
                            (-> d
                                (update :created-at str)
                                (dissoc :old-content :new-content  ; Tier 3
                                        :hunks                     ; Tier 2
                                        :unified-diff)))           ; Legacy
                          filtered)]
      (log/info "Listed proposed diffs" {:count (count safe-diffs)})
      (mcp-json {:count (count safe-diffs)
                 :diffs (vec safe-diffs)}))
    (catch Exception e
      (log/error e "Failed to list proposed diffs")
      (mcp-error-json (str "Failed to list diffs: " (.getMessage e))))))

(defn handle-apply-diff
  "Handle apply_diff tool call.
   Applies the diff by finding and replacing old-content within the file.
   If old-content is empty and file doesn't exist, creates a new file.

   CLARITY-Y: Defense-in-depth validation blocks empty new_content even if
   it passed propose_diff, preventing 0-byte file writes from LLM failures."
  [{:keys [diff_id]}]
  (log/debug "apply_diff called" {:diff_id diff_id})
  (cond
    (str/blank? diff_id)
    (do
      (log/warn "apply_diff missing diff_id")
      (mcp-error-json "Missing required field: diff_id"))

    (not (contains? @state/pending-diffs diff_id))
    (do
      (log/warn "apply_diff diff not found" {:diff_id diff_id})
      (mcp-error-json (str "Diff not found: " diff_id)))

    :else
    (let [{:keys [file-path old-content new-content]} (get @state/pending-diffs diff_id)
          file-exists? (.exists (io/file file-path))
          creating-new-file? (and (str/blank? old-content) (not file-exists?))]
      ;; CLARITY-Y: Defense-in-depth - block empty content even if it passed propose_diff
      (cond
        ;; Block empty new_content (prevents 0-byte file writes)
        (str/blank? new-content)
        (do
          (log/warn "apply_diff blocked: empty new_content" {:diff_id diff_id :file file-path})
          (swap! state/pending-diffs dissoc diff_id)
          (mcp-error-json "Cannot apply diff: new_content is empty or whitespace-only. This typically indicates the LLM returned an empty response."))

        ;; Case 1: Creating a new file (old-content empty, file doesn't exist)
        creating-new-file?
        (try
          (let [parent (.getParentFile (io/file file-path))]
            (when (and parent (not (.exists parent)))
              (.mkdirs parent)))
          (spit file-path new-content)
          (swap! state/pending-diffs dissoc diff_id)
          (log/info "New file created" {:id diff_id :file file-path})
          (mcp-json {:id diff_id
                     :status "applied"
                     :file-path file-path
                     :created true
                     :message "New file created successfully"})
          (catch Exception e
            (log/error e "Failed to create file" {:diff_id diff_id})
            (mcp-error-json (str "Failed to create file: " (.getMessage e)))))

        ;; Case 2: File doesn't exist but old-content is not empty - error
        (not file-exists?)
        (do
          (log/warn "apply_diff file not found" {:file file-path})
          (mcp-error-json (str "File not found: " file-path)))

        ;; Case 3: Normal replacement in existing file
        :else
        (try
          (let [current-content (slurp file-path)]
            (cond
              ;; old-content not found in file
              (not (str/includes? current-content old-content))
              (do
                (log/warn "apply_diff old content not found in file" {:file file-path})
                (mcp-error-json "Old content not found in file. File may have been modified since diff was proposed."))

              ;; Multiple occurrences - ambiguous
              (> (count (re-seq (re-pattern (java.util.regex.Pattern/quote old-content)) current-content)) 1)
              (do
                (log/warn "apply_diff multiple matches found" {:file file-path})
                (mcp-error-json "Multiple occurrences of old content found. Cannot apply safely - diff is ambiguous."))

              ;; Exactly one match - apply the replacement
              :else
              (do
                (spit file-path (str/replace-first current-content old-content new-content))
                (swap! state/pending-diffs dissoc diff_id)
                (log/info "Diff applied" {:id diff_id :file file-path})
                (mcp-json {:id diff_id
                           :status "applied"
                           :file-path file-path
                           :message "Diff applied successfully"}))))
          (catch Exception e
            (log/error e "Failed to apply diff" {:diff_id diff_id})
            (mcp-error-json (str "Failed to apply diff: " (.getMessage e)))))))))

(defn handle-reject-diff
  "Handle reject_diff tool call.
   Removes the diff from pending without applying."
  [{:keys [diff_id reason]}]
  (log/debug "reject_diff called" {:diff_id diff_id :reason reason})
  (cond
    (str/blank? diff_id)
    (do
      (log/warn "reject_diff missing diff_id")
      (mcp-error-json "Missing required field: diff_id"))

    (not (contains? @state/pending-diffs diff_id))
    (do
      (log/warn "reject_diff diff not found" {:diff_id diff_id})
      (mcp-error-json (str "Diff not found: " diff_id)))

    :else
    (let [{:keys [file-path drone-id]} (get @state/pending-diffs diff_id)]
      ;; Remove from pending (don't apply)
      (swap! state/pending-diffs dissoc diff_id)
      (log/info "Diff rejected" {:id diff_id :file file-path :reason reason})
      (mcp-json {:id diff_id
                 :status "rejected"
                 :file-path file-path
                 :drone-id drone-id
                 :reason (or reason "No reason provided")
                 :message "Diff rejected and discarded"}))))

(defn handle-get-diff-details
  "Handle get_diff_details tool call.
   Returns diff details with formatted hunks for review.

   Token-Efficient Tier 2 Response (ADR 20260125002853):
   Returns: all metadata + :unified-diff (formatted from hunks on-demand)
   Does NOT return: raw old-content, new-content (tier-3 internal only)

   ~500 tokens/diff - use for detailed inspection before approve/reject."
  [{:keys [diff_id]}]
  (log/debug "get_diff_details called" {:diff_id diff_id})
  (cond
    (str/blank? diff_id)
    (mcp-error-json "Missing required field: diff_id")

    (not (contains? @state/pending-diffs diff_id))
    (mcp-error-json (str "Diff not found: " diff_id))

    :else
    (let [diff (get @state/pending-diffs diff_id)
          ;; Format hunks as unified diff on-demand (not stored)
          formatted-diff (compute/format-hunks-as-unified (:hunks diff) (:file-path diff))]
      (mcp-json (-> diff
                    (update :created-at str)
                    (dissoc :old-content :new-content)  ; Never expose tier-3
                    (assoc :unified-diff formatted-diff))))))

(defn handle-get-auto-approve-rules
  "Handle get_auto_approve_rules tool call.
   Returns the current auto-approve rules configuration."
  [_params]
  (log/debug "get_auto_approve_rules called")
  (mcp-json (auto-approve/get-auto-approve-rules)))
