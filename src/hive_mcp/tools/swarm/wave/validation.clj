(ns hive-mcp.tools.swarm.wave.validation
  "Wave validation module for pre-flight and post-apply checks.

   CLARITY-I (Inputs Guarded): Validate all inputs at boundaries.
   CLARITY-Y (Yield Safe Failure): Graceful handling of validation failures.

   Validation types:
   - Path validation: Ensure directories exist
   - Lint validation: Run clj-kondo before applying diffs
   - Compile check: Verify namespaces load after changes"
  (:require [clj-kondo.core :as kondo]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Path Validation (Pre-flight)
;;; ============================================================

(defn ensure-parent-dirs!
  "Create parent directories for all task files.
   Called during pre-flight to prevent drone failures from missing directories.

   Arguments:
     tasks - Collection of maps with :file key

   Returns:
     Number of directories created.

   CLARITY-I: Guard inputs before processing."
  [tasks]
  (let [created (atom 0)]
    (doseq [{:keys [file]} tasks]
      (when file
        (let [parent (.getParentFile (io/file file))]
          (when (and parent (not (.exists parent)))
            (io/make-parents file)
            (swap! created inc)))))
    @created))

(defn valid-parent-path?
  "Check if parent directory exists.
   Returns true if:
   - file-path is nil (no validation needed)
   - parent directory exists

   Note: This is strict validation. Use ensure-parent-dirs! first
   to create directories, then validate remaining issues."
  [file-path]
  (if (nil? file-path)
    true
    (let [parent (.getParentFile (io/file file-path))]
      (or (nil? parent)
          (.exists parent)))))

(defn validate-task-paths
  "Validate all task paths have accessible parent directories.
   Throws ex-info with :invalid-paths if any paths are invalid.

   Arguments:
     tasks - Collection of maps with :file key

   Returns:
     true if all paths valid

   Throws:
     ex-info with :invalid-paths on failure

   CLARITY-I: Fail fast at boundaries."
  [tasks]
  (let [invalid (->> tasks
                     (map :file)
                     (remove nil?)
                     (remove valid-parent-path?)
                     (vec))]
    (if (seq invalid)
      (throw (ex-info "Invalid paths in wave"
                      {:error-type :validation
                       :invalid-paths invalid}))
      true)))

(defn path-validation-summary
  "Get summary of path validation without throwing.

   Arguments:
     tasks - Collection of maps with :file key

   Returns:
     Map with :valid? :invalid-count :invalid-paths"
  [tasks]
  (let [invalid (->> tasks
                     (map :file)
                     (remove nil?)
                     (remove valid-parent-path?)
                     (vec))]
    {:valid? (empty? invalid)
     :invalid-count (count invalid)
     :invalid-paths invalid}))

;;; ============================================================
;;; Lint Validation (Pre-Apply)
;;; ============================================================

(defn lint-file!
  "Run clj-kondo lint on a single file.

   Arguments:
     file-path - Path to file to lint
     level     - Minimum severity (:error :warning :info)

   Returns:
     Map with :valid? :errors :warnings :info-count"
  [file-path & [{:keys [level] :or {level :error}}]]
  (try
    (let [result (kondo/run! {:lint [file-path]})
          findings (:findings result)
          errors (filter #(= :error (:level %)) findings)
          warnings (filter #(= :warning (:level %)) findings)
          info-msgs (filter #(= :info (:level %)) findings)]
      {:valid? (case level
                 :error (empty? errors)
                 :warning (and (empty? errors) (empty? warnings))
                 :info (empty? findings))
       :errors (vec errors)
       :warnings (vec warnings)
       :info-count (count info-msgs)
       :file file-path})
    (catch Exception e
      (log/warn "Lint failed for" file-path ":" (ex-message e))
      {:valid? false
       :errors [{:message (str "Lint error: " (ex-message e))
                 :file file-path}]
       :warnings []
       :info-count 0
       :file file-path})))

(defn lint-before-apply!
  "Run clj-kondo on proposed diff content before applying.
   Prevents introducing lint errors.

   Arguments:
     diff-content - String content of the diff (new file contents)
     file-path    - Target file path (for context)
     opts         - Options map:
                    :level - Minimum severity (:error :warning :info)

   Returns:
     Map with :valid? :errors :warnings

   Note: This lints the content string, not the file on disk."
  [diff-content file-path & [{:keys [level] :or {level :error}}]]
  (try
    ;; Create temp file with diff content for linting
    (let [temp-file (java.io.File/createTempFile "lint-" ".clj")
          _ (spit temp-file diff-content)
          result (lint-file! (.getAbsolutePath temp-file) {:level level})]
      ;; Clean up temp file
      (.delete temp-file)
      ;; Return result with original file path
      (assoc result :file file-path))
    (catch Exception e
      (log/warn "Lint-before-apply failed for" file-path ":" (ex-message e))
      {:valid? false
       :errors [{:message (str "Lint error: " (ex-message e))
                 :file file-path}]
       :warnings []
       :file file-path})))

(defn lint-files!
  "Lint multiple files, aggregating results.

   Arguments:
     files - Collection of file paths
     opts  - Options map with :level

   Returns:
     Map with :valid? :total-errors :total-warnings :results"
  [files & [opts]]
  (let [results (mapv #(lint-file! % opts) files)
        total-errors (reduce + (map #(count (:errors %)) results))
        total-warnings (reduce + (map #(count (:warnings %)) results))]
    {:valid? (every? :valid? results)
     :total-errors total-errors
     :total-warnings total-warnings
     :results results}))

;;; ============================================================
;;; Compile Check (Post-Apply)
;;; ============================================================

(defn file->namespace
  "Convert file path to Clojure namespace.

   Examples:
     src/foo/bar.clj -> foo.bar
     test/foo/bar_test.clj -> foo.bar-test"
  [file-path]
  (when file-path
    (-> file-path
        (str/replace #"^.*/(?:src|test)/" "")
        (str/replace #"\.clj[sx]?$" "")
        (str/replace "/" ".")
        (str/replace "_" "-")
        (symbol))))

(defn compile-check!
  "After applying diffs, try to require affected namespaces.
   Catches compile errors early.

   Arguments:
     files - Collection of file paths that were modified

   Returns:
     Map with :success? :failed-ns :errors

   CLARITY-Y: Graceful partial success - reports all failures."
  [files]
  (let [namespaces (->> files
                        (map file->namespace)
                        (remove nil?)
                        (distinct))
        results (atom {:success? true :failed-ns [] :errors []})]

    (doseq [ns-sym namespaces]
      (try
        ;; Remove the namespace first to force reload
        (when (find-ns ns-sym)
          (remove-ns ns-sym))
        ;; Try to require it
        (require ns-sym :reload)
        (catch Exception e
          (swap! results update :success? (constantly false))
          (swap! results update :failed-ns conj ns-sym)
          (swap! results update :errors conj
                 {:namespace ns-sym
                  :error (ex-message e)
                  :type (type e)}))))

    @results))

(defn compile-check-safe!
  "Compile check with exception handling (won't throw).

   Returns:
     Same as compile-check! but catches top-level errors."
  [files]
  (try
    (compile-check! files)
    (catch Exception e
      (log/error "Compile check failed:" (ex-message e))
      {:success? false
       :failed-ns []
       :errors [{:error (str "Compile check error: " (ex-message e))
                 :type :check-failure}]})))

;;; ============================================================
;;; Combined Validation
;;; ============================================================

(defn pre-flight-validation!
  "Run all pre-flight validations for wave execution.

   Arguments:
     tasks - Collection of task maps with :file key
     opts  - Options map:
             :ensure-dirs    - Create parent dirs (default: true)
             :validate-paths - Fail on invalid paths (default: true)

   Returns:
     Map with :valid? :dirs-created :path-summary

   Throws:
     ex-info if validate-paths is true and paths are invalid"
  [tasks & [{:keys [ensure-dirs validate-paths]
             :or {ensure-dirs true validate-paths true}}]]
  (let [;; Step 1: Ensure parent directories
        dirs-created (if ensure-dirs
                       (ensure-parent-dirs! tasks)
                       0)
        ;; Step 2: Validate paths (after ensuring dirs)
        path-summary (path-validation-summary tasks)]

    ;; Throw if validation required and failed
    (when (and validate-paths (not (:valid? path-summary)))
      (throw (ex-info "Invalid paths in wave"
                      {:error-type :validation
                       :invalid-paths (:invalid-paths path-summary)})))

    {:valid? (:valid? path-summary)
     :dirs-created dirs-created
     :path-summary path-summary}))

(defn post-apply-validation!
  "Run all post-apply validations after diffs are applied.

   Arguments:
     files - Collection of modified file paths
     opts  - Options map:
             :lint          - Run lint check (default: true)
             :lint-level    - Lint severity (:error :warning :info)
             :compile-check - Run compile check (default: true)

   Returns:
     Map with :valid? :lint-result :compile-result"
  [files & [{:keys [lint lint-level compile-check]
             :or {lint true lint-level :error compile-check true}}]]
  (let [lint-result (when lint
                      (lint-files! files {:level lint-level}))
        compile-result (when compile-check
                         (compile-check-safe! files))
        valid? (and (or (nil? lint-result) (:valid? lint-result))
                    (or (nil? compile-result) (:success? compile-result)))]

    {:valid? valid?
     :lint-result lint-result
     :compile-result compile-result}))

(defn validation-summary
  "Create a human-readable validation summary.

   Arguments:
     pre-result  - Result from pre-flight-validation!
     post-result - Result from post-apply-validation! (optional)

   Returns:
     String summary"
  [pre-result & [post-result]]
  (let [parts [(format "Pre-flight: %s (dirs created: %d)"
                       (if (:valid? pre-result) "PASS" "FAIL")
                       (or (:dirs-created pre-result) 0))]]
    (when post-result
      (conj parts
            (format "Post-apply: %s (lint errors: %d, compile failures: %d)"
                    (if (:valid? post-result) "PASS" "FAIL")
                    (get-in post-result [:lint-result :total-errors] 0)
                    (count (get-in post-result [:compile-result :failed-ns] [])))))
    (str/join " | " parts)))
