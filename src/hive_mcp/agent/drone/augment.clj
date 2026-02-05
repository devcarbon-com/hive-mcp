(ns hive-mcp.agent.drone.augment
  "Task augmentation for drone execution.

   Extracted from drone.clj to reduce complexity (SOLID-S: Single Responsibility).
   This namespace handles all context gathering and task augmentation:
   - Memory context (conventions, decisions, snippets)
   - File content pre-injection
   - Smart context (imports, lint warnings, related functions)
   - KG-first optimization (skip reads for known files)

   CLARITY-L: Pure functions where possible, infrastructure at boundaries."
  (:require [hive-mcp.agent.registry :as registry]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.context :as ctx]
            [hive-mcp.agent.drone.kg-context :as kg-ctx]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [hive-mcp.tools.diff :as diff]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;;; ============================================================
;;; Memory Context Gathering
;;; ============================================================

(defn prepare-context
  "Prepare context for drone delegation by gathering catchup data.

   Returns map with:
     :conventions - List of convention content
     :decisions   - List of decision content
     :snippets    - List of code snippets
     :project     - Project metadata

   Returns empty map on failure (CLARITY-Y: Yield safe failure)."
  []
  (try
    (let [catchup-handler (registry/get-tool "mcp_get_context")
          context (when (:handler catchup-handler)
                    ((:handler catchup-handler) {}))]
      (if (and context (:text context))
        (let [parsed (json/read-str (:text context) :key-fn keyword)]
          {:conventions (get-in parsed [:memory :conventions] [])
           :decisions (get-in parsed [:memory :decisions] [])
           :snippets (get-in parsed [:memory :snippets] [])
           :project (get parsed :project {})})
        {}))
    (catch Exception e
      (log/warn e "Failed to gather ling context")
      {})))

(defn format-context-str
  "Format context data as string for task augmentation.

   Arguments:
     context - Map with :conventions and :decisions keys

   Returns:
     Formatted string with headers, or nil if empty."
  [context]
  (when (seq context)
    (let [sections (cond-> ""
                     (seq (:conventions context))
                     (str "### Conventions\n"
                          (str/join "\n" (map :content (:conventions context)))
                          "\n\n")

                     (seq (:decisions context))
                     (str "### Decisions\n"
                          (str/join "\n" (map :content (:decisions context)))
                          "\n\n"))]
      (when (seq sections)
        (str "## Project Context\n" sections)))))

;;; ============================================================
;;; File Content Pre-Injection
;;; ============================================================

(defn format-file-contents
  "Pre-read file contents so drone has exact content for propose_diff.
   Records disc entity reads for L1 file state tracking.

   Arguments:
     files        - List of file paths to read
     project-root - Project root for path containment validation

   Returns:
     Formatted string with file contents, or nil if empty.

   CLARITY-I: Validates paths don't escape project directory before reading."
  [files project-root]
  (when (seq files)
    (let [effective-root (or project-root (diff/get-project-root) "")
          contents (for [f files]
                     ;; SECURITY FIX: Validate path containment before reading
                     (let [validation (sandbox/validate-path-containment f effective-root)]
                       (if (:valid? validation)
                         (try
                           (let [content (slurp (:canonical-path validation))]
                             ;; Track disc read (async, non-blocking)
                             (future
                               (try
                                 (kg-disc/touch-disc! (:canonical-path validation))
                                 (catch Exception e
                                   (log/debug "Disc touch failed (non-fatal):" (.getMessage e)))))
                             (str "### " f "\n```\n" content "```\n"))
                           (catch Exception e
                             (str "### " f "\n(File not found or unreadable: " (.getMessage e) ")\n")))
                         ;; Path escapes project - reject with security warning
                         (do
                           (log/warn "Path validation failed in format-file-contents"
                                     {:file f :error (:error validation)})
                           (str "### " f "\n(BLOCKED: " (:error validation) ")\n")))))]
      (str "## Current File Contents\n"
           "IMPORTANT: Use this EXACT content as old_content in propose_diff.\n"
           "Do NOT guess or assume file content - use what is provided below.\n\n"
           (str/join "\n" contents)))))

;;; ============================================================
;;; Smart Context Building
;;; ============================================================

(defn build-smart-context
  "Build smart context for each target file.

   Gathers:
   - Imports/requires from ns form
   - Related function signatures
   - Existing lint warnings
   - Relevant conventions from memory

   Arguments:
     files        - List of file paths
     task         - Task description
     project-root - Project root directory
     project-id   - Project ID for memory scoping

   Returns:
     Formatted context string, or nil if empty."
  [files task project-root project-id]
  (when (seq files)
    (let [contexts (for [f files]
                     (try
                       (let [ctx-data (ctx/build-drone-context
                                        {:file-path f
                                         :task task
                                         :project-root project-root
                                         :project-id project-id})]
                         (when (:formatted ctx-data)
                           (str "## Smart Context for " f "\n"
                                (:formatted ctx-data))))
                       (catch Exception e
                         (log/debug "Could not build smart context for" f (.getMessage e))
                         nil)))
          non-nil-contexts (remove nil? contexts)]
      (when (seq non-nil-contexts)
        (str/join "\n\n" non-nil-contexts)))))

;;; ============================================================
;;; Main Augmentation Function
;;; ============================================================

(defn augment-task
  "Augment task with context and file contents using KG-first approach.

   KG-First Flow:
   1. Consult KG for existing knowledge about files
   2. For :kg-known files -> inject KG summary (skip file read)
   3. For :needs-read/:stale -> read file contents

   Arguments:
     task         - Task description
     files        - List of files to include
     opts         - Options map with:
       :project-root    - Optional project root override
       :project-id      - Optional project ID for memory scoping
       :use-kg-first    - Whether to use KG-first approach (default: true)
       :return-metadata - If true, returns map with {:task :files-read :kg-skipped}

   Returns:
     Augmented task string, or map with metadata if :return-metadata is true."
  [task files & [{:keys [project-root project-id use-kg-first return-metadata]
                  :or {use-kg-first true}}]]
  (let [effective-root (or project-root (diff/get-project-root) "")
        effective-project-id (or project-id "hive-mcp")
        context (prepare-context)
        context-str (format-context-str context)
        smart-ctx-str (build-smart-context files task effective-root effective-project-id)

        ;; KG-FIRST: Use knowledge graph to minimize file reads
        {:keys [context files-read kg-skipped summary]}
        (if (and use-kg-first (seq files))
          (kg-ctx/format-files-with-kg-context files {:project-root effective-root})
          ;; Fallback to legacy file reading
          {:context (format-file-contents files project-root)
           :files-read files
           :kg-skipped []
           :summary {:kg-known 0 :needs-read (count files) :stale 0}})

        file-contents-str context

        augmented (str context-str
                       (when smart-ctx-str
                         (str "\n" smart-ctx-str "\n"))
                       ;; CRITICAL: Inject project root so drones use correct directory in propose_diff
                       (when (seq effective-root)
                         (str "## Project Directory\n"
                              "IMPORTANT: When calling propose_diff, you MUST include:\n"
                              "  directory: \"" effective-root "\"\n"
                              "This ensures paths are validated against YOUR project, not the MCP server.\n\n"))
                       "## Task\n" task
                       (when (seq files)
                         (str "\n\n## Files to modify\n"
                              (str/join "\n" (map #(str "- " %) files))))
                       (when file-contents-str
                         (str "\n\n" file-contents-str)))]

    ;; Log KG-first efficiency when files were skipped
    (when (seq kg-skipped)
      (log/info "KG-first augment-task saved file reads"
                {:kg-skipped (count kg-skipped)
                 :files-read (count files-read)
                 :summary summary}))

    (if return-metadata
      {:task augmented
       :files-read files-read
       :kg-skipped kg-skipped
       :summary summary}
      augmented)))
