(ns hive-mcp.tools.memory.scope
  "Project scope utilities for memory operations.

   SOLID: SRP - Single responsibility for scope management.
   CLARITY: R - Represented intent with clear scope semantics.

   Go Context Pattern:
   Context flows down explicitly via the 'directory' parameter, never derived
   from ambient state (like Emacs buffer focus). When directory is nil,
   we return 'global' scope rather than querying Emacs - this prevents
   cross-project scope leaks when ambient state doesn't match MCP request origin.

   Handles:
   - Project ID detection from directory path (explicit context)
   - Scope tag injection for memory entries
   - Scope matching for filtering queries
   - Hierarchical scope resolution (SAA strategy)

   Hierarchical Resolution (SAA - Scope Ancestry Algorithm):
   Delegates to hive-mcp.knowledge-graph.scope for hierarchy resolution.
   kg-scope provides the canonical hierarchy walk via:
   - get-parent-scope: single parent lookup (config or string inference)
   - visible-scopes: walk parent chain to global
   - visible-scope-tags: scope tags for Chroma filtering
   - descendant-scope-tags / full-hierarchy-scope-tags: downward traversal

   This namespace wraps kg-scope with memory-specific semantics:
   - resolve-scope-chain: ordered ancestry for a project-id
   - expand-scope-tags: all scope tags a project should see (incl. aliases, R3)
   - make-scope-tag: hierarchical scope tag creation
   - matches-scope?: hierarchical scope matching
   - get-current-project-id: alias-aware project-id resolution (R3)"
  (:require [hive-mcp.knowledge-graph.scope :as kg-scope]
            [clojure.set]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Project ID Detection
;; ============================================================

(defn- last-path-segment
  "Extract last non-blank path segment from a directory path.
   Returns project-name string or nil if path is empty/root."
  [directory]
  (let [parts (str/split directory #"/")
        project-name (last parts)]
    (when (and project-name (not (str/blank? project-name)))
      project-name)))

(defn get-current-project-id
  "Get current project ID from directory path.

   Go Context Pattern: Context flows down explicitly, never derived from
   ambient state. When directory is nil, returns 'global' rather than
   querying Emacs - this prevents cross-project scope leaks.

   Resolution priority:
   1. .hive-project.edn :project-id (via kg-scope/infer-scope-from-path)
   2. Last path segment (backward compat fallback when no .edn found)
   3. 'global' when directory is nil or blank

   Alias resolution (R3):
   After resolving the project-id via steps 1-2, passes it through
   kg-scope/resolve-project-id to handle renamed/aliased projects.
   If the directory name or .edn project-id is a known alias,
   the canonical project-id is returned instead.

   This closes the disconnect between KG scope (reads .edn) and memory
   scope (previously used path segment). Both layers now agree on project-id.

   Callers should pass directory explicitly via:
   - Tool parameter: {:directory \"/path/to/project\"}
   - Context fallback: (or directory (ctx/current-directory))

   Returns:
   - :project-id from .hive-project.edn when found (preferred, alias-resolved)
   - Last path segment when no .hive-project.edn exists (fallback, alias-resolved)
   - 'global' when directory is nil or blank"
  ([]
   (get-current-project-id nil))
  ([directory]
   (if (and directory (not (str/blank? (str/trim directory))))
     ;; Priority 1: Try .hive-project.edn via kg-scope
     (let [edn-project-id (try
                            (kg-scope/infer-scope-from-path directory)
                            (catch Exception e
                              (log/debug "get-current-project-id: infer-scope-from-path failed:"
                                         (.getMessage e))
                              nil))]
       (if (and edn-project-id (not= edn-project-id "global"))
         ;; Found .hive-project.edn with :project-id — also resolve aliases
         (let [resolved (kg-scope/resolve-project-id edn-project-id)]
           (log/trace "get-current-project-id: resolved via .hive-project.edn ->"
                      edn-project-id (when (not= resolved edn-project-id)
                                       (str " (alias -> " resolved ")")))
           resolved)
         ;; Priority 2: Fall back to last path segment, resolve aliases
         (let [segment (last-path-segment directory)]
           (or (when segment (kg-scope/resolve-project-id segment))
               "global"))))
     ;; No directory = global scope (Go context pattern: explicit context only)
     (do
       (log/debug "get-current-project-id: no directory provided, using global scope")
       "global"))))

;; ============================================================
;; Scope Tag Management
;; ============================================================

(defn inject-project-scope
  "Add project scope tag if not already present.
   Returns tags vector with scope:project:<id> or scope:global added."
  [tags project-id]
  (let [has-scope? (some #(str/starts-with? % "scope:") tags)]
    (if has-scope?
      tags
      (if (= project-id "global")
        (conj (vec tags) "scope:global")
        (conj (vec tags) (str "scope:project:" project-id))))))

(defn make-scope-tag
  "Create a scope tag for a project-id.
   Delegates to kg-scope/scope->tag for consistent tag generation.

   Returns 'scope:global' for 'global' or nil project-id,
   otherwise 'scope:project:<project-id>'.

   Supports hierarchical project-ids:
     (make-scope-tag 'global')           => 'scope:global'
     (make-scope-tag 'hive-mcp')         => 'scope:project:hive-mcp'
     (make-scope-tag 'hive-mcp:agora')   => 'scope:project:hive-mcp:agora'"
  [project-id]
  (kg-scope/scope->tag project-id))

;; ============================================================
;; Scope Filtering
;; ============================================================

(defn matches-scope?
  "Check if entry matches the given scope filter.

   Supports hierarchical matching via kg-scope delegation:
   - nil or 'all': match everything
   - 'global': only match scope:global entries
   - scope tag string: match entry against hierarchical scope set
   - scope tag set: match entry against any tag in set (for pre-computed sets)

   Hierarchical behavior (SAA):
   When scope-filter is a specific scope tag (e.g., 'scope:project:hive-mcp:agora'),
   entries match if they have that scope OR any ancestor scope tag.
   This means an entry tagged 'scope:project:hive-mcp' is visible from
   'scope:project:hive-mcp:agora' because hive-mcp is an ancestor.

   For flat backward compatibility, entries tagged 'scope:global' match any
   non-nil, non-'all' filter (global is always an ancestor)."
  [entry scope-filter]
  (let [tags (set (or (:tags entry) []))]
    (cond
      ;; No filter or "all" - match everything
      (or (nil? scope-filter) (= scope-filter "all"))
      true

      ;; "global" - only global scope
      (= scope-filter "global")
      (contains? tags "scope:global")

      ;; Pre-computed set of scope tags (from expand-scope-tags or similar)
      (set? scope-filter)
      (some tags scope-filter)

      ;; Specific scope tag - use hierarchical matching via kg-scope
      ;; Expand the filter into the full ancestor set, then match
      :else
      (let [;; Extract project-id from scope tag for hierarchy resolution
            visible-tags (if (str/starts-with? (str scope-filter) "scope:")
                           ;; It's a scope tag - resolve hierarchy from it
                           (kg-scope/visible-scope-tags scope-filter)
                           ;; It's a bare project-id - resolve and convert to tags
                           (kg-scope/visible-scope-tags scope-filter))]
        (some tags visible-tags)))))

(defn derive-scope-filter
  "Derive scope filter from scope parameter and project-id.

   Returns:
   - nil if scope is 'all' (no filtering)
   - the scope value if explicitly provided
   - 'scope:project:<project-id>' if scope is nil (auto mode)"
  [scope project-id]
  (cond
    (= scope "all") nil
    (some? scope) scope
    :else (make-scope-tag project-id)))

;; ============================================================
;; Hierarchical Scope Support (SAA Strategy)
;; ============================================================
;; SAA = Scope Ancestry Algorithm
;; Delegates hierarchy resolution to kg-scope which supports:
;; 1. Explicit parent-id from .hive-project.edn
;; 2. String-inferred hierarchy (colon-delimited)
;; 3. Global as root ancestor

(defn resolve-scope-chain
  "Walk the parent chain for a project-id via kg-scope.

   Returns an ordered vector of project-ids from self to root (global).
   Uses kg-scope/visible-scopes which resolves via:
   1. Explicit :parent-id in registered .hive-project.edn config
   2. Inferred from colon-delimited scope string
   3. Global scope as root

   Examples:
     (resolve-scope-chain 'hive-mcp:agora')
     => ['hive-mcp:agora' 'hive-mcp' 'global']

     (resolve-scope-chain 'hive-mcp')
     => ['hive-mcp' 'global']

     (resolve-scope-chain 'global')
     => ['global']

     (resolve-scope-chain nil)
     => ['global']"
  [project-id]
  (kg-scope/visible-scopes project-id))

(defn- collect-alias-scope-tags
  "Collect scope tags for aliases of all projects in a scope chain.

   R3: Alias-aware Chroma queries.
   For each canonical project-id in the chain, looks up its registered config
   and adds 'scope:project:<alias>' tags for all declared aliases.
   This ensures Chroma queries also find memories stored under old/aliased
   project names (before a rename).

   Example: if hive-mcp has aliases [\"emacs-mcp\"], and chain is
   [\"hive-mcp\" \"global\"], returns #{\"scope:project:emacs-mcp\"}.

   Returns a set of alias scope tags, or empty set if no aliases found."
  [scope-chain]
  (reduce
   (fn [acc project-id]
     (if (= project-id "global")
       acc
       (let [config (kg-scope/get-project-config project-id)
             aliases (:aliases config)]
         (if (seq aliases)
           (into acc (map #(str "scope:project:" %) aliases))
           acc))))
   #{}
   scope-chain))

(defn expand-scope-tags
  "Expand a project-id into all scope tags it should be able to see.

   Returns a set of scope tags including:
   - Self: scope:project:<project-id>
   - All ancestors: scope:project:<parent>, scope:project:<grandparent>, ...
   - Root: scope:global
   - Alias tags: scope:project:<alias> for all aliases in the chain (R3)

   Delegates to kg-scope/visible-scope-tags for hierarchy resolution,
   then adds alias scope tags so Chroma queries also find memories
   stored under old/aliased project names.

   When include-descendants? is true, also includes child project scope tags
   via kg-scope/full-hierarchy-scope-tags (bidirectional traversal).

   Examples:
     (expand-scope-tags 'hive-mcp:agora')
     => #{'scope:project:hive-mcp:agora' 'scope:project:hive-mcp' 'scope:global'
          'scope:project:emacs-mcp'}  ;; if hive-mcp has alias emacs-mcp

     (expand-scope-tags 'hive-mcp:agora' true)
     => #{'scope:project:hive-mcp:agora' 'scope:project:hive-mcp'
          'scope:global' 'scope:project:emacs-mcp'
          'scope:project:hive-mcp:agora:feature' ...}"
  ([project-id]
   (expand-scope-tags project-id false))
  ([project-id include-descendants?]
   (let [base-tags (if include-descendants?
                     (kg-scope/full-hierarchy-scope-tags project-id)
                     (kg-scope/visible-scope-tags project-id))
         ;; R3: Also include scope tags for aliases in the scope chain
         ;; This ensures Chroma queries find memories stored under old names
         chain (kg-scope/visible-scopes project-id)
         alias-tags (collect-alias-scope-tags chain)]
     (if (seq alias-tags)
       (clojure.set/union base-tags alias-tags)
       base-tags))))

(defn derive-hierarchy-scope-filter
  "Derive hierarchical scope filter that includes ancestors and aliases (R3).

   Returns a set of valid scope tags:
   - nil if scope is 'all' (no filtering)
   - nil if scope is nil (let caller handle auto mode)
   - set including scope + ancestors + aliases + global if hierarchical
   - #{\"scope:global\"} for 'global' scope

   Delegates to kg-scope/visible-scope-tags for hierarchical support,
   then adds alias scope tags so Chroma queries also match memories
   stored under old/aliased project names."
  [scope]
  (cond
    (= scope "all") nil
    (nil? scope) nil  ;; Let caller handle nil -> auto mode
    (= scope "global") #{"scope:global"}
    :else
    ;; Delegate to knowledge-graph.scope for hierarchical resolution
    ;; Then add alias tags for Chroma backward compat (R3)
    (let [base-tags (kg-scope/visible-scope-tags scope)
          chain (kg-scope/visible-scopes scope)
          alias-tags (collect-alias-scope-tags chain)]
      (if (seq alias-tags)
        (clojure.set/union base-tags alias-tags)
        base-tags))))

(defn matches-hierarchy-scopes?
  "Check if entry matches any of the hierarchical scope filters.

   Entry matches if it has any scope tag in the valid-scopes set.
   The valid-scopes set should be pre-computed via expand-scope-tags
   or derive-hierarchy-scope-filter, which already includes ancestors
   and optionally descendants.

   nil valid-scopes means no filtering (match all)."
  [entry valid-scopes]
  (if (nil? valid-scopes)
    true  ;; No filter = match all
    (let [tags (set (or (:tags entry) []))]
      (some tags valid-scopes))))
