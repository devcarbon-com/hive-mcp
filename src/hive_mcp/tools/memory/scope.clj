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
   - expand-scope-tags: all scope tags a project should see
   - make-scope-tag: hierarchical scope tag creation
   - matches-scope?: hierarchical scope matching"
  (:require [hive-mcp.knowledge-graph.scope :as kg-scope]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Project ID Detection
;; ============================================================

(defn get-current-project-id
  "Get current project ID from directory path.

   Go Context Pattern: Context flows down explicitly, never derived from
   ambient state. When directory is nil, returns 'global' rather than
   querying Emacs - this prevents cross-project scope leaks.

   Callers should pass directory explicitly via:
   - Tool parameter: {:directory \"/path/to/project\"}
   - Context fallback: (or directory (ctx/current-directory))

   Returns:
   - Project name (last path segment) when directory is provided
   - 'global' when directory is nil or blank"
  ([]
   (get-current-project-id nil))
  ([directory]
   (if directory
     ;; Derive from directory path directly (no emacsclient roundtrip)
     (let [parts (str/split directory #"/")
           project-name (last parts)]
       (if (and project-name (not (str/blank? project-name)))
         project-name
         "global"))
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

(defn expand-scope-tags
  "Expand a project-id into all scope tags it should be able to see.

   Returns a set of scope tags including:
   - Self: scope:project:<project-id>
   - All ancestors: scope:project:<parent>, scope:project:<grandparent>, ...
   - Root: scope:global

   Delegates to kg-scope/visible-scope-tags for the actual resolution.

   When include-descendants? is true, also includes child project scope tags
   via kg-scope/full-hierarchy-scope-tags (bidirectional traversal).

   Examples:
     (expand-scope-tags 'hive-mcp:agora')
     => #{'scope:project:hive-mcp:agora' 'scope:project:hive-mcp' 'scope:global'}

     (expand-scope-tags 'hive-mcp:agora' true)
     => #{'scope:project:hive-mcp:agora' 'scope:project:hive-mcp'
          'scope:global' 'scope:project:hive-mcp:agora:feature' ...}"
  ([project-id]
   (expand-scope-tags project-id false))
  ([project-id include-descendants?]
   (if include-descendants?
     (kg-scope/full-hierarchy-scope-tags project-id)
     (kg-scope/visible-scope-tags project-id))))

(defn derive-hierarchy-scope-filter
  "Derive hierarchical scope filter that includes ancestors.

   Returns a set of valid scope tags:
   - nil if scope is 'all' (no filtering)
   - nil if scope is nil (let caller handle auto mode)
   - set including scope + ancestors + global if hierarchical
   - #{\"scope:global\"} for 'global' scope

   Delegates to kg-scope/visible-scope-tags for full hierarchical support
   including explicit parent-id from .hive-project.edn."
  [scope]
  (cond
    (= scope "all") nil
    (nil? scope) nil  ;; Let caller handle nil -> auto mode
    (= scope "global") #{"scope:global"}
    :else
    ;; Delegate to knowledge-graph.scope for hierarchical resolution
    ;; This supports both explicit parent-id and string-based inference
    (kg-scope/visible-scope-tags scope)))

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
