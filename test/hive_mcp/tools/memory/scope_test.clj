(ns hive-mcp.tools.memory.scope-test
  "Unit tests for memory scope utilities.

   Tests the Go Context Pattern fix:
   - Directory nil -> 'global' (no Emacs fallback)
   - Directory provided -> project-id from path

   SAA (Scope Ancestry Algorithm) tests:
   - resolve-scope-chain: hierarchy walk via kg-scope
   - expand-scope-tags: ancestor + optional descendant tag expansion
   - make-scope-tag: hierarchical tag creation via kg-scope delegation
   - matches-scope?: hierarchical matching (SAA-aware)

   This prevents cross-project scope leaks when Emacs buffer focus
   doesn't match MCP request origin."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.knowledge-graph.scope :as kg-scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-state-fixture
  "Clear caches and registries before each test."
  [f]
  (kg-scope/clear-config-cache!)
  (f))

(use-fixtures :each reset-state-fixture)

;; =============================================================================
;; Go Context Pattern: Explicit directory -> project-id
;; =============================================================================

(deftest test-get-current-project-id-nil-returns-global
  (testing "nil directory returns 'global' (Go Context Pattern)"
    (is (= "global" (scope/get-current-project-id nil)))
    (is (= "global" (scope/get-current-project-id)))))

(deftest test-get-current-project-id-from-directory
  (testing "Extracts project-id from directory path"
    (is (= "hive-mcp" (scope/get-current-project-id "/home/user/projects/hive-mcp")))
    (is (= "my-project" (scope/get-current-project-id "/var/projects/my-project")))))

(deftest test-get-current-project-id-trailing-slash
  (testing "Handles trailing slash correctly"
    ;; Trailing slash is stripped by str/split, so we get the project name
    (is (= "projects" (scope/get-current-project-id "/home/user/projects/")))))

(deftest test-get-current-project-id-blank-directory
  (testing "Blank directory string returns 'global'"
    (is (= "global" (scope/get-current-project-id "")))
    (is (= "global" (scope/get-current-project-id "   ")))))

(deftest test-get-current-project-id-root-path
  (testing "Root path returns 'global'"
    (is (= "global" (scope/get-current-project-id "/")))))

;; =============================================================================
;; Scope Tag Injection
;; =============================================================================

(deftest test-inject-project-scope-adds-tag
  (testing "Injects scope tag when none present"
    (is (= ["tag1" "scope:project:myproj"]
           (scope/inject-project-scope ["tag1"] "myproj")))))

(deftest test-inject-project-scope-preserves-existing
  (testing "Preserves existing scope tag"
    (is (= ["scope:global" "tag1"]
           (scope/inject-project-scope ["scope:global" "tag1"] "myproj")))))

(deftest test-inject-project-scope-global-project
  (testing "Injects scope:global for global project-id"
    (is (= ["tag1" "scope:global"]
           (scope/inject-project-scope ["tag1"] "global")))))

;; =============================================================================
;; make-scope-tag (now delegates to kg-scope/scope->tag)
;; =============================================================================

(deftest test-make-scope-tag-global
  (testing "Global project-id produces scope:global"
    (is (= "scope:global" (scope/make-scope-tag "global")))))

(deftest test-make-scope-tag-nil
  (testing "nil project-id produces scope:global"
    (is (= "scope:global" (scope/make-scope-tag nil)))))

(deftest test-make-scope-tag-simple-project
  (testing "Simple project-id produces scope:project:<id>"
    (is (= "scope:project:hive-mcp" (scope/make-scope-tag "hive-mcp")))))

(deftest test-make-scope-tag-hierarchical-project
  (testing "Hierarchical project-id produces correct scope tag"
    (is (= "scope:project:hive-mcp:agora" (scope/make-scope-tag "hive-mcp:agora")))
    (is (= "scope:project:org:team:project" (scope/make-scope-tag "org:team:project")))))

(deftest test-make-scope-tag-already-tagged
  (testing "scope:project: prefix input is normalized"
    ;; kg-scope/scope->tag normalizes via normalize-scope first
    (is (= "scope:project:hive-mcp" (scope/make-scope-tag "scope:project:hive-mcp")))))

;; =============================================================================
;; matches-scope? (hierarchical SAA-aware matching)
;; =============================================================================

(deftest test-matches-scope-nil-filter
  (testing "nil filter matches everything"
    (is (scope/matches-scope? {:tags ["scope:project:x"]} nil))
    (is (scope/matches-scope? {:tags ["scope:global"]} nil))
    (is (scope/matches-scope? {:tags []} nil))))

(deftest test-matches-scope-all-filter
  (testing "'all' filter matches everything"
    (is (scope/matches-scope? {:tags ["scope:project:x"]} "all"))
    (is (scope/matches-scope? {:tags ["scope:global"]} "all"))))

(deftest test-matches-scope-global-filter
  (testing "'global' filter matches only scope:global entries"
    (is (scope/matches-scope? {:tags ["scope:global"]} "global"))
    (is (not (scope/matches-scope? {:tags ["scope:project:x"]} "global")))))

(deftest test-matches-scope-specific-filter-exact-match
  (testing "Specific scope matches that exact scope tag"
    (is (scope/matches-scope? {:tags ["scope:project:x"]} "scope:project:x"))))

(deftest test-matches-scope-specific-filter-global-entry
  (testing "Specific scope matches global entries (global is ancestor of all)"
    (is (scope/matches-scope? {:tags ["scope:global"]} "scope:project:x"))))

(deftest test-matches-scope-specific-filter-no-match
  (testing "Specific scope does not match unrelated project"
    (is (not (scope/matches-scope? {:tags ["scope:project:y"]} "scope:project:x")))))

(deftest test-matches-scope-hierarchical-ancestor-match
  (testing "SAA: Child scope filter matches ancestor entries"
    ;; Query from hive-mcp:agora should match entry tagged hive-mcp (ancestor)
    (is (scope/matches-scope?
         {:tags ["scope:project:hive-mcp"]}
         "scope:project:hive-mcp:agora"))
    ;; Query from deep hierarchy should match all ancestors
    (is (scope/matches-scope?
         {:tags ["scope:project:hive-mcp"]}
         "scope:project:hive-mcp:agora:feature"))
    (is (scope/matches-scope?
         {:tags ["scope:project:hive-mcp:agora"]}
         "scope:project:hive-mcp:agora:feature"))))

(deftest test-matches-scope-hierarchical-sibling-no-match
  (testing "SAA: Sibling scope does NOT match"
    ;; hive-mcp:agora should NOT see hive-mcp:tools entries
    (is (not (scope/matches-scope?
              {:tags ["scope:project:hive-mcp:tools"]}
              "scope:project:hive-mcp:agora")))))

(deftest test-matches-scope-hierarchical-child-not-visible-from-parent
  (testing "SAA: Parent scope filter does NOT match child entries (upward only)"
    ;; hive-mcp should NOT see hive-mcp:agora entries
    (is (not (scope/matches-scope?
              {:tags ["scope:project:hive-mcp:agora"]}
              "scope:project:hive-mcp")))))

(deftest test-matches-scope-set-filter
  (testing "Pre-computed set filter matches any member"
    (let [filter-set #{"scope:project:hive-mcp" "scope:global"}]
      (is (scope/matches-scope? {:tags ["scope:project:hive-mcp"]} filter-set))
      (is (scope/matches-scope? {:tags ["scope:global"]} filter-set))
      (is (not (scope/matches-scope? {:tags ["scope:project:other"]} filter-set))))))

(deftest test-matches-scope-bare-project-id-filter
  (testing "Bare project-id (not scope: prefixed) also resolves hierarchically"
    ;; bare "hive-mcp:agora" should match ancestors via kg-scope
    (is (scope/matches-scope?
         {:tags ["scope:project:hive-mcp"]}
         "hive-mcp:agora"))
    (is (scope/matches-scope?
         {:tags ["scope:global"]}
         "hive-mcp:agora"))))

;; =============================================================================
;; resolve-scope-chain (SAA hierarchy walk)
;; =============================================================================

(deftest test-resolve-scope-chain-nil
  (testing "nil resolves to ['global']"
    (is (= ["global"] (scope/resolve-scope-chain nil)))))

(deftest test-resolve-scope-chain-global
  (testing "global resolves to ['global']"
    (is (= ["global"] (scope/resolve-scope-chain "global")))))

(deftest test-resolve-scope-chain-single-level
  (testing "Single-level project resolves to self + global"
    (is (= ["hive-mcp" "global"] (scope/resolve-scope-chain "hive-mcp")))))

(deftest test-resolve-scope-chain-two-level
  (testing "Two-level project resolves to self + parent + global"
    (is (= ["hive-mcp:agora" "hive-mcp" "global"]
           (scope/resolve-scope-chain "hive-mcp:agora")))))

(deftest test-resolve-scope-chain-three-level
  (testing "Three-level project resolves full hierarchy"
    (is (= ["hive-mcp:agora:feature" "hive-mcp:agora" "hive-mcp" "global"]
           (scope/resolve-scope-chain "hive-mcp:agora:feature")))))

(deftest test-resolve-scope-chain-with-explicit-parent
  (testing "Explicit parent-id from config is used"
    (kg-scope/register-project-config! "my-sub" {:parent-id "custom-parent"})
    (kg-scope/register-project-config! "custom-parent" {})
    (is (= ["my-sub" "custom-parent" "global"]
           (scope/resolve-scope-chain "my-sub")))))

(deftest test-resolve-scope-chain-deep
  (testing "Deeply nested scope resolves full chain"
    (is (= ["org:team:project:sub:feature"
            "org:team:project:sub"
            "org:team:project"
            "org:team"
            "org"
            "global"]
           (scope/resolve-scope-chain "org:team:project:sub:feature")))))

;; =============================================================================
;; expand-scope-tags (tag expansion for filtering)
;; =============================================================================

(deftest test-expand-scope-tags-nil
  (testing "nil expands to just scope:global"
    (is (= #{"scope:global"} (scope/expand-scope-tags nil)))))

(deftest test-expand-scope-tags-global
  (testing "global expands to just scope:global"
    (is (= #{"scope:global"} (scope/expand-scope-tags "global")))))

(deftest test-expand-scope-tags-single-level
  (testing "Single-level project expands to self + global"
    (is (= #{"scope:project:hive-mcp" "scope:global"}
           (scope/expand-scope-tags "hive-mcp")))))

(deftest test-expand-scope-tags-two-level
  (testing "Two-level project expands to self + parent + global"
    (is (= #{"scope:project:hive-mcp:agora"
             "scope:project:hive-mcp"
             "scope:global"}
           (scope/expand-scope-tags "hive-mcp:agora")))))

(deftest test-expand-scope-tags-three-level
  (testing "Three-level project expands full hierarchy tags"
    (is (= #{"scope:project:hive-mcp:agora:feature"
             "scope:project:hive-mcp:agora"
             "scope:project:hive-mcp"
             "scope:global"}
           (scope/expand-scope-tags "hive-mcp:agora:feature")))))

(deftest test-expand-scope-tags-upward-only-default
  (testing "Default (no descendants): only ancestors + self + global"
    (let [tags (scope/expand-scope-tags "hive-mcp")]
      ;; Self
      (is (contains? tags "scope:project:hive-mcp"))
      ;; Global
      (is (contains? tags "scope:global"))
      ;; Should NOT contain child tags (no descendant traversal)
      ;; (Children are not in the default upward-only set)
      )))

(deftest test-expand-scope-tags-with-explicit-parent
  (testing "Explicit parent-id from config is used in expansion"
    (kg-scope/register-project-config! "leaf-proj" {:parent-id "mid-proj"})
    (kg-scope/register-project-config! "mid-proj" {:parent-id "root-proj"})
    (kg-scope/register-project-config! "root-proj" {})
    (is (= #{"scope:project:leaf-proj"
             "scope:project:mid-proj"
             "scope:project:root-proj"
             "scope:global"}
           (scope/expand-scope-tags "leaf-proj")))))

;; =============================================================================
;; derive-hierarchy-scope-filter (backward compat)
;; =============================================================================

(deftest test-derive-hierarchy-scope-filter-all
  (testing "scope 'all' returns nil (no filtering)"
    (is (nil? (scope/derive-hierarchy-scope-filter "all")))))

(deftest test-derive-hierarchy-scope-filter-nil
  (testing "scope nil returns nil (caller handles auto mode)"
    (is (nil? (scope/derive-hierarchy-scope-filter nil)))))

(deftest test-derive-hierarchy-scope-filter-global
  (testing "scope 'global' returns #{scope:global}"
    (is (= #{"scope:global"} (scope/derive-hierarchy-scope-filter "global")))))

(deftest test-derive-hierarchy-scope-filter-specific
  (testing "specific scope returns visible scope tags set"
    (is (= #{"scope:project:hive-mcp:agora"
             "scope:project:hive-mcp"
             "scope:global"}
           (scope/derive-hierarchy-scope-filter "hive-mcp:agora")))))

;; =============================================================================
;; matches-hierarchy-scopes? (pre-computed set matching)
;; =============================================================================

(deftest test-matches-hierarchy-scopes-nil-matches-all
  (testing "nil valid-scopes matches everything"
    (is (scope/matches-hierarchy-scopes? {:tags ["anything"]} nil))
    (is (scope/matches-hierarchy-scopes? {:tags []} nil))))

(deftest test-matches-hierarchy-scopes-exact-match
  (testing "Entry tag in valid-scopes matches"
    (let [valid #{"scope:project:hive-mcp" "scope:global"}]
      (is (scope/matches-hierarchy-scopes? {:tags ["scope:project:hive-mcp"]} valid))
      (is (scope/matches-hierarchy-scopes? {:tags ["scope:global"]} valid)))))

(deftest test-matches-hierarchy-scopes-no-match
  (testing "Entry tag NOT in valid-scopes does not match"
    (let [valid #{"scope:project:hive-mcp" "scope:global"}]
      (is (not (scope/matches-hierarchy-scopes?
                {:tags ["scope:project:other"]} valid))))))

(deftest test-matches-hierarchy-scopes-empty-tags
  (testing "Entry with no tags does not match non-nil valid-scopes"
    (is (not (scope/matches-hierarchy-scopes?
              {:tags []} #{"scope:global"})))))

;; =============================================================================
;; Cross-Project Leak Prevention (regression test)
;; =============================================================================

(deftest test-no-emacs-fallback-on-nil-directory
  (testing "No Emacs fallback: nil directory -> global, not Emacs buffer context"
    ;; This test documents the fix for the cross-project scope leak.
    ;; When directory is nil, we return 'global' rather than querying Emacs.
    ;; This prevents Project B memories from leaking into Project A queries
    ;; when Emacs happens to have a Project B buffer focused.
    (let [result (scope/get-current-project-id nil)]
      (is (= "global" result)
          "Should return 'global' without calling Emacs"))))

;; =============================================================================
;; Integration: SAA with crud.clj apply-auto-scope-filter
;; =============================================================================
;; These tests verify the scope functions work correctly with the patterns
;; used in crud.clj's apply-auto-scope-filter (HCR Wave 4 integration)

(deftest test-saa-integration-expand-then-match
  (testing "expand-scope-tags output can be used directly with matches-hierarchy-scopes?"
    (let [scope-tags (scope/expand-scope-tags "hive-mcp:agora")
          parent-entry {:tags ["scope:project:hive-mcp"]}
          self-entry {:tags ["scope:project:hive-mcp:agora"]}
          global-entry {:tags ["scope:global"]}
          sibling-entry {:tags ["scope:project:hive-mcp:tools"]}
          unrelated-entry {:tags ["scope:project:other"]}]
      ;; Parent visible (upward visibility)
      (is (scope/matches-hierarchy-scopes? parent-entry scope-tags))
      ;; Self visible
      (is (scope/matches-hierarchy-scopes? self-entry scope-tags))
      ;; Global visible
      (is (scope/matches-hierarchy-scopes? global-entry scope-tags))
      ;; Sibling NOT visible
      (is (not (scope/matches-hierarchy-scopes? sibling-entry scope-tags)))
      ;; Unrelated NOT visible
      (is (not (scope/matches-hierarchy-scopes? unrelated-entry scope-tags))))))

(deftest test-saa-integration-scope-chain-to-tags
  (testing "resolve-scope-chain and expand-scope-tags are consistent"
    ;; Every scope in the chain should have a tag in expanded tags
    (let [chain (scope/resolve-scope-chain "hive-mcp:agora:feature")
          tags (scope/expand-scope-tags "hive-mcp:agora:feature")]
      (doseq [scope-id chain]
        (is (contains? tags (scope/make-scope-tag scope-id))
            (str "Tag for " scope-id " should be in expanded tags"))))))

(deftest test-saa-integration-matches-scope-consistent-with-hierarchy
  (testing "matches-scope? with scope tag is consistent with expand/match pattern"
    ;; Both approaches should give same results
    (let [test-entries [{:tags ["scope:project:hive-mcp"]}
                        {:tags ["scope:project:hive-mcp:agora"]}
                        {:tags ["scope:global"]}
                        {:tags ["scope:project:hive-mcp:tools"]}
                        {:tags ["scope:project:other"]}]
          filter-scope "scope:project:hive-mcp:agora"
          ;; Approach 1: matches-scope? directly
          direct-results (filter #(scope/matches-scope? % filter-scope) test-entries)
          ;; Approach 2: expand + matches-hierarchy-scopes?
          expanded (scope/expand-scope-tags "hive-mcp:agora")
          hierarchy-results (filter #(scope/matches-hierarchy-scopes? % expanded) test-entries)]
      ;; Both should return the same entries
      (is (= (set (map :tags direct-results))
             (set (map :tags hierarchy-results)))))))
