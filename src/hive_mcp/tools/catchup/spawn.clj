(ns hive-mcp.tools.catchup.spawn
  "Spawn context injection for ling priming.

   SOLID: SRP - Single responsibility for spawn-time context generation.
   Extracted from hive-mcp.tools.catchup (Sprint 2 refactoring).

   Dual-mode (feature-flagged):
   - :full (default, backward compatible) — full text injection (~12K chars)
   - :hints — structured pointers only (~500 tokens), ling self-hydrates

   Architecture > LLM behavior: inject context at spawn time rather than
   relying on lings to /catchup themselves."
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.catchup.scope :as catchup-scope]
            [hive-mcp.tools.catchup.git :as catchup-git]
            [hive-mcp.tools.catchup.format :as fmt]
            [hive-mcp.agent.hints :as hints]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn spawn-context
  "Generate a compact context payload for ling spawn injection.

   Architecture > LLM behavior: inject context at spawn time rather than
   relying on lings to /catchup themselves.

   Dual-mode (feature-flagged):
   - :full (default, backward compatible) — full text injection (~12K chars)
   - :hints — structured pointers only (~500 tokens), ling self-hydrates

   Arguments:
     directory - Working directory for project scoping
     opts      - Optional map:
                 :mode - :full (default) or :hints
                 :task - Task description (hints mode: generates semantic queries)

   Returns a markdown string, or nil on error/no-config.

   CLARITY-C: Composes from existing catchup query helpers.
   CLARITY-I: Validates payload size (< 3K tokens / ~12K chars).
   HCR Wave 4: Uses full hierarchy scope, inherits project context from parent.

   See decision: 20260205213357-62f62a93 (Memory Hints Replace Text Injection)"
  ([directory] (spawn-context directory {}))
  ([directory {:keys [mode task task-id] :or {mode :full}}]
   (when (chroma/embedding-configured?)
     (try
       (let [project-id (scope/get-current-project-id directory)
             project-name (catchup-scope/get-current-project-name directory)]

         (if (= mode :hints)
           ;; === HINTS MODE: Compact structured pointers (~500 tokens) ===
           (let [hint-data (hints/generate-hints project-id {:task task})
                 ;; When task-id provided, enrich hints with KG-driven task hints
                 task-hints (when task-id
                              (try
                                (hints/generate-task-hints {:task-id task-id :depth 2})
                                (catch Exception e
                                  (log/debug "Task hint generation failed (non-fatal):" (.getMessage e))
                                  nil)))
                 ;; Merge task-specific hints into general hints
                 enriched-hints (if task-hints
                                  (update hint-data :memory-hints
                                          (fn [mh]
                                            (cond-> mh
                                              (seq (:l1-ids task-hints))
                                              (update :read-ids (fnil into []) (:l1-ids task-hints))
                                              (seq (:l2-queries task-hints))
                                              (update :queries (fnil into []) (:l2-queries task-hints))
                                              (seq (:l3-seeds task-hints))
                                              (assoc :kg-seeds (mapv :id (:l3-seeds task-hints))
                                                     :kg-depth (get (first (:l3-seeds task-hints)) :depth 2)))))
                                  hint-data)
                 git-info (catchup-git/gather-git-info directory)]
             (hints/serialize-hints enriched-hints
                                    :project-name (or project-name project-id "global")
                                    :git-info git-info))

           ;; === FULL MODE: Full text injection (default, backward compatible) ===
           (let [;; HCR Wave 4: query-scoped-entries now uses full hierarchy scope
                 ;; so spawned lings get context from parent + child projects
                 axioms (catchup-scope/query-axioms project-id)
                 priority-conventions (catchup-scope/query-scoped-entries "convention" ["catchup-priority"]
                                                                          project-id 50)
                 decisions (catchup-scope/query-scoped-entries "decision" nil project-id 50)
                 git-info (catchup-git/gather-git-info directory)

                 ;; L1 Disc: Surface top-N most stale files needing re-grounding
                 stale-files (try
                               (kg-disc/top-stale-files :n 5 :project-id project-id)
                               (catch Exception e
                                 (log/debug "spawn-context stale-files query failed:" (.getMessage e))
                                 []))

                 ;; Transform to metadata
                 axioms-meta (mapv fmt/entry->axiom-meta axioms)
                 priority-meta (mapv fmt/entry->priority-meta priority-conventions)
                 decisions-meta (mapv #(fmt/entry->catchup-meta % 80) decisions)

                 ;; Serialize to markdown
                 context-str (fmt/serialize-spawn-context
                              {:axioms axioms-meta
                               :priority-conventions priority-meta
                               :decisions decisions-meta
                               :stale-files stale-files
                               :git-info git-info
                               :project-name (or project-name project-id "global")})]

             ;; CLARITY-I: Validate payload size
             (if (> (count context-str) fmt/max-spawn-context-chars)
               (do
                 (log/warn "spawn-context exceeds token budget:"
                           (count context-str) "chars, truncating decisions + stale-files")
                 ;; Truncate: keep axioms + priority conventions, drop decisions + stale files
                 (fmt/serialize-spawn-context
                  {:axioms axioms-meta
                   :priority-conventions priority-meta
                   :decisions []
                   :stale-files []
                   :git-info git-info
                   :project-name (or project-name project-id "global")}))
               context-str))))
       (catch Exception e
         (log/warn "spawn-context failed (non-fatal):" (.getMessage e))
         nil)))))
