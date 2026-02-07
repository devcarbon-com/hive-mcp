(ns hive-mcp.agent.ling.strategy
  "ILingStrategy protocol — mode-specific spawn/dispatch/status/kill operations.

   Implementations:
   - VtermStrategy  (ling/vterm.clj)   — Emacs vterm buffer
   - HeadlessStrategy (ling/headless_strategy.clj) — ProcessBuilder subprocess

   The Ling record (agent/ling.clj) delegates mode-specific calls to its strategy.
   Mode-independent operations (claims, agent-type, etc.) stay on the Ling record.

   SOLID: Open-Closed — new spawn modes add a new strategy, don't modify Ling.
   CLARITY: C — Composition over case dispatch.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol ILingStrategy
  "Strategy protocol for mode-specific ling operations.

   Each method receives the ling context map and operation-specific opts.
   Returns mode-specific results that the Ling facade normalizes."

  (strategy-spawn! [this ling-ctx opts]
    "Spawn a ling using this strategy's mechanism.

     Arguments:
       ling-ctx - Map with :id, :cwd, :presets, :project-id, :model
       opts     - Spawn options: :task, :depth, :parent, :kanban-task-id, :presets

     Returns:
       The effective slave-id (may differ from requested id in vterm mode)")

  (strategy-dispatch! [this ling-ctx task-opts]
    "Dispatch a task to a running ling.

     Arguments:
       ling-ctx  - Map with :id, :cwd, :spawn-mode, :model
       task-opts - Map with :task, :timeout-ms

     Returns:
       true on success

     Throws:
       ExceptionInfo on failure")

  (strategy-status [this ling-ctx ds-status]
    "Get mode-specific liveness/status information.

     Arguments:
       ling-ctx  - Map with :id
       ds-status - DataScript slave record (may be nil)

     Returns:
       Enriched status map with mode-specific fields")

  (strategy-kill! [this ling-ctx]
    "Terminate the ling using this strategy's mechanism.

     Arguments:
       ling-ctx - Map with :id

     Returns:
       {:killed? bool :id string ...}"))
