(ns hive-mcp.workflow.yaml-engine
  "YAML-based workflow engine implementing IWorkflowEngine.

   FACADE: This namespace re-exports the public API from submodules under
   hive-mcp.workflow.yaml.* for backward compatibility.

   Submodules:
   - yaml.vars      - Variable substitution ({{var}} syntax)
   - yaml.parser    - YAML parsing and normalization
   - yaml.toposort  - Dependency resolution (topological sort)
   - yaml.actions   - Built-in actions (echo, noop, transform, shell, mcp-call)
   - yaml.executor  - Execution state management and step/workflow execution
   - yaml.engine    - YAMLWorkflowEngine record implementing IWorkflowEngine

   SOLID-O: Open for extension via :action-registry.
   CLARITY-Y: Never throws - all errors returned in result maps.
   CLARITY-I: Full introspection via get-status."
  (:require [hive-mcp.workflow.yaml.vars :as vars]
            [hive-mcp.workflow.yaml.parser :as parser]
            [hive-mcp.workflow.yaml.toposort :as toposort]
            [hive-mcp.workflow.yaml.actions :as actions]
            [hive-mcp.workflow.yaml.executor :as executor]
            [hive-mcp.workflow.yaml.engine :as engine]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Re-exported API (backward compatibility)
;;; =============================================================================

;; --- Variable Substitution ---
;; Private in the original, but tests access via #'engine/substitute-vars
(defn- substitute-vars
  "Replace all {{var}} placeholders in a string with values from context.
   Delegates to hive-mcp.workflow.yaml.vars/substitute-vars."
  [s ctx]
  (vars/substitute-vars s ctx))

;; --- YAML Parsing ---
(def parse-yaml
  "Parse a YAML string into a workflow definition map.
   Delegates to hive-mcp.workflow.yaml.parser/parse-yaml."
  parser/parse-yaml)

;; --- Actions ---
;; Re-export the multimethod so execute-action dispatches work from this ns
(def execute-action
  "Execute a workflow step action. Dispatches on :action keyword.
   Delegates to hive-mcp.workflow.yaml.actions/execute-action."
  actions/execute-action)

;; --- Engine ---
(def create-yaml-engine
  "Create a new YAMLWorkflowEngine.
   Delegates to hive-mcp.workflow.yaml.engine/create-yaml-engine."
  engine/create-yaml-engine)

;; --- Execution State ---
(def clear-executions!
  "Clear all execution state. Primarily for testing.
   Delegates to hive-mcp.workflow.yaml.executor/clear-executions!."
  executor/clear-executions!)
