(ns hive-mcp.tools.consolidated.analysis
  "Consolidated Code Analysis CLI tool.

   Combines clj-kondo and scc for comprehensive code analysis.

   Subcommands:
   - lint: Check for errors/warnings (kondo)
   - analyze: Project summary (kondo)
   - callers: Find call sites (kondo)
   - calls: Find outgoing calls (kondo)
   - graph: Namespace dependency graph (kondo)
   - scc: Source code count/metrics
   - hotspots: Complexity hotspots
   - compare: Compare metrics between directories

   Usage via MCP: analysis {\"command\": \"lint\", \"path\": \"src/\"}

   SOLID: Facade pattern - single tool entry point for code analysis.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.kondo :as kondo-handlers]
            [hive-mcp.tools.scc :as scc-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {;; clj-kondo commands
   :lint     kondo-handlers/handle-kondo-lint
   :analyze  kondo-handlers/handle-kondo-analyze
   :callers  kondo-handlers/handle-kondo-find-callers
   :calls    kondo-handlers/handle-kondo-find-calls
   :graph    kondo-handlers/handle-kondo-namespace-graph
   ;; scc commands
   :scc      scc-handlers/handle-scc-analyze
   :hotspots scc-handlers/handle-scc-hotspots
   :file     scc-handlers/handle-scc-file
   :compare  scc-handlers/handle-scc-compare})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-analysis
  "Unified CLI handler for code analysis operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated analysis command."
  {:name "analysis"
   :consolidated true
   :description "Code analysis: lint (kondo errors), analyze (kondo summary), callers/calls/graph (kondo), scc (code metrics), hotspots (complexity), file (single file), compare (diff dirs). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["lint" "analyze" "callers" "calls" "graph"
                                                "scc" "hotspots" "file" "compare" "help"]
                                         :description "Analysis operation to perform"}
                              ;; common params
                              "path" {:type "string"
                                      :description "Path to file or directory to analyze"}
                              ;; kondo lint params
                              "level" {:type "string"
                                       :enum ["error" "warning" "info"]
                                       :description "Minimum severity level (lint)"}
                              ;; kondo callers/calls params
                              "ns" {:type "string"
                                    :description "Namespace of the target/source function"}
                              "var_name" {:type "string"
                                          :description "Name of the function"}
                              ;; scc params
                              "threshold" {:type "number"
                                           :description "Minimum complexity for hotspots (default: 20)"}
                              "file_path" {:type "string"
                                           :description "Path to specific file (file command)"}
                              ;; compare params
                              "path_a" {:type "string"
                                        :description "First directory for comparison"}
                              "path_b" {:type "string"
                                        :description "Second directory for comparison"}}
                 :required ["command"]}
   :handler handle-analysis})

(def tools
  "Tool definitions for registration."
  [tool-def])
