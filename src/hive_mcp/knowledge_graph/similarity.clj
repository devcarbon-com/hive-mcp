(ns hive-mcp.knowledge-graph.similarity
  "Structural similarity between KG entries based on relationship patterns.

   Delegates to hive-knowledge (proprietary). Returns noop when not available.

   This is a thin stub that attempts to load the real implementation from
   hive-knowledge.similarity via requiring-resolve. If hive-knowledge is not
   on the classpath, all functions gracefully degrade to empty/noop results."
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Dynamic Resolution Helpers
;; =============================================================================

(defn- try-resolve
  "Attempt to resolve a symbol from hive-knowledge.similarity.
   Returns the var if available, nil otherwise."
  [sym-name]
  (try
    (requiring-resolve (symbol "hive-knowledge.similarity" sym-name))
    (catch Exception _
      nil)))

(defn- delegate-or-noop
  "Try to delegate to hive-knowledge fn, fall back to default value."
  [fn-name default-val args]
  (if-let [f (try-resolve fn-name)]
    (apply f args)
    (do
      (log/debug "hive-knowledge not available, returning noop for" fn-name)
      default-val)))

;; =============================================================================
;; Empty Signature (noop fallback)
;; =============================================================================

(def ^:private empty-signature
  {:outgoing-types {}
   :incoming-types {}
   :outgoing-neighbors #{}
   :incoming-neighbors #{}
   :degree 0})

(def ^:private empty-similarity
  {:score 0.0
   :details {:edge-type 0.0
             :neighbor 0.0
             :degree 0.0}})

;; =============================================================================
;; Public API — delegates to hive-knowledge.similarity or returns noop
;; =============================================================================

(defn relation-signature
  "Extract the relation signature of a node.
   Delegates to hive-knowledge (proprietary). Returns noop when not available."
  [node-id]
  (delegate-or-noop "relation-signature" empty-signature [node-id]))

(defn neighbor-overlap
  "Compute neighbor overlap between two nodes using Jaccard index.
   Delegates to hive-knowledge (proprietary). Returns noop when not available."
  [sig-a sig-b]
  (delegate-or-noop "neighbor-overlap"
                    {:outgoing 0.0 :incoming 0.0 :combined 0.0}
                    [sig-a sig-b]))

(defn edge-type-similarity
  "Compute similarity based on edge type distributions.
   Delegates to hive-knowledge (proprietary). Returns noop when not available."
  [sig-a sig-b]
  (delegate-or-noop "edge-type-similarity"
                    {:outgoing 0.0 :incoming 0.0 :combined 0.0}
                    [sig-a sig-b]))

(defn structural-similarity
  "Compute combined structural similarity between two nodes.
   Delegates to hive-knowledge (proprietary). Returns noop when not available."
  [sig-a sig-b & [opts]]
  (delegate-or-noop "structural-similarity" empty-similarity
                    (if opts [sig-a sig-b opts] [sig-a sig-b])))

(defn build-signature-index
  "Pre-compute relation signatures for all nodes in scope.
   Delegates to hive-knowledge (proprietary). Returns noop when not available."
  [& [opts]]
  (delegate-or-noop "build-signature-index" {} (if opts [opts] [])))

(defn find-structurally-similar
  "Find entries structurally similar to a given entry.
   Delegates to hive-knowledge (proprietary). Returns noop when not available."
  [node-id & [opts]]
  (delegate-or-noop "find-structurally-similar" []
                    (if opts [node-id opts] [node-id])))

(defn pairwise-similarity
  "Compute structural similarity between two specific nodes.
   Delegates to hive-knowledge (proprietary). Returns noop when not available."
  [node-a node-b & [opts]]
  (delegate-or-noop "pairwise-similarity" empty-similarity
                    (if opts [node-a node-b opts] [node-a node-b])))

(defn find-structural-roles
  "Group nodes by their structural role.
   Delegates to hive-knowledge (proprietary). Returns noop when not available."
  [& [opts]]
  (delegate-or-noop "find-structural-roles" [] (if opts [opts] [])))

(defn similarity-report
  "Generate a similarity report for a node.
   Delegates to hive-knowledge (proprietary). Returns noop when not available."
  [node-id & [opts]]
  (delegate-or-noop "similarity-report"
                    {:node-id node-id :signature empty-signature :similar [] :roles []}
                    (if opts [node-id opts] [node-id])))
