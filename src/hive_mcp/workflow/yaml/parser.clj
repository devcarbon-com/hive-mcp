(ns hive-mcp.workflow.yaml.parser
  "YAML parsing and normalization for workflow definitions.

   Parses raw YAML strings into canonical workflow structure with:
   - Step normalization (id, title, action, args, dependencies, parallel branches)
   - Workflow-level normalization (name, version, description, params, steps)

   CLARITY-Y: Never throws - returns structured error maps on parse failure."
  (:require [clj-yaml.core :as yaml]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn parse-yaml
  "Parse a YAML string into a workflow definition map.
   Returns {:parsed? true :data ...} or {:parsed? false :errors [...]}."
  [yaml-str]
  (try
    (let [data (yaml/parse-string yaml-str)]
      (if (map? data)
        {:parsed? true :data data}
        {:parsed? false :errors ["YAML did not parse to a map"]}))
    (catch Exception e
      {:parsed? false :errors [(str "YAML parse error: " (.getMessage e))]})))

(defn normalize-step
  "Normalize a raw YAML step map to canonical form."
  [raw-step]
  (let [step (into {} (map (fn [[k v]] [(keyword k) v]) raw-step))]
    (cond-> {:step-id      (or (:id step) (str (gensym "step-")))
             :title        (or (:title step) "Untitled step")
             :description  (:description step)
             :action       (keyword (or (:action step) :noop))
             :args         (or (:args step) {})
             :dependencies (vec (or (:depends-on step) []))}
      (:when step)     (assoc :when-expr (:when step))
      (:parallel step) (assoc :parallel
                              (mapv normalize-step (:parallel step))))))

(defn normalize-workflow
  "Normalize parsed YAML data into canonical workflow structure."
  [data]
  {:name        (or (:name data) "unnamed")
   :version     (or (:version data) "0.0.1")
   :description (or (:description data) "")
   :params      (or (:params data) {})
   :steps       (mapv normalize-step (or (:steps data) []))})
