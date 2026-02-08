(ns hive-mcp.workflow.yaml.vars
  "Variable substitution for YAML workflow engine.

   Handles {{var}} placeholder resolution against execution context.
   Supports:
   - Simple variable lookup: {{name}} -> (get ctx :name)
   - Dotted path resolution: {{step-1.result.message}} -> (get-in ctx [:step-1 :result :message])
   - Deep map substitution: recursively substitutes all string values

   CLARITY-Y: Unresolved variables are left as-is (never throws)."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn resolve-var-path
  "Resolve a dotted variable path against context.
   E.g., 'step-1.result.message' -> (get-in ctx [:step-1 :result :message])"
  [ctx path-str]
  (let [parts (str/split path-str #"\.")
        ks    (mapv keyword parts)]
    (get-in ctx ks)))

(defn substitute-vars
  "Replace all {{var}} placeholders in a string with values from context.
   Unresolved vars are left as-is."
  [s ctx]
  (if (string? s)
    (str/replace s #"\{\{([^}]+)\}\}"
                 (fn [[_match var-name]]
                   (let [v (or (get ctx (keyword var-name))
                               (resolve-var-path ctx var-name))]
                     (if (some? v) (str v) (str "{{" var-name "}}")))))
    s))

(defn substitute-in-map
  "Deep-substitute all string values in a map using context."
  [m ctx]
  (walk/postwalk
   (fn [x]
     (if (string? x)
       (substitute-vars x ctx)
       x))
   m))
