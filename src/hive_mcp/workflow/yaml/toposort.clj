(ns hive-mcp.workflow.yaml.toposort
  "Topological sort and dependency resolution for workflow steps.

   Implements Kahn's algorithm for DAG ordering.
   Detects circular dependencies and reports them."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn collect-step-ids
  "Collect all step IDs from steps (including nested parallel branches)."
  [steps]
  (reduce (fn [ids step]
            (let [ids (conj ids (:step-id step))]
              (if (:parallel step)
                (into ids (map :step-id (:parallel step)))
                ids)))
          #{}
          steps))

(defn build-dep-graph
  "Build adjacency map {step-id -> set-of-deps} from steps."
  [steps]
  (reduce (fn [graph step]
            (let [sid  (:step-id step)
                  deps (set (:dependencies step))]
              (assoc graph sid deps)))
          {}
          steps))

(defn topological-sort
  "Kahn's algorithm for topological sort.
   Returns {:sorted [...]} or {:cycle-detected true :nodes ...}."
  [graph all-ids]
  (let [in-degree (reduce (fn [deg id]
                            (reduce (fn [d dep] (update d dep (fnil identity 0)))
                                    (assoc deg id (count (get graph id #{})))
                                    (get graph id #{})))
                          {}
                          all-ids)
        in-degree (reduce (fn [d id] (update d id (fnil identity 0))) in-degree all-ids)]
    (loop [queue   (vec (filter #(zero? (get in-degree %)) all-ids))
           visited #{}
           sorted  []
           deg     in-degree]
      (if (empty? queue)
        (if (= (count sorted) (count all-ids))
          {:sorted sorted}
          {:cycle-detected true
           :nodes (remove (set sorted) all-ids)})
        (let [node  (first queue)
              queue (subvec queue 1)]
          (if (visited node)
            (recur queue visited sorted deg)
            (let [dependents (filter (fn [id]
                                       (contains? (get graph id #{}) node))
                                     all-ids)
                  new-deg    (reduce (fn [d dep-id]
                                       (update d dep-id dec))
                                     deg
                                     dependents)
                  new-ready  (filter #(and (not (visited %))
                                           (not= % node)
                                           (zero? (get new-deg %)))
                                     dependents)]
              (recur (into queue new-ready)
                     (conj visited node)
                     (conj sorted node)
                     new-deg))))))))

(defn validate-dependencies
  "Validate step dependencies and return validation result.
   Returns {:valid? bool :errors [...] :warnings [...] :dependency-order [...]}"
  [workflow]
  (let [steps      (:steps workflow)
        step-ids   (collect-step-ids steps)
        flat-steps (reduce (fn [acc step]
                             (if (:parallel step)
                               (into acc (:parallel step))
                               (conj acc step)))
                           []
                           steps)
        dep-graph  (build-dep-graph flat-steps)
        bad-deps   (reduce (fn [errs step]
                             (let [missing (remove step-ids (:dependencies step))]
                               (if (seq missing)
                                 (conj errs (str "Step '" (:step-id step)
                                                 "' references unknown deps: "
                                                 (str/join ", " missing)))
                                 errs)))
                           []
                           flat-steps)
        topo       (when (empty? bad-deps)
                     (topological-sort dep-graph step-ids))
        cycle?     (:cycle-detected topo)
        errors     (cond-> bad-deps
                     cycle? (conj (str "Dependency cycle detected involving: "
                                       (str/join ", " (:nodes topo)))))
        warnings   (cond-> []
                     (empty? steps)
                     (conj "Workflow has no steps")

                     (not (:loaded? workflow))
                     (conj "Workflow was not successfully loaded"))]
    {:valid?           (and (empty? errors) (:loaded? workflow))
     :errors           (vec errors)
     :warnings         (vec warnings)
     :dependency-order (or (:sorted topo) [])}))
