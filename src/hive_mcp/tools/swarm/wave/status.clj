(ns hive-mcp.tools.swarm.wave.status
  "Wave status queries and formatting.

   Provides read-only query functions for wave state.
   Delegates to DataScript for data access.

   SOLID-S: Single responsibility - status queries only.
   CLARITY-L: Thin adapter layer for status access."
  (:require [hive-mcp.swarm.datascript :as ds]
            [clojure.string]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Wave Status Queries
;;; ============================================================

(defn get-wave-status
  "Get current status of a wave execution.

   Arguments:
     wave-id - Wave identifier

   Returns:
     Map with :wave-id :plan-id :status :active-count :completed-count :failed-count
     or nil if wave not found."
  [wave-id]
  (when-let [wave (ds/get-wave wave-id)]
    {:wave-id wave-id
     :plan-id (:wave/plan wave)
     :status (:wave/status wave)
     :active-count (:wave/active-count wave)
     :completed-count (:wave/completed-count wave)
     :failed-count (:wave/failed-count wave)
     :started-at (:wave/started-at wave)
     :completed-at (:wave/completed-at wave)}))

(defn wave-in-progress?
  "Check if a wave is still executing."
  [wave-id]
  (when-let [status (get-wave-status wave-id)]
    (#{:pending :in-progress :running} (:status status))))

(defn wave-completed?
  "Check if a wave has completed (success or failure)."
  [wave-id]
  (when-let [status (get-wave-status wave-id)]
    (#{:completed :partial-failure :failed :cancelled} (:status status))))

;;; ============================================================
;;; Plan Status Queries
;;; ============================================================

(defn get-plan-status
  "Get current status of a change plan.

   Arguments:
     plan-id - Plan identifier

   Returns:
     Map with plan info and all items."
  [plan-id]
  (when-let [plan (ds/get-plan plan-id)]
    (let [items (ds/get-plan-items plan-id)]
      {:plan-id plan-id
       :status (:change-plan/status plan)
       :preset (:change-plan/preset plan)
       :items (mapv (fn [item]
                      {:item-id (:change-item/id item)
                       :file (:change-item/file item)
                       :status (:change-item/status item)
                       :result (:change-item/result item)})
                    items)})))

(defn get-pending-items
  "Get pending items for a plan."
  [plan-id]
  (ds/get-pending-items plan-id))

(defn get-plan-items
  "Get all items for a plan."
  [plan-id]
  (ds/get-plan-items plan-id))

;;; ============================================================
;;; Failed Items Queries
;;; ============================================================

(defn get-failed-items
  "Get details of failed items for a wave.

   Arguments:
     wave-id - Wave identifier

   Returns:
     Vector of maps with :item-id :file :result for failed items."
  [wave-id]
  (when-let [status (get-wave-status wave-id)]
    (let [plan-status (get-plan-status (:plan-id status))]
      (->> (:items plan-status)
           (filter #(= :failed (:status %)))
           (mapv #(select-keys % [:item-id :file :result]))))))

(defn get-wave-summary
  "Get a summary of wave execution.

   Arguments:
     wave-id - Wave identifier

   Returns:
     Map with :wave-id :status :completed :failed :success-rate :failed-items"
  [wave-id]
  (when-let [status (get-wave-status wave-id)]
    (let [completed (:completed-count status)
          failed (:failed-count status)
          total (+ completed failed)
          success-rate (if (pos? total) (/ (double completed) total) 1.0)]
      {:wave-id wave-id
       :status (:status status)
       :completed completed
       :failed failed
       :success-rate success-rate
       :failed-items (get-failed-items wave-id)})))

;;; ============================================================
;;; Formatting
;;; ============================================================

(defn format-wave-status
  "Format wave status for display.

   Arguments:
     wave-id - Wave identifier

   Returns:
     Formatted string or nil if wave not found."
  [wave-id]
  (when-let [summary (get-wave-summary wave-id)]
    (format "Wave %s: %s (%d/%d completed, %.1f%% success rate)"
            wave-id
            (name (:status summary))
            (:completed summary)
            (+ (:completed summary) (:failed summary))
            (* 100 (:success-rate summary)))))

(defn format-failed-items
  "Format failed items for display.

   Arguments:
     wave-id - Wave identifier

   Returns:
     Formatted string listing failed items."
  [wave-id]
  (let [failed (get-failed-items wave-id)]
    (if (empty? failed)
      "No failed items"
      (->> failed
           (map #(format "  - %s: %s" (:file %) (:result %)))
           (cons "Failed items:")
           (clojure.string/join "\n")))))

;;; ============================================================
;;; Active Waves
;;; ============================================================

(defn get-active-waves
  "Get all currently active waves.

   Returns:
     Vector of wave status maps."
  []
  (->> (ds/get-all-waves)
       (filter #(#{:pending :in-progress :running} (:wave/status %)))
       (mapv (fn [w]
               {:wave-id (:wave/id w)
                :plan-id (:wave/plan w)
                :status (:wave/status w)
                :active-count (:wave/active-count w)
                :started-at (:wave/started-at w)}))))

(defn get-recent-waves
  "Get recently completed waves.

   Arguments:
     limit - Maximum number of waves to return (default: 10)

   Returns:
     Vector of wave status maps, most recent first."
  [& [limit]]
  (let [n (or limit 10)]
    (->> (ds/get-all-waves)
         (sort-by :wave/started-at #(compare %2 %1))
         (take n)
         (mapv (fn [w]
                 {:wave-id (:wave/id w)
                  :plan-id (:wave/plan w)
                  :status (:wave/status w)
                  :completed-count (:wave/completed-count w)
                  :failed-count (:wave/failed-count w)
                  :started-at (:wave/started-at w)
                  :completed-at (:wave/completed-at w)})))))
