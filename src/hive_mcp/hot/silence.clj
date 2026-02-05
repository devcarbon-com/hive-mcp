(ns hive-mcp.hot.silence
  "SAA Silence Strategy - Hot-reload aware exploration phase.

   Implements the 'Silence' phase of the SAA (Silence-Abstract-Act) workflow:
   - Ground in territory before creating abstractions
   - Track files read and discoveries made
   - Pause exploration during hot-reload to avoid stale state
   - Generate grounding metadata for Knowledge Graph integration

   Philosophy (General Semantics):
   'The map is not the territory' - explore first, abstract second.

   Hot-Reload Integration:
   - Registers with hive-hot to receive reload events
   - Pauses exploration during reload (prevents reading stale state)
   - Resumes automatically after successful reload
   - Tracks reload count for debugging

   CLARITY: Explicit state management over implicit preservation."
  (:require [taoensso.timbre :as log])
  (:import [java.time Instant]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; State
;;; =============================================================================

(defonce ^:private silence-state
  (atom {:session nil}))

(defn- generate-session-id
  "Generate unique session ID."
  []
  (str "silence-" (System/currentTimeMillis) "-" (subs (str (random-uuid)) 0 8)))

(defn- now-inst
  "Get current instant as java.util.Date for compatibility."
  []
  (java.util.Date/from (Instant/now)))

;;; =============================================================================
;;; Session Lifecycle
;;; =============================================================================

(defn start-exploration!
  "Start a new exploration session.
   
   Arguments:
     opts - Map with:
       :task     - Description of what's being explored
       :agent-id - ID of the ling performing exploration
   
   Returns:
     Session ID string"
  [{:keys [task agent-id]}]
  (let [session-id (generate-session-id)]
    (reset! silence-state
            {:session {:id session-id
                       :task task
                       :agent-id agent-id
                       :started-at (now-inst)
                       :paused? false
                       :files-read []
                       :discoveries []
                       :hot-reload-count 0}})
    (log/info "[silence] Started exploration session" 
              {:session-id session-id :task task})
    session-id))

(defn status
  "Get current exploration session status.
   
   Returns map with:
     :exploring?        - Whether a session is active
     :task              - Current task description
     :files-read        - Count of files read
     :paused?           - Whether paused for hot-reload
     :hot-reload-count  - Number of hot-reloads during session"
  []
  (if-let [session (:session @silence-state)]
    {:exploring? true
     :session-id (:id session)
     :task (:task session)
     :agent-id (:agent-id session)
     :files-read (count (:files-read session))
     :discoveries (count (:discoveries session))
     :paused? (:paused? session)
     :hot-reload-count (:hot-reload-count session)
     :started-at (:started-at session)}
    {:exploring? false
     :paused? false
     :files-read 0
     :discoveries 0}))

(defn end-exploration!
  "End the current exploration session.
   
   Returns:
     Summary map with exploration results and grounding metadata,
     or nil if no active session."
  []
  (when-let [session (:session @silence-state)]
    (let [ended-at (now-inst)
          files (:files-read session)
          discoveries (:discoveries session)
          total-lines (reduce + 0 (keep :lines files))
          summary {:session-id (:id session)
                   :task (:task session)
                   :agent-id (:agent-id session)
                   :started-at (:started-at session)
                   :ended-at ended-at
                   :files-read (count files)
                   :total-lines total-lines
                   :discoveries (count discoveries)
                   :hot-reload-count (:hot-reload-count session)
                   ;; KG-compatible grounding section
                   :grounding {:files-read (mapv (fn [f]
                                                   (select-keys f [:path :hash :namespace :read-at]))
                                                 files)
                               :discoveries discoveries}}]
      (reset! silence-state {:session nil})
      (log/info "[silence] Ended exploration session" 
                {:session-id (:id session)
                 :files (count files)
                 :discoveries (count discoveries)})
      summary)))

(defn reset-state!
  "Reset silence state. For testing."
  []
  (reset! silence-state {:session nil}))

;;; =============================================================================
;;; File Read Tracking
;;; =============================================================================

(defn record-file-read!
  "Record a file read during exploration.
   
   Arguments:
     path - File path that was read
     opts - Map with optional metadata:
       :lines     - Line count
       :hash      - Content hash (for grounding)
       :namespace - Clojure namespace if applicable
   
   Returns:
     :ok        - File read recorded
     :paused    - Exploration paused for hot-reload
     :no-session - No active exploration session"
  [path opts]
  (let [session (:session @silence-state)]
    (cond
      (nil? session)
      :no-session
      
      (:paused? session)
      :paused
      
      :else
      (do
        (swap! silence-state update-in [:session :files-read]
               conj (merge {:path path
                            :read-at (now-inst)}
                           opts))
        :ok))))

(defn get-files-read
  "Get list of files read in current session."
  []
  (get-in @silence-state [:session :files-read] []))

;;; =============================================================================
;;; Discovery Tracking
;;; =============================================================================

(defn record-discovery!
  "Record a discovery/finding during exploration.
   
   Arguments:
     discovery - Map with:
       :type        - Discovery type (:pattern :issue :decision :convention)
       :description - What was discovered
       :file        - Related file (optional)
       :line        - Related line number (optional)
   
   Returns:
     :ok         - Discovery recorded
     :no-session - No active exploration session"
  [discovery]
  (if-let [session (:session @silence-state)]
    (do
      (swap! silence-state update-in [:session :discoveries]
             conj (assoc discovery :discovered-at (now-inst)))
      :ok)
    :no-session))

(defn get-discoveries
  "Get list of discoveries in current session."
  []
  (get-in @silence-state [:session :discoveries] []))

;;; =============================================================================
;;; Hot-Reload Integration
;;; =============================================================================

(defn on-reload-start!
  "Called when hot-reload starts. Pauses exploration."
  []
  (when (:session @silence-state)
    (swap! silence-state assoc-in [:session :paused?] true)
    (log/debug "[silence] Paused exploration for hot-reload")))

(defn on-reload-success!
  "Called when hot-reload succeeds. Resumes exploration."
  []
  (when (:session @silence-state)
    (swap! silence-state (fn [state]
                           (-> state
                               (assoc-in [:session :paused?] false)
                               (update-in [:session :hot-reload-count] inc))))
    (log/debug "[silence] Resumed exploration after hot-reload")))

(defn on-reload-error!
  "Called when hot-reload fails. Resumes exploration (code unchanged)."
  []
  (when (:session @silence-state)
    (swap! silence-state (fn [state]
                           (-> state
                               (assoc-in [:session :paused?] false)
                               (update-in [:session :hot-reload-count] inc))))
    (log/debug "[silence] Resumed exploration after hot-reload error")))

;;; =============================================================================
;;; hive-hot Registration
;;; =============================================================================

(defn- resolve-hot-fn
  "Dynamically resolve hive-hot function."
  [fn-sym]
  (try
    (require 'hive-hot.core)
    (when-let [v (ns-resolve (find-ns 'hive-hot.core) fn-sym)]
      @v)
    (catch Exception _ nil)))

(defn register-with-hive-hot!
  "Register Silence callbacks with hive-hot.
   
   Registers a listener that:
   - Pauses exploration on :reload-start
   - Resumes exploration on :reload-success or :reload-error
   
   Returns:
     :registered     - Successfully registered
     :not-available  - hive-hot not available"
  []
  (if-let [add-listener! (resolve-hot-fn 'add-listener!)]
    (do
      (add-listener! :hive-mcp-silence
                     (fn [{:keys [type]}]
                       (case type
                         :reload-start (on-reload-start!)
                         :reload-success (on-reload-success!)
                         :reload-error (on-reload-error!)
                         nil)))
      (log/info "[silence] Registered with hive-hot")
      :registered)
    (do
      (log/debug "[silence] hive-hot not available")
      :not-available)))

(defn unregister-from-hive-hot!
  "Remove Silence callbacks from hive-hot."
  []
  (when-let [remove-listener! (resolve-hot-fn 'remove-listener!)]
    (remove-listener! :hive-mcp-silence)
    (log/info "[silence] Unregistered from hive-hot")
    :unregistered))
