(ns olympus-web.ws.state
  "Re-frame subscriptions and events for WebSocket connection state.

   Provides enriched state management for the WS connection:
   - Connection status subscriptions (:connected, :reconnecting, :disconnected)
   - Backoff state tracking (attempt count, current delay, next retry time)
   - Heartbeat management
   - DataScript state update events (from server-pushed snapshots)

   State shape in app-db [:connection]:
   {:status              :disconnected | :connecting | :connected | :reconnecting
    :url                 \"ws://localhost:7911\"
    :reconnect-attempts  0
    :backoff-delay-ms    nil   ; current computed delay (nil when connected)
    :next-retry-at       nil   ; js/Date.now + delay (nil when connected)
    :last-error          nil
    :last-snapshot       nil   ; timestamp of last init-snapshot
    :connected-at        nil   ; timestamp of connection established
    :messages-received   0}"   ; counter for debugging
  (:require [re-frame.core :as rf]
            [olympus-web.config :as config]))

;; =============================================================================
;; Connection State Events
;; =============================================================================

(rf/reg-event-fx
 :ws/reconnect-scheduled
 (fn [{:keys [db]} [_ {:keys [attempt delay-ms]}]]
   {:db (-> db
            (assoc-in [:connection :status] :reconnecting)
            (assoc-in [:connection :reconnect-attempts] (inc attempt))
            (assoc-in [:connection :backoff-delay-ms] delay-ms)
            (assoc-in [:connection :next-retry-at] (+ (js/Date.now) delay-ms)))}))

(rf/reg-event-db
 :ws/disconnected-clean
 (fn [db _]
   (-> db
       (assoc-in [:connection :status] :disconnected)
       (assoc-in [:connection :reconnect-attempts] 0)
       (assoc-in [:connection :backoff-delay-ms] nil)
       (assoc-in [:connection :next-retry-at] nil)
       (assoc-in [:connection :connected-at] nil))))

(rf/reg-event-db
 :ws/message-counted
 (fn [db _]
   (update-in db [:connection :messages-received] (fnil inc 0))))

;; =============================================================================
;; Connection Subscriptions (enriched)
;; =============================================================================

(rf/reg-sub
 :ws/connection
 (fn [db _]
   (:connection db)))

(rf/reg-sub
 :ws/connected?
 :<- [:connection/status]
 (fn [status _]
   (= status :connected)))

(rf/reg-sub
 :ws/reconnecting?
 :<- [:connection/status]
 (fn [status _]
   (= status :reconnecting)))

(rf/reg-sub
 :ws/disconnected?
 :<- [:connection/status]
 (fn [status _]
   (#{:disconnected} status)))

(rf/reg-sub
 :ws/reconnect-attempts
 (fn [db _]
   (get-in db [:connection :reconnect-attempts] 0)))

(rf/reg-sub
 :ws/backoff-delay
 (fn [db _]
   (get-in db [:connection :backoff-delay-ms])))

(rf/reg-sub
 :ws/next-retry-at
 (fn [db _]
   (get-in db [:connection :next-retry-at])))

(rf/reg-sub
 :ws/connected-at
 (fn [db _]
   (get-in db [:connection :connected-at])))

(rf/reg-sub
 :ws/messages-received
 (fn [db _]
   (get-in db [:connection :messages-received] 0)))

(rf/reg-sub
 :ws/connection-display
 :<- [:ws/connection]
 (fn [conn _]
   (let [status (:status conn)]
     {:status status
      :status-text (case status
                     :connected    "Connected"
                     :connecting   "Connecting..."
                     :reconnecting (str "Reconnecting (attempt "
                                        (:reconnect-attempts conn)
                                        ", next in "
                                        (when-let [delay (:backoff-delay-ms conn)]
                                          (str (js/Math.round (/ delay 1000)) "s"))
                                        ")")
                     :disconnected "Disconnected"
                     "Unknown")
      :color (case status
               :connected    "#22c55e"  ;; green
               :connecting   "#eab308"  ;; yellow
               :reconnecting "#f97316"  ;; orange
               :disconnected "#ef4444"  ;; red
               "#6b7280")})))

;; =============================================================================
;; Snapshot State Events
;; =============================================================================

(rf/reg-event-db
 :ws/snapshot-timestamp
 (fn [db [_ timestamp]]
   (assoc-in db [:connection :last-snapshot] timestamp)))
