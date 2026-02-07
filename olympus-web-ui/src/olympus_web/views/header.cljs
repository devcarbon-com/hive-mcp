(ns olympus-web.views.header
  "Header component with title and connection status."
  (:require [re-frame.core :as rf]))

(defn connection-indicator
  "Shows WebSocket connection status with backoff info.
   Uses enriched :ws/connection-display subscription from ws.state."
  []
  (let [display @(rf/subscribe [:ws/connection-display])
        error @(rf/subscribe [:connection/error])]
    [:div.connection-status
     [:span.status-dot {:class (name (:status display))
                        :style {:background-color (:color display)}}]
     [:span (:status-text display)]
     (when (and error (= (:status display) :disconnected))
       [:span.connection-error {:style {:font-size "0.75rem" :opacity 0.7}}
        (str " — " error)])]))

(defn header
  "App header with logo, title, and status."
  []
  [:header.header
   [:div.header-title
    [:span.logo "🏛️"]
    [:span "Olympus"]
    [:span {:style {:font-weight "normal" :opacity 0.7 :font-size "0.875rem"}}
     " — Hive Swarm Dashboard"]]
   [connection-indicator]])
