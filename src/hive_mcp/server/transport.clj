(ns hive-mcp.server.transport
  "Network server management: nREPL, WebSocket, Channel.

   Bounded context: MCP protocol handling and network transport.

   Manages:
   - Embedded nREPL server (for bb-mcp tool forwarding)
   - WebSocket MCP server (Claude Code IDE integration)
   - WebSocket channel with auto-healing (hivemind events)
   - Olympus WebSocket server (Olympus Web UI)
   - Legacy TCP channel (deprecated, backward compat)"
  (:require [nrepl.server :as nrepl-server]
            [hive-mcp.transport.websocket :as ws]
            [hive-mcp.transport.olympus :as olympus-ws]
            [hive-mcp.channel :as channel]
            [hive-mcp.channel.websocket :as ws-channel]
            [clojure.core.async :as async]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; nREPL Server
;; =============================================================================

(defn start-embedded-nrepl!
  "Start an embedded nREPL server for bb-mcp tool forwarding.

   CRITICAL: This runs in the SAME JVM as the MCP server and channel,
   allowing bb-mcp to forward tool calls that access the live channel.

   Without this, bb-mcp connects to a separate nREPL JVM that has no
   channel server running, so hivemind broadcasts go nowhere.

   Parameters:
     nrepl-server-atom - atom to store nREPL server reference for shutdown"
  [nrepl-server-atom]
  (let [nrepl-port (parse-long (or (System/getenv "HIVE_MCP_NREPL_PORT") "7910"))]
    (try
      ;; Try to load cider middleware if available
      (let [middleware (try
                         (require 'cider.nrepl)
                         (let [mw-var (resolve 'cider.nrepl/cider-middleware)]
                           (when mw-var @mw-var))
                         (catch Exception _
                           nil))
            ;; default-handler takes middleware as varargs, use apply
            handler (if (seq middleware)
                      (apply nrepl-server/default-handler middleware)
                      (nrepl-server/default-handler))
            server-opts {:port nrepl-port :bind "127.0.0.1" :handler handler}
            server (nrepl-server/start-server server-opts)]
        (reset! nrepl-server-atom server)
        (log/info "Embedded nREPL started on port" nrepl-port
                  (if middleware "(with CIDER middleware)" "(basic)"))
        server)
      (catch Exception e
        (log/warn "Embedded nREPL failed to start (non-fatal):" (.getMessage e))
        nil))))

;; =============================================================================
;; WebSocket MCP Server
;; =============================================================================

(defn start-websocket-server!
  "Start WebSocket MCP server if HIVE_MCP_WEBSOCKET=true."
  []
  (when (= "true" (System/getenv "HIVE_MCP_WEBSOCKET"))
    (let [port (some-> (System/getenv "HIVE_MCP_WS_PORT") parse-long)
          project-dir (System/getenv "HIVE_MCP_PROJECT_DIR")]
      (log/info "Starting WebSocket MCP server" {:port port :project-dir project-dir})
      (ws/start-server! {:port port
                         :project-dir project-dir}))))

;; =============================================================================
;; WebSocket Channel with Auto-Healing
;; =============================================================================

(defn start-ws-channel-with-healing!
  "Start WebSocket channel server with auto-healing.

   CLARITY: Yield safe failure - if server dies, restart it automatically.
   Runs a background async loop that monitors and restarts if needed.

   Parameters:
     ws-channel-monitor - atom to store the monitoring go-loop channel"
  [ws-channel-monitor]
  (let [port (parse-long (or (System/getenv "HIVE_MCP_WS_CHANNEL_PORT") "9999"))
        check-interval-ms 30000] ; Check every 30 seconds
    ;; Start initial server
    (try
      (ws-channel/start! {:port port})
      (log/info "WebSocket channel server started on port" port)
      (catch Exception e
        (log/warn "WebSocket channel initial start failed:" (.getMessage e))))
    ;; Start monitoring loop
    (when-not @ws-channel-monitor
      (reset! ws-channel-monitor
              (async/go-loop []
                (async/<! (async/timeout check-interval-ms))
                (when-not (ws-channel/connected?)
                  (log/info "WebSocket channel: no clients, server healthy"))
                  ;; Server running but no clients is fine

                (when-not (:running? (ws-channel/status))
                  (log/warn "WebSocket channel server died, attempting restart...")
                  (try
                    (ws-channel/start! {:port port})
                    (log/info "WebSocket channel server restarted on port" port)
                    (catch Exception e
                      (log/error "WebSocket channel restart failed:" (.getMessage e)))))
                (recur)))
      (log/info "WebSocket channel auto-heal monitor started"))))

;; =============================================================================
;; Olympus WebSocket Server
;; =============================================================================

(defn start-olympus-ws!
  "Start Olympus WebSocket server for Olympus Web UI (port 7911).
   Sends full snapshot on connect, supports typed event protocol."
  []
  (try
    (olympus-ws/start!)
    (olympus-ws/wire-hivemind-events!)
    (log/info "Olympus WebSocket server started on port 7911")
    (catch Exception e
      (log/warn "Olympus WebSocket server failed to start (non-fatal):" (.getMessage e)))))

;; =============================================================================
;; Legacy Channel (deprecated)
;; =============================================================================

(defn start-legacy-channel!
  "Start legacy bidirectional channel server (deprecated - kept for backward compat).
   Marks coordinator as running to protect from test fixture cleanup."
  []
  (let [channel-port (parse-long (or (System/getenv "HIVE_MCP_CHANNEL_PORT") "9998"))]
    (try
      (channel/start-server! {:type :tcp :port channel-port})
      ;; Mark coordinator as running to protect from test fixture cleanup
      ;; CLARITY-Y: This prevents ch/stop-server! in test fixtures from killing
      ;; the production server when tests run in the same JVM
      (channel/mark-coordinator-running!)
      (log/info "Legacy channel server started on TCP port" channel-port)
      (catch Exception e
        (log/warn "Legacy channel server failed to start (non-fatal):" (.getMessage e))))))
