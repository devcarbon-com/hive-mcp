(ns emacs-mcp.server
  "MCP server for Emacs interaction via emacsclient."
  (:require [io.modelcontext.clojure-sdk.stdio-server :as io-server]
            [emacs-mcp.tools :as tools]
            [taoensso.timbre :as log])
  (:gen-class))

;; Convert our tool definitions to the SDK format
(defn make-tool
  "Convert a tool definition with :handler to SDK format."
  [{:keys [name description inputSchema handler]}]
  {:name name
   :description description
   :inputSchema inputSchema
   :handler handler})

(def emacs-server-spec
  {:name "emacs-mcp"
   :version "0.1.0"
   :tools (mapv make-tool tools/tools)})

(defn start!
  "Start the MCP server."
  [& _args]
  (let [server-id (random-uuid)]
    (log/info "Starting emacs-mcp server:" server-id)
    @(io-server/run! (assoc emacs-server-spec :server-id server-id))))

(defn -main
  "Entry point for the MCP server."
  [& args]
  (apply start! args))

(comment
  ;; For REPL development
  (start!))
