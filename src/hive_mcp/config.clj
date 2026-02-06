(ns hive-mcp.config
  "Global configuration loader for ~/.config/hive.edn.

   Bounded context: Global user-level configuration for hive-mcp.

   SOLID:
   - S: Single responsibility — load+cache global config
   - O: Open for extension — new config keys can be added without code changes
   - L: Layers pure — core functions are pure, I/O isolated to load-global-config!
   - I: Inputs guarded — missing file returns defaults, malformed file logged

   CLARITY:
   - R: Represented intent — explicit defaults merged with user overrides
   - Y: Yield safe failure — missing/malformed config uses defaults

   Schema (~/.config/hive.edn):
     {:project-roots  [\"path1\" \"path2\"]
      :defaults       {:kg-backend :datahike :hot-reload false}
      :project-overrides {\"proj\" {:hot-reload true}}
      :parent-rules   [{:path-prefix \"/path/prefix/\" :parent-id \"parent-proj\"}]}

   Usage:
     (load-global-config!)       ;; Load from disk, cache in atom
     (get-global-config)         ;; Return cached config (or defaults)
     (get-project-roots)         ;; Shortcut for :project-roots
     (get-defaults)              ;; Shortcut for :defaults
     (get-project-overrides k)   ;; Overrides for specific project
     (get-parent-for-path p)     ;; Resolve parent-id via :parent-rules"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Defaults
;; =============================================================================

(def ^:private default-config
  "Default configuration when ~/.config/hive.edn is missing or incomplete."
  {:project-roots []
   :defaults {:kg-backend :datahike
              :hot-reload false
              :presets-path nil}
   :project-overrides {}
   :parent-rules []})

(def ^:private config-path
  "Canonical path for global hive config."
  (str (System/getProperty "user.home") "/.config/hive.edn"))

;; =============================================================================
;; State
;; =============================================================================

;; Cached global configuration atom.
;; nil = not loaded yet, map = loaded config.
(defonce ^:private global-config (atom nil))

;; =============================================================================
;; Loading
;; =============================================================================

(defn- read-config-file
  "Read and parse an EDN config file. Returns parsed map or nil on failure.
   CLARITY-Y: Yield safe failure — logs and returns nil on any error."
  [path]
  (try
    (let [f (io/file path)]
      (when (.exists f)
        (let [content (slurp f)
              parsed (edn/read-string content)]
          (when (map? parsed)
            parsed))))
    (catch Exception e
      (log/warn "Failed to read config file" path ":" (.getMessage e))
      nil)))

(defn- merge-config
  "Deep-merge user config with defaults.
   User values override defaults at each level.
   Nested maps are merged recursively; other values replaced."
  [defaults user-config]
  (merge-with
   (fn [default-val user-val]
     (if (and (map? default-val) (map? user-val))
       (merge default-val user-val)
       user-val))
   defaults
   user-config))

(defn load-global-config!
  "Load global config from ~/.config/hive.edn, merge with defaults, cache in atom.

   Safe to call multiple times — always re-reads from disk and updates cache.
   Returns the merged config map.

   CLARITY-Y: If file is missing or malformed, returns defaults (never throws)."
  ([]
   (load-global-config! config-path))
  ([path]
   (let [user-config (read-config-file path)
         merged (if user-config
                  (merge-config default-config user-config)
                  default-config)]
     (reset! global-config merged)
     (log/info "Global config loaded from" path
               (if user-config "(user config found)" "(using defaults)"))
     merged)))

;; =============================================================================
;; Accessors (all return defaults if not yet loaded)
;; =============================================================================

(defn get-global-config
  "Return the cached global config, or defaults if not yet loaded.
   Does NOT trigger a disk read — call load-global-config! first."
  []
  (or @global-config default-config))

(defn get-project-roots
  "Return the :project-roots vector from global config.
   These are root directories to scan for projects."
  []
  (:project-roots (get-global-config)))

(defn get-defaults
  "Return the :defaults map from global config.
   Contains default settings for new/unconfigured projects."
  []
  (:defaults (get-global-config)))

(defn get-project-overrides
  "Return overrides for a specific project-id, or nil if none.
   Overrides are merged with :defaults by the caller."
  [project-id]
  (get-in (get-global-config) [:project-overrides project-id]))

(defn get-project-config
  "Return the effective config for a project-id.
   Merges :defaults with :project-overrides for that project.
   Returns just :defaults if no overrides exist."
  [project-id]
  (let [defaults (get-defaults)
        overrides (get-project-overrides project-id)]
    (if overrides
      (merge defaults overrides)
      defaults)))

(defn get-parent-rules
  "Return the :parent-rules vector from global config.
   Each rule is {:path-prefix \"...\" :parent-id \"...\"}."
  []
  (:parent-rules (get-global-config)))

(defn get-parent-for-path
  "Resolve parent-id for a directory path via :parent-rules.

   Iterates through rules in order. First rule whose :path-prefix
   matches the start of the given path wins.

   Returns parent-id string or nil if no rule matches.

   Example:
     With rule {:path-prefix \"/home/user/PP/hive/\" :parent-id \"hive-mcp\"}
     (get-parent-for-path \"/home/user/PP/hive/hive-hot\") => \"hive-mcp\""
  [directory-path]
  (when directory-path
    (let [rules (get-parent-rules)
          ;; Normalize path to end with / for consistent prefix matching
          norm-path (if (.endsWith (str directory-path) "/")
                      (str directory-path)
                      (str directory-path "/"))]
      (->> rules
           (filter (fn [{:keys [path-prefix]}]
                     (and path-prefix
                          (.startsWith norm-path path-prefix))))
           first
           :parent-id))))

;; =============================================================================
;; Reset (for testing)
;; =============================================================================

(defn reset-config!
  "Reset the cached config to nil. Useful for testing."
  []
  (reset! global-config nil))
