(ns hive-mcp.agent.sdk.availability
  "SDK availability detection with graceful degradation.

   Checks for libpython-clj2 on classpath and claude-code-sdk
   Python package availability. Results are cached after first check.

   Status values:
   - :available        - Both libpython-clj and claude-code-sdk ready
   - :no-libpython     - libpython-clj not on classpath
   - :no-sdk           - libpython-clj available but claude-code-sdk not installed
   - :not-initialized  - libpython-clj available but Python not yet initialized"
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Cached availability of libpython-clj + claude-code-sdk.
;; Checked once at first use, not at namespace load time.
(defonce ^:private sdk-available? (atom nil))

(defn- check-libpython-available?
  "Check if libpython-clj2 is on the classpath.
   Returns true if the namespace can be required."
  []
  (try
    (require 'libpython-clj2.python)
    true
    (catch Exception _
      (log/debug "[sdk.availability] libpython-clj2 not on classpath")
      false)))

(defn- check-sdk-available?
  "Check if claude-code-sdk Python package is importable.
   Must be called AFTER libpython-clj is initialized."
  []
  (try
    (let [py-fn (requiring-resolve 'libpython-clj2.python/run-simple-string)]
      (py-fn "import claude_code_sdk")
      true)
    (catch Exception _
      (log/debug "[sdk.availability] claude-code-sdk Python package not found")
      false)))

(defn sdk-status
  "Check SDK availability. Returns one of:
   :available        - Both libpython-clj and claude-code-sdk ready
   :no-libpython     - libpython-clj not on classpath
   :no-sdk           - libpython-clj available but claude-code-sdk not installed
   :not-initialized  - libpython-clj available but Python not yet initialized"
  []
  (if-let [cached @sdk-available?]
    cached
    (let [status (cond
                   (not (check-libpython-available?))
                   :no-libpython

                   :else
                   ;; Try to initialize and check SDK
                   (try
                     (let [init! (requiring-resolve 'libpython-clj2.python/initialize!)]
                       (init!)
                       (if (check-sdk-available?)
                         :available
                         :no-sdk))
                     (catch Exception e
                       (log/warn "[sdk.availability] Failed to initialize Python" (ex-message e))
                       :not-initialized)))]
      (reset! sdk-available? status)
      status)))

(defn available?
  "Returns true if the SDK backend is fully available."
  []
  (= :available (sdk-status)))

(defn reset-availability!
  "Reset cached availability check. For testing."
  []
  (reset! sdk-available? nil))
