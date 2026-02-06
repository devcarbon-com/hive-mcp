(ns hive-mcp.protocols.automation
  "Protocols for browser automation and web scraping.

   Defines abstractions for programmatic browser control:
   - Page navigation, interaction, and data extraction
   - Session lifecycle management
   - Screenshot capture and JavaScript evaluation

   Architecture:
   - IBrowserAutomation: Core browser control operations
   - IAutomationSession: Session identity and lifecycle
   - NoopBrowserAutomation: No-op fallback (always available)
   - NoopAutomationSession: No-op session fallback
   - Registry for managing multiple automation backends

   Future implementations:
   - PlaywrightAutomation: Playwright-based browser automation
   - PuppeteerAutomation: Puppeteer-based (if needed)

   SOLID-O: Open for extension via new automation backends.
   SOLID-D: Depend on IBrowserAutomation abstraction, not concretions.
   SOLID-S: Session lifecycle separate from browser operations.
   CLARITY-Y: Yield safe failure - return error maps, never throw.
   CLARITY-L: Layers stay pure - protocol boundary between
              automation domain and browser engine implementation.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IAutomationSession Protocol (Session Identity & Lifecycle)
;;; ============================================================================

(defprotocol IAutomationSession
  "Protocol for automation session identity and lifecycle.

   An automation session represents a single browser context with its own
   cookies, storage, and state. Sessions are lightweight and can be created
   per-task for isolation.

   Relationship to IBrowserAutomation:
   - IAutomationSession: Identity and lifecycle (who am I, am I alive?)
   - IBrowserAutomation: Operations (navigate, click, type, etc.)
   - An IBrowserAutomation holds one or more IAutomationSessions

   SOLID-I: Interface segregation - session identity separate from operations.
   CLARITY-I: Introspection first - expose session state for debugging."

  (session-id [this]
    "Return unique string identifier for this session.
     Used for registry lookup, logging, and correlation.
     Example: \"session-abc123\", \"playwright-1234\"")

  (session-info [this]
    "Return metadata about this automation session.
     Returns map with:
       :id          - String identifier (same as session-id)
       :browser     - Browser type keyword (:chromium, :firefox, :webkit, :noop)
       :headless?   - Whether browser is running headless
       :created-at  - Session creation timestamp
       :user-agent  - User agent string (nil if default)
       :viewport    - Map with :width :height (nil if default)
       :metadata    - Backend-specific metadata map")

  (active? [this]
    "Check if this session is still active and usable.
     Returns boolean.

     A session becomes inactive when:
     - close! is called on the parent automation
     - The browser process crashes
     - The session times out"))

;;; ============================================================================
;;; IBrowserAutomation Protocol (Core Browser Control)
;;; ============================================================================

(defprotocol IBrowserAutomation
  "Protocol for programmatic browser control.

   Provides the core operations for web automation:
   - Browser lifecycle (launch, close)
   - Navigation (go to URL)
   - Interaction (click, type)
   - Data extraction (page source, screenshots)
   - Waiting (selector-based)
   - JavaScript evaluation

   Implementations:
   - NoopBrowserAutomation: No-op fallback (always available)
   - PlaywrightAutomation: Full Playwright implementation (future)

   All mutating methods end with ! (Clojure convention).
   All methods return result maps, never throw.

   CLARITY-Y: Must not throw - return :success? false on failure.
   CLARITY-T: Log operations for observability."

  (launch! [this opts]
    "Launch a browser instance and create an automation session.

     Arguments:
       opts - Browser launch options:
              :browser    - Browser type (:chromium, :firefox, :webkit) default :chromium
              :headless?  - Run headless (default: true)
              :viewport   - Map with :width :height (default: {:width 1280 :height 720})
              :user-agent - Custom user agent string (nil for default)
              :timeout-ms - Launch timeout (default: 30000)
              :args       - Additional browser args vector

     Returns map with:
       :success?  - Boolean indicating launch success
       :session   - IAutomationSession instance (nil on failure)
       :errors    - Vector of error messages (empty on success)
       :metadata  - Launch metadata (browser version, pid, etc.)

     CLARITY-Y: Must not throw - return :success? false on failure.")

  (close! [this]
    "Close the browser and release all resources.

     Closes all pages, contexts, and the browser process.

     Returns map with:
       :success?  - Boolean indicating clean shutdown
       :errors    - Vector of error messages (empty on success)

     Idempotent - safe to call multiple times.")

  (navigate! [this url opts]
    "Navigate the browser to a URL.

     Arguments:
       url  - Target URL string (must be valid URL)
       opts - Navigation options:
              :wait-until  - When to consider navigation done
                             (:load, :domcontentloaded, :networkidle)
                             default: :load
              :timeout-ms  - Navigation timeout (default: 30000)
              :referer     - Referer header string

     Returns map with:
       :success?    - Boolean indicating navigation success
       :url         - Final URL after redirects
       :status-code - HTTP status code (nil if unavailable)
       :errors      - Vector of error messages (empty on success)
       :duration-ms - Navigation time in milliseconds")

  (click! [this selector opts]
    "Click an element matching the CSS/XPath selector.

     Arguments:
       selector - CSS selector or XPath expression string
       opts     - Click options:
                  :button    - Mouse button (:left, :right, :middle) default :left
                  :count     - Click count (1=click, 2=double-click) default 1
                  :timeout-ms - Max wait for element (default: 5000)
                  :force?    - Click even if element is obscured (default: false)
                  :position  - {:x n :y n} click position within element

     Returns map with:
       :success?  - Boolean indicating click success
       :selector  - The selector that was clicked
       :errors    - Vector of error messages (empty on success)")

  (type-text! [this selector text opts]
    "Type text into an element matching the selector.

     Arguments:
       selector - CSS selector or XPath expression for input element
       text     - String to type
       opts     - Typing options:
                  :delay-ms   - Delay between keystrokes (default: 0)
                  :clear?     - Clear field before typing (default: false)
                  :timeout-ms - Max wait for element (default: 5000)

     Returns map with:
       :success?  - Boolean indicating type success
       :selector  - The selector that was typed into
       :text      - The text that was typed
       :errors    - Vector of error messages (empty on success)")

  (screenshot! [this opts]
    "Capture a screenshot of the current page.

     Arguments:
       opts - Screenshot options:
              :path      - File path to save screenshot (nil for bytes)
              :full-page? - Capture full scrollable page (default: false)
              :format    - Image format (:png, :jpeg) default :png
              :quality   - JPEG quality 0-100 (only for :jpeg)
              :selector  - Capture only this element (nil for full page)

     Returns map with:
       :success?  - Boolean indicating capture success
       :path      - File path if saved (nil if bytes returned)
       :bytes     - Screenshot bytes (nil if saved to path)
       :format    - Image format used
       :errors    - Vector of error messages (empty on success)")

  (get-page-source [this]
    "Get the current page's HTML source.

     Returns map with:
       :success? - Boolean indicating retrieval success
       :html     - HTML source string (nil on failure)
       :url      - Current page URL
       :errors   - Vector of error messages (empty on success)")

  (wait-for-selector [this selector opts]
    "Wait for an element matching the selector to appear.

     Arguments:
       selector - CSS selector or XPath expression
       opts     - Wait options:
                  :state      - Element state to wait for
                                (:visible, :hidden, :attached, :detached)
                                default: :visible
                  :timeout-ms - Max wait time (default: 30000)

     Returns map with:
       :success?    - Boolean indicating element was found
       :selector    - The selector waited for
       :found?      - Whether the element was found
       :duration-ms - Time spent waiting
       :errors      - Vector of error messages (empty on success)")

  (evaluate-js [this expression opts]
    "Evaluate JavaScript expression in the browser context.

     Arguments:
       expression - JavaScript expression string
       opts       - Evaluation options:
                    :timeout-ms - Eval timeout (default: 30000)
                    :arg        - Single argument to pass to the expression
                                  (accessible as first arg in JS function)

     Returns map with:
       :success? - Boolean indicating evaluation success
       :result   - JavaScript return value (auto-converted to Clojure)
       :errors   - Vector of error messages (empty on success)"))

;;; ============================================================================
;;; NoopAutomationSession (No-Op Fallback)
;;; ============================================================================

(defrecord NoopAutomationSession [id created-at]
  IAutomationSession

  (session-id [_] id)

  (session-info [_]
    {:id id
     :browser :noop
     :headless? true
     :created-at created-at
     :user-agent nil
     :viewport nil
     :metadata {:engine :noop}})

  (active? [_] false))

(defn ->noop-session
  "Create a NoopAutomationSession.

   Arguments:
     id - String session identifier (default: auto-generated)"
  ([] (->noop-session (str "noop-session-" (System/currentTimeMillis))))
  ([id] (->NoopAutomationSession id (java.time.Instant/now))))

;;; ============================================================================
;;; NoopBrowserAutomation (No-Op Fallback)
;;; ============================================================================

(def ^:private noop-msg "NoopBrowserAutomation: No automation backend configured. Set one via set-automation!")

(defrecord NoopBrowserAutomation []
  IBrowserAutomation

  (launch! [_ _opts]
    {:success? false
     :session nil
     :errors [noop-msg]
     :metadata {:engine :noop}})

  (close! [_]
    {:success? true
     :errors []})

  (navigate! [_ _url _opts]
    {:success? false
     :url nil
     :status-code nil
     :errors [noop-msg]
     :duration-ms 0})

  (click! [_ selector _opts]
    {:success? false
     :selector selector
     :errors [noop-msg]})

  (type-text! [_ selector text _opts]
    {:success? false
     :selector selector
     :text text
     :errors [noop-msg]})

  (screenshot! [_ _opts]
    {:success? false
     :path nil
     :bytes nil
     :format :png
     :errors [noop-msg]})

  (get-page-source [_]
    {:success? false
     :html nil
     :url nil
     :errors [noop-msg]})

  (wait-for-selector [_ selector _opts]
    {:success? false
     :selector selector
     :found? false
     :duration-ms 0
     :errors [noop-msg]})

  (evaluate-js [_ _expression _opts]
    {:success? false
     :result nil
     :errors [noop-msg]}))

;;; ============================================================================
;;; Automation Registry
;;; ============================================================================

(defonce ^:private automation-registry (atom {}))

(defn register-automation!
  "Register a browser automation backend.

   Arguments:
     id         - Keyword identifier (:playwright, :puppeteer, etc.)
     automation - Implementation of IBrowserAutomation protocol

   Returns the automation.
   Does NOT auto-launch - call launch! separately."
  [id automation]
  {:pre [(keyword? id)
         (satisfies? IBrowserAutomation automation)]}
  (swap! automation-registry assoc id automation)
  automation)

(defn get-automation
  "Get automation backend by ID.

   Arguments:
     id - Automation keyword (:playwright, :puppeteer, etc.)

   Returns automation or nil if not found."
  [id]
  (get @automation-registry id))

(defn list-automations
  "List all registered automation backend IDs.

   Returns vector of keyword IDs."
  []
  (vec (keys @automation-registry)))

(defn automation-registered?
  "Check if an automation backend is registered."
  [id]
  (contains? @automation-registry id))

(defn unregister-automation!
  "Unregister an automation backend. Closes it first.

   Arguments:
     id - Automation keyword

   Returns true if removed, false if not found."
  [id]
  (if-let [automation (get-automation id)]
    (do
      (close! automation)
      (swap! automation-registry dissoc id)
      true)
    false))

;;; ============================================================================
;;; Active Implementation Management
;;; ============================================================================

;; Atom holding the currently active IBrowserAutomation implementation.
;; Defaults to NoopBrowserAutomation (no-op fallback).
(defonce ^:private active-automation (atom nil))

(defn set-automation!
  "Set the active browser automation implementation.

   Arguments:
     automation - Implementation of IBrowserAutomation protocol

   Returns:
     The automation.

   Throws:
     AssertionError if automation doesn't satisfy protocol."
  [automation]
  {:pre [(satisfies? IBrowserAutomation automation)]}
  (reset! active-automation automation)
  automation)

(defn get-active-automation
  "Get the active browser automation implementation.

   Returns NoopBrowserAutomation if no automation is set.
   This ensures hive-mcp always works, with or without a real backend."
  []
  (or @active-automation
      (->NoopBrowserAutomation)))

(defn automation-set?
  "Check if an active automation backend is configured.

   Returns:
     true if set-automation! has been called."
  []
  (some? @active-automation))

(defn clear-automation!
  "Clear the active automation backend. Used for testing.

   Returns nil."
  []
  (reset! active-automation nil)
  nil)

(defn clear-registry!
  "Clear the automation registry. Used for testing.

   Returns nil."
  []
  (reset! automation-registry {})
  nil)

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defn browser-automation?
  "Check if object implements IBrowserAutomation protocol."
  [x]
  (satisfies? IBrowserAutomation x))

(defn automation-session?
  "Check if object implements IAutomationSession protocol."
  [x]
  (satisfies? IAutomationSession x))

(defn enhanced?
  "Check if enhanced automation capabilities are available.

   Returns:
     true if a non-noop IBrowserAutomation implementation is active."
  []
  (and (automation-set?)
       (not (instance? NoopBrowserAutomation @active-automation))))

(defn capabilities
  "Get a summary of available automation capabilities.

   Returns:
     Map describing what features are available."
  []
  (let [automation (get-active-automation)]
    {:engine-type (if (enhanced?)
                    (-> automation class .getSimpleName)
                    :noop)
     :enhanced? (enhanced?)
     :launch? true        ;; Always available (may be no-op)
     :close? true
     :navigate? true
     :click? true
     :type-text? true
     :screenshot? true
     :get-page-source? true
     :wait-for-selector? true
     :evaluate-js? true}))
