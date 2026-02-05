(ns hive-mcp.protocols.automation
  "Browser automation abstraction layer for hive-mcp.

   Provides a unified protocol for browser automation backends,
   allowing seamless switching between implementations (Playwright, Selenium, etc.).

   Architecture:
   - IBrowserAutomation defines core browser operations
   - Implementations wrap specific automation libraries
   - Supports headless and headed modes for flexibility
   - Enables screenshot capture, DOM interaction, and navigation

   Use Cases:
   - Visual regression testing for Olympus UI
   - Automated documentation screenshots
   - Integration testing of web components
   - Web scraping for knowledge acquisition

   Usage:
     ;; Create automation instance
     (def browser (playwright-automation {:headless? true}))

     ;; Browser operations
     (launch! browser)
     (navigate! browser \"https://example.com\")
     (screenshot! browser \"/tmp/capture.png\")
     (click! browser \"button.submit\")
     (type-text! browser \"input#search\" \"query\")
     (get-text browser \"h1.title\")
     (close! browser)

   SOLID-O: Open for extension via new implementations
   CLARITY-L: Layers stay pure - protocol boundary between
              automation domain logic and library implementation.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; IBrowserAutomation Protocol
;;; =============================================================================

(defprotocol IBrowserAutomation
  "Protocol for browser automation backends.

   All browser automation modules should use this protocol
   instead of calling Playwright/Selenium directly.

   Implementations:
   - PlaywrightAutomation: Modern, fast, recommended for new code
   - SeleniumAutomation: Legacy support, broader browser coverage
   - PuppeteerAutomation: Chrome-focused, JS ecosystem integration"

  (launch! [this]
    "Launch the browser instance.
     Must be called before any other operations.
     Returns true on success, throws on failure.")

  (close! [this]
    "Close the browser and release all resources.
     Safe to call multiple times.")

  (launched? [this]
    "Check if browser instance is currently running.
     Returns true if browser is active and ready.")

  (navigate! [this url]
    "Navigate to the specified URL.
     url - full URL string (http/https)
     Returns true when page load completes.")

  (current-url [this]
    "Get the current page URL.
     Returns URL string or nil if no page loaded.")

  (screenshot! [this path]
    "Capture screenshot of current page.
     path - absolute file path for output (png/jpg)
     Returns true on success.")

  (screenshot-element! [this selector path]
    "Capture screenshot of specific element.
     selector - CSS selector string
     path - absolute file path for output
     Returns true on success, nil if element not found.")

  (click! [this selector]
    "Click on an element matching the selector.
     selector - CSS selector string
     Returns true on success, nil if element not found.")

  (type-text! [this selector text]
    "Type text into an input element.
     selector - CSS selector for input/textarea
     text - string to type
     Returns true on success.")

  (clear! [this selector]
    "Clear text from an input element.
     selector - CSS selector for input/textarea
     Returns true on success.")

  (get-text [this selector]
    "Get text content of an element.
     selector - CSS selector string
     Returns text string or nil if not found.")

  (get-attribute [this selector attr]
    "Get attribute value from an element.
     selector - CSS selector string
     attr - attribute name (string)
     Returns attribute value or nil.")

  (exists? [this selector]
    "Check if element exists in DOM.
     selector - CSS selector string
     Returns true if element found.")

  (wait-for! [this selector timeout-ms]
    "Wait for element to appear in DOM.
     selector - CSS selector string
     timeout-ms - maximum wait time in milliseconds
     Returns true if found, false on timeout.")

  (execute-js! [this script]
    "Execute JavaScript in browser context.
     script - JavaScript code string
     Returns evaluation result (may be serialized).")

  (get-page-source [this]
    "Get the full HTML source of current page.
     Returns HTML string."))

;;; =============================================================================
;;; IBrowserPage Protocol (Multi-Page Support)
;;; =============================================================================

(defprotocol IBrowserPage
  "Extended protocol for multi-page/tab browser operations.

   Optional extension for automation backends that support
   multiple concurrent pages or browser contexts."

  (new-page! [this]
    "Open a new page/tab in the browser.
     Returns a page identifier (implementation-specific).")

  (switch-page! [this page-id]
    "Switch to a specific page/tab.
     page-id - identifier returned by new-page!
     Returns true on success.")

  (close-page! [this page-id]
    "Close a specific page/tab.
     page-id - identifier of page to close
     Returns true on success.")

  (list-pages [this]
    "List all open pages/tabs.
     Returns sequence of page identifiers."))

;;; =============================================================================
;;; IBrowserContext Protocol (Isolation Support)
;;; =============================================================================

(defprotocol IBrowserContext
  "Protocol for browser context management.

   Contexts provide isolated environments with separate:
   - Cookies and storage
   - Cache
   - Proxy settings

   Useful for:
   - Testing authenticated flows in parallel
   - Simulating different user sessions
   - Isolating test state"

  (new-context! [this opts]
    "Create a new browser context with isolation.
     opts - map with optional keys:
       :viewport {:width 1280 :height 720}
       :user-agent \"custom-ua\"
       :locale \"en-US\"
       :timezone \"America/New_York\"
       :geolocation {:latitude 0 :longitude 0}
       :permissions [\"geolocation\" \"notifications\"]
     Returns context identifier.")

  (close-context! [this context-id]
    "Close a browser context and all its pages.
     context-id - identifier from new-context!
     Returns true on success."))

;;; =============================================================================
;;; Utility Functions
;;; =============================================================================

(defn automation?
  "Check if object implements IBrowserAutomation protocol."
  [x]
  (satisfies? IBrowserAutomation x))

(defn multi-page?
  "Check if automation backend supports multi-page operations."
  [x]
  (satisfies? IBrowserPage x))

(defn context-aware?
  "Check if automation backend supports browser contexts."
  [x]
  (satisfies? IBrowserContext x))
