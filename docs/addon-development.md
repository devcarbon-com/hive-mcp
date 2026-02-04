# Addon Development Guide

This guide explains how to create addons for hive-mcp.

## Overview

Addons extend hive-mcp with integrations for external tools, packages, and services. They follow a consistent pattern with lifecycle hooks for initialization and cleanup.

## Quick Start

1. Copy the template: `elisp/addons/hive-mcp-addon-template.el`
2. Rename to `hive-mcp-YOUR-ADDON.el`
3. Replace "template" with your addon name
4. Implement your integration
5. Register with `hive-mcp-addon-register`

## File Structure

```
elisp/addons/
├── hive-mcp-addon-template.el    # Template to copy
├── hive-mcp-cider.el             # CIDER integration
├── hive-mcp-vibe-kanban.el       # Vibe Kanban integration
├── hive-mcp-package-lint.el      # MELPA tools
└── hive-mcp-YOUR-ADDON.el        # Your addon
```

## Basic Addon Structure

```elisp
;;; hive-mcp-my-addon.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name
;; Author: Your Name <your@email.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Description of what this addon does.

;;; Code:

(require 'hive-mcp-api)

;;;; Customization

(defgroup hive-mcp-my-addon nil
  "My addon for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-my-addon-")

(defcustom hive-mcp-my-addon-auto-start nil
  "When non-nil, auto-start on addon load."
  :type 'boolean
  :group 'hive-mcp-my-addon)

;;;; Internal Variables

(defvar hive-mcp-my-addon--process nil
  "Process object for the server.")

;;;; Lifecycle Functions

(defun hive-mcp-my-addon--addon-init ()
  "Synchronous init - runs immediately after load."
  (require 'hive-mcp-api nil t)
  (message "hive-mcp-my-addon: initialized"))

(defun hive-mcp-my-addon--addon-async-init ()
  "Asynchronous init - runs in background.
Should return a process object if starting a subprocess."
  (when hive-mcp-my-addon-auto-start
    ;; Start your server/process here
    nil))

(defun hive-mcp-my-addon--addon-shutdown ()
  "Shutdown - runs when addon is unloaded."
  (when (and hive-mcp-my-addon--process
             (process-live-p hive-mcp-my-addon--process))
    (kill-process hive-mcp-my-addon--process))
  (message "hive-mcp-my-addon: shutdown complete"))

;;;; Public Functions

;;;###autoload
(defun hive-mcp-my-addon-do-something ()
  "Do something useful."
  (interactive)
  ;; Your implementation
  )

;;;; Registration

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'my-addon
   :version "0.1.0"
   :description "My addon description"
   :requires '(hive-mcp-api)
   :provides '(hive-mcp-my-addon-do-something)
   :init #'hive-mcp-my-addon--addon-init
   :async-init #'hive-mcp-my-addon--addon-async-init
   :shutdown #'hive-mcp-my-addon--addon-shutdown))

(provide 'hive-mcp-my-addon)
;;; hive-mcp-my-addon.el ends here
```

## Lifecycle Hooks

### `:init` - Synchronous Initialization

Runs immediately after the addon file is loaded. Use for:
- Loading required features
- Setting up keybindings
- Lightweight configuration

```elisp
(defun my-addon--addon-init ()
  (require 'hive-mcp-api nil t)
  (define-key some-map (kbd "C-c m") #'my-addon-command))
```

### `:async-init` - Asynchronous Initialization

Runs after `:init`, in the background. Use for:
- Starting external servers
- Long-running setup tasks
- Subprocess creation

**Important:** Should return a process object if starting a subprocess.

```elisp
(defun my-addon--addon-async-init ()
  "Start server asynchronously."
  (when my-addon-auto-start
    (setq my-addon--process
          (start-process "my-server" "*my-server*"
                         "npx" "my-server"))
    my-addon--process))  ; Return process for tracking
```

### `:shutdown` - Cleanup

Runs when addon is unloaded via `hive-mcp-addon-unload`. Use for:
- Stopping servers
- Canceling timers
- Saving state

```elisp
(defun my-addon--addon-shutdown ()
  (when (process-live-p my-addon--process)
    (kill-process my-addon--process))
  (setq my-addon--process nil))
```

## Process and Timer Tracking

The addon system can automatically track processes and timers for cleanup.

### Automatic Process Tracking

If `:async-init` returns a process, it's automatically tracked:

```elisp
(defun my-addon--addon-async-init ()
  (start-process ...))  ; Returned process is tracked
```

### Manual Timer Registration

For timers, register them explicitly:

```elisp
(let ((timer (run-with-timer 1 1 #'my-tick-function)))
  (hive-mcp-addon-register-timer 'my-addon timer))
```

## Using the MCP API

The `hive-mcp-api` module provides functions for interacting with MCP:

```elisp
;; Get current context (buffer, project, git info)
(hive-mcp-api-get-context)

;; Add to memory
(hive-mcp-api-memory-add "note" "Content here" '("tag1" "tag2"))

;; Query memory
(hive-mcp-api-memory-query "note" '("tag1") 10)
```

## Auto-Loading

### Trigger-Based Loading

Load addon when a feature becomes available:

```elisp
;; In hive-mcp-addons.el
(defcustom hive-mcp-addon-auto-load-list
  '((my-addon . my-trigger-package))
  ...)
```

### Always-Load

Load addon on every Emacs startup:

```elisp
;; In user config
(add-to-list 'hive-mcp-addon-always-load 'my-addon)
```

## Transient Menus

Add a transient menu for discoverability:

```elisp
;;;###autoload (autoload 'hive-mcp-my-addon-transient "hive-mcp-my-addon" nil t)
(transient-define-prefix hive-mcp-my-addon-transient ()
  "My addon menu."
  ["My Addon"
   ["Actions"
    ("a" "Action 1" my-addon-action-1)
    ("b" "Action 2" my-addon-action-2)]
   ["Settings"
    ("s" "Toggle something" my-addon-toggle)]])
```

## Best Practices

1. **Prefix all symbols** with `hive-mcp-ADDON-`
2. **Use `defcustom`** for user-configurable options
3. **Declare soft dependencies** with `declare-function`
4. **Return processes** from `:async-init` for tracking
5. **Handle errors gracefully** in lifecycle hooks
6. **Provide autoload cookies** for public functions
7. **Document with docstrings** for discoverability

## Example Addons

- **hive-mcp-cider.el** - CIDER/nREPL integration with async server startup
- **hive-mcp-vibe-kanban.el** - Task management with npx subprocess
- **hive-mcp-package-lint.el** - MELPA compliance tools

## Testing Your Addon

```elisp
;; Load addon
(hive-mcp-addon-load 'my-addon)

;; Check if loaded
(hive-mcp-addon-loaded-p 'my-addon)

;; Check if running (has active processes)
(hive-mcp-addon-running-p 'my-addon)

;; Unload addon
(hive-mcp-addon-unload 'my-addon)

;; View all addons
M-x hive-mcp-addon-info
```
