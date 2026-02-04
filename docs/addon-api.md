# Addon API Reference

Complete reference for hive-mcp addon development.

## Registration

### `hive-mcp-addon-register`

Register an addon with the addon system.

```elisp
(hive-mcp-addon-register ADDON &rest PROPS)
```

**ADDON**: Symbol identifying the addon (e.g., `'cider`, `'vibe-kanban`)

**PROPS**: Property list with the following keys:

| Key | Type | Required | Description |
|-----|------|----------|-------------|
| `:version` | string | No | Version string (e.g., "1.0.0") |
| `:description` | string | No | One-line description |
| `:requires` | list | No | Required features/packages |
| `:provides` | list | No | Provided features/commands |
| `:init` | function | No | Synchronous init hook |
| `:async-init` | function | No | Asynchronous init hook |
| `:shutdown` | function | No | Shutdown/cleanup hook |

**Example:**

```elisp
(hive-mcp-addon-register
 'my-addon
 :version "1.0.0"
 :description "My awesome addon"
 :requires '(hive-mcp-api some-package)
 :provides '(my-addon-mode my-addon-transient)
 :init #'my-addon--init
 :async-init #'my-addon--async-init
 :shutdown #'my-addon--shutdown)
```

## Lifecycle Hooks

### `:init`

**Signature:** `(defun my-addon--init ())`

Called synchronously immediately after the addon file is loaded.

- Runs before `:async-init`
- Should complete quickly (< 100ms)
- Errors are caught and logged

**Use cases:**
- `(require 'dependency nil t)`
- Setup keybindings
- Register hooks

### `:async-init`

**Signature:** `(defun my-addon--async-init ())`

Called after `:init`. Should not block Emacs.

- **Must return** a process object if starting a subprocess
- Returned process is automatically tracked for cleanup
- Errors are caught and logged

**Use cases:**
- Start external servers
- Spawn background processes
- Long-running initialization

**Example:**

```elisp
(defun my-addon--async-init ()
  (when my-addon-auto-start
    (start-process "server" "*server*" "my-command")))
```

### `:shutdown`

**Signature:** `(defun my-addon--shutdown ())`

Called when addon is unloaded via `hive-mcp-addon-unload`.

- Cleanup is automatic for tracked processes/timers
- Use for additional cleanup (save state, remove hooks)
- Errors are caught and logged

**Example:**

```elisp
(defun my-addon--shutdown ()
  (save-some-state)
  (remove-hook 'some-hook #'my-function))
```

## Load/Unload Functions

### `hive-mcp-addon-load`

Load an addon.

```elisp
(hive-mcp-addon-load ADDON)
```

- Loads the addon file from addon directories
- Calls `:init` then `:async-init`
- Returns `t` if successful, `nil` otherwise

**Interactive:** `M-x hive-mcp-addon-load`

### `hive-mcp-addon-unload`

Unload an addon.

```elisp
(hive-mcp-addon-unload ADDON)
```

- Calls `:shutdown` hook
- Kills tracked processes
- Cancels tracked timers
- Marks addon as unloaded

**Interactive:** `M-x hive-mcp-addon-unload`

### `hive-mcp-addon-restart`

Restart an addon.

```elisp
(hive-mcp-addon-restart ADDON)
```

Equivalent to unload + load.

**Interactive:** `M-x hive-mcp-addon-restart`

## Query Functions

### `hive-mcp-addon-loaded-p`

Check if addon is loaded.

```elisp
(hive-mcp-addon-loaded-p ADDON) ; => t or nil
```

### `hive-mcp-addon-running-p`

Check if addon has active processes or timers.

```elisp
(hive-mcp-addon-running-p ADDON) ; => t or nil
```

### `hive-mcp-addon-available-p`

Check if addon file exists.

```elisp
(hive-mcp-addon-available-p ADDON) ; => t or nil
```

### `hive-mcp-addon-list-available`

List all available addons.

```elisp
(hive-mcp-addon-list-available) ; => (cider vibe-kanban ...)
```

### `hive-mcp-addon-list-loaded`

List currently loaded addons.

```elisp
(hive-mcp-addon-list-loaded) ; => (cider ...)
```

## Process/Timer Management

### `hive-mcp-addon-register-process`

Register a process for lifecycle tracking.

```elisp
(hive-mcp-addon-register-process ADDON PROCESS)
```

Processes are automatically killed on addon unload.

**Note:** Returning a process from `:async-init` auto-registers it.

### `hive-mcp-addon-register-timer`

Register a timer for lifecycle tracking.

```elisp
(hive-mcp-addon-register-timer ADDON TIMER)
```

Timers are automatically cancelled on addon unload.

**Example:**

```elisp
(let ((timer (run-with-timer 1 1 #'my-tick)))
  (hive-mcp-addon-register-timer 'my-addon timer))
```

### `hive-mcp-addon-unregister-timer`

Unregister a timer.

```elisp
(hive-mcp-addon-unregister-timer ADDON TIMER)
```

## Auto-Loading

### `hive-mcp-addon-auto-load-list`

Customizable alist of `(ADDON . TRIGGER-FEATURE)` pairs.

```elisp
(setq hive-mcp-addon-auto-load-list
      '((cider . cider)
        (org-ai . org-ai)
        (my-addon . my-package)))
```

When `TRIGGER-FEATURE` is loaded, `ADDON` is automatically loaded.

### `hive-mcp-addon-always-load`

List of addons to always load on startup.

```elisp
(setq hive-mcp-addon-always-load '(cider vibe-kanban))
```

### `hive-mcp-addons-auto-load`

Enable auto-loading based on `hive-mcp-addon-auto-load-list`.

```elisp
(hive-mcp-addons-auto-load)
```

### `hive-mcp-addons-load-always`

Load all addons in `hive-mcp-addon-always-load`.

```elisp
(hive-mcp-addons-load-always)
```

### `hive-mcp-addons-setup`

Main entry point - loads always-load addons and enables auto-loading.

```elisp
(hive-mcp-addons-setup)
```

Called automatically by `hive-mcp-initialize` when `hive-mcp-setup-addons` is non-nil.

## MCP API Functions

Available via `(require 'hive-mcp-api)`:

### `hive-mcp-api-get-context`

Get current context (buffer, project, git status).

```elisp
(hive-mcp-api-get-context)
;; => (:buffer (:name "foo.el" :mode "emacs-lisp-mode")
;;     :project (:name "my-project" :root "/path/to/project")
;;     :git (:branch "main" :dirty t))
```

### `hive-mcp-api-memory-add`

Add entry to persistent memory.

```elisp
(hive-mcp-api-memory-add TYPE CONTENT TAGS)
```

- **TYPE**: "note", "snippet", "decision", etc.
- **CONTENT**: String content
- **TAGS**: List of tag strings

**Example:**

```elisp
(hive-mcp-api-memory-add "snippet" "(defn foo [])" '("clojure" "function"))
```

### `hive-mcp-api-memory-query`

Query persistent memory.

```elisp
(hive-mcp-api-memory-query TYPE TAGS LIMIT)
```

Returns vector of matching entries.

## Customization Variables

### `hive-mcp-addon-directories`

List of directories to search for addons.

```elisp
(add-to-list 'hive-mcp-addon-directories "~/my-addons")
```

Default includes `elisp/addons/` in the hive-mcp installation.

## Info Command

### `hive-mcp-addon-info`

Display buffer with addon information.

```elisp
M-x hive-mcp-addon-info
```

Shows:
- Available addons
- Loaded status
- Running status (has active processes)
- Lifecycle hooks defined
- Addon directories
