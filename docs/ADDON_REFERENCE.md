# Addon Reference

Complete documentation for all hive-mcp addons.

## chroma

**Purpose**: Semantic memory search via Chroma vector database and Ollama embeddings.

**Features**:
- Docker-compose management for Chroma container
- Automatic Ollama embedding provider configuration
- Semantic similarity search across project memory
- Fallback to local text search when Chroma unavailable
- Health monitoring and status display

**Config**:
- `hive-mcp-chroma-host` - Chroma server host (default: "localhost")
- `hive-mcp-chroma-port` - Chroma server port (default: 8000)
- `hive-mcp-chroma-auto-start` - Auto-start container on init (default: t)
- `hive-mcp-chroma-embedding-provider` - 'ollama, 'mock, or 'none
- `hive-mcp-chroma-ollama-model` - Model for embeddings (default: "nomic-embed-text")

**Usage**:
```elisp
(require 'hive-mcp-chroma)
(hive-mcp-chroma-mode 1)

;; Semantic search
(hive-mcp-chroma-search "authentication patterns")

;; Check status
M-x hive-mcp-chroma-status

;; Transient menu
M-x hive-mcp-chroma-transient
```

---

## cider

**Purpose**: CIDER integration for Clojure development with async nREPL and multi-session support.

**Features**:
- Async nREPL server startup (non-blocking)
- Auto-connect when nREPL becomes available
- Named sessions for parallel agent work
- Silent and explicit evaluation modes
- Memory integration for REPL history

**Config**:
- `hive-mcp-cider-auto-start-nrepl` - Auto-start nREPL on mode enable
- `hive-mcp-cider-nrepl-timeout` - Connection timeout in seconds
- `hive-mcp-cider-default-deps-aliases` - Default aliases for deps.edn

**Usage**:
```elisp
(require 'hive-mcp-cider)

;; Spawn isolated session for parallel work
(hive-mcp-cider-spawn-session "agent-1")

;; Evaluate in specific session
(hive-mcp-cider-eval-in-session "agent-1" "(+ 1 2)")

;; List all sessions
(hive-mcp-cider-list-sessions)
```

---

## magit

**Purpose**: Git operations via Magit with shell command fallback.

**Features**:
- Comprehensive repo status (staged, unstaged, untracked, stashes)
- Branch management (list, create, checkout)
- Non-interactive staging and commits for MCP use
- Diff viewing (staged, unstaged, all)
- Remote operations (fetch, pull, push)
- Automatic Magit refresh after operations

**Config**:
- `hive-mcp-magit-log-count` - Default commits in log (default: 10)
- `hive-mcp-magit-diff-context-lines` - Context lines in diffs (default: 3)
- `hive-mcp-magit-prefer-magit` - Use Magit functions when available (default: t)

**Usage**:
```elisp
(require 'hive-mcp-magit)

;; Get full status
(hive-mcp-magit-api-status)
;; => (:branch "main" :staged ("file.el") :unstaged () ...)

;; Stage and commit
(hive-mcp-magit-api-stage 'all)
(hive-mcp-magit-api-commit "feat: add feature")

;; Transient menu
M-x hive-mcp-magit-transient
```

---

## swarm

**Purpose**: Multi-agent orchestration for parallel Claude Code instances.

**Features**:
- Spawn slave Claude instances in vterm/eat buffers
- Preset system (markdown system prompts)
- Task dispatch and collection
- Broadcast to all slaves
- Recursion depth limits (prevent runaway spawning)
- Rate limiting for spawn protection
- Custom preset directories

**Config**:
- `hive-mcp-swarm-terminal` - 'vterm (recommended) or 'eat
- `hive-mcp-swarm-max-slaves` - Maximum concurrent slaves (default: 5)
- `hive-mcp-swarm-max-depth` - Recursion depth limit (default: 3)
- `hive-mcp-swarm-presets-dir` - Built-in presets directory
- `hive-mcp-swarm-custom-presets-dirs` - Additional preset directories

**Usage**:
```elisp
(require 'hive-mcp-swarm)
(hive-mcp-swarm-mode 1)

;; Spawn with presets
(hive-mcp-swarm-spawn "tester" :presets '("tdd" "clarity"))

;; Dispatch task
(hive-mcp-swarm-dispatch "swarm-tester-xxx" "Run all tests")

;; Collect result
(hive-mcp-swarm-collect "task-tester-xxx-001" 30000)

;; Broadcast to all
(hive-mcp-swarm-broadcast "Status report")
```

---

## projectile

**Purpose**: Project management integration via Projectile.

**Features**:
- Project info (name, root, type, file count)
- Extended type detection (npm, cargo, go-mod, deps.edn, etc.)
- File listing with glob pattern filtering
- File search by name
- Project-wide grep (ripgrep preferred)
- Recent files in project

**Config**:
- `hive-mcp-projectile-max-files` - Max files in listings (default: 1000)
- `hive-mcp-projectile-max-search-results` - Max search results (default: 100)
- `hive-mcp-projectile-use-ripgrep` - Prefer rg over grep (default: t)

**Usage**:
```elisp
(require 'hive-mcp-projectile)

;; Get project info
(hive-mcp-projectile-api-project-info)
;; => (:name "hive-mcp" :root "/path/to" :type "clojure-deps" ...)

;; List files with pattern
(hive-mcp-projectile-api-project-files "*.el")

;; Search project
(hive-mcp-projectile-api-search "defun.*api")
```

---

## docs

**Purpose**: Expose Emacs's built-in documentation to MCP.

**Features**:
- Describe functions (signature, docstring, type, source)
- Describe variables (value, docstring, custom status)
- Apropos search with type filtering
- List package functions by prefix
- Find keybindings for commands
- Extract package commentary sections

**Config**:
- `hive-mcp-docs-max-results` - Max apropos results (default: 50)
- `hive-mcp-docs-include-source-location` - Include file paths (default: t)

**Usage**:
```elisp
(hive-mcp-docs-describe-function "mapcar")
;; => (:name "mapcar" :type "built-in function" :signature "(FN SEQUENCE)" ...)

(hive-mcp-docs-apropos "json" "function")
;; => (:pattern "json" :total-matches 42 :symbols (...))
```

---

## presentation

**Purpose**: Streamline org-mode presentation creation for Beamer and Reveal.js.

**Features**:
- Pre-configured templates for Beamer (PDF) and Reveal.js (HTML)
- Slide scaffolding: code, image, quote, two-column, iframe
- Export and preview commands
- MCP memory integration for slide snippets
- Transient UI menu

**Config**:
- `hive-mcp-presentation-default-format` - 'beamer or 'revealjs
- `hive-mcp-presentation-beamer-theme` - Beamer color theme
- `hive-mcp-presentation-reveal-theme` - Reveal.js theme
- `hive-mcp-presentation-author` / `hive-mcp-presentation-email`

**Usage**:
```elisp
;; Create new presentation
M-x hive-mcp-presentation-create

;; Insert slides
M-x hive-mcp-presentation-insert-code-slide
M-x hive-mcp-presentation-insert-quote-slide

;; Export and preview
M-x hive-mcp-presentation-refresh

;; Transient menu
M-x hive-mcp-presentation-transient
```

---

## melpazoid

**Purpose**: MELPA submission testing via melpazoid Docker integration.

**Features**:
- Full melpazoid test suite (package-lint, byte-compile, checkdoc)
- Multiple speed modes: full Docker, cached Docker, local Python
- Auto-detect MELPA recipes from recipes/ directory
- Parse and display structured results
- Save results to MCP memory

**Config**:
- `hive-mcp-melpazoid-path` - Path to melpazoid repo (auto-detected)
- `hive-mcp-melpazoid-fast-mode` - nil (full), 'cached, or 'local
- `hive-mcp-melpazoid-timeout` - Timeout in seconds (default: 300)
- `hive-mcp-melpazoid-save-results` - Auto-save to MCP memory (default: t)

**Usage**:
```elisp
;; Run on current project
M-x hive-mcp-melpazoid-run-current-project

;; Fast mode (no Docker)
M-x hive-mcp-melpazoid-run-fast

;; View results
M-x hive-mcp-melpazoid-show-results

;; Transient menu
M-x hive-mcp-melpazoid-transient
```

---

## org-kanban

**Purpose**: Native Clojure org-mode parser for kanban boards.

**Features**:
- Parse org files to JSON structure without elisp
- Query headlines by ID, status, or type
- Move tasks between status columns
- Render visual kanban boards (terminal ASCII or org-mode)
- Statistics and progress tracking

**Usage**:
```elisp
;; Parse org file
(org_clj_parse "/path/to/kanban.org")

;; Get kanban status
(org_kanban_native_status "/path/to/kanban.org")

;; Move task
(org_kanban_native_move "/path/to/kanban.org" "task-id" "DONE")

;; Render board
(org_kanban_render "/path/to/kanban.org" :format "terminal")
```

---

## vibe-kanban

**Purpose**: Cloud task management via vibe-kanban server.

**Features**:
- NPX subprocess management for vibe-kanban server
- Task CRUD operations
- Status column management
- Sync with org-mode kanban
- Roadmap and milestone views

**Usage**:
```elisp
;; Get kanban status
(mcp_kanban_status)

;; Create task
(mcp_kanban_create_task :title "New feature" :description "Details...")

;; Move task
(mcp_kanban_move_task :task-id "xxx" :new-status "done")

;; Sync backends
(mcp_kanban_sync)
```

---

## package-lint

**Purpose**: MELPA compliance checking via package-lint.

**Features**:
- Run package-lint on current buffer
- Check all project elisp files
- Format results for MCP consumption
- Integration with memory for tracking issues

---

## claude-code

**Purpose**: Integration with claude-code.el for Claude CLI.

**Features**:
- Context injection for Claude conversations
- Memory sharing between MCP and claude-code.el

---

## cci (claude-code-ide)

**Purpose**: Swarm orchestration via claude-code-ide.el with hivemind completion tracking.

**Features**:
- Structured session management (no terminal scraping)
- Reliable prompt dispatch via claude-code-ide API
- Task completion tracking via hivemind coordinator
- Auto-sync from hivemind for completion status
- Integration with hive-mcp-swarm presets

**Benefits over vterm-based swarm**:
- No terminal timing issues or output parsing
- Structured JSON-RPC communication
- Lings report completion via hivemind_shout (not terminal markers)
- Reliable multi-agent coordination

**Config**:
- `hive-mcp-cci-default-timeout` - Task timeout in ms (default: 300000)
- `hive-mcp-cci-max-lings` - Maximum concurrent lings (default: 10)
- `hive-mcp-cci-hivemind-poll-interval` - Hivemind sync interval (default: 5s)
- `hive-mcp-cci-auto-sync` - Auto-sync from hivemind (default: t)

**Usage**:
```elisp
(require 'hive-mcp-cci)
(hive-mcp-cci-mode 1)

;; Spawn a ling
(hive-mcp-cci-spawn "worker" :presets '("hivemind" "tdd"))
;; => "ling-worker-123456"

;; Dispatch task (completion via hivemind)
(hive-mcp-cci-dispatch "ling-worker-123456" "Run all tests")
;; => "task-worker-123456-001"

;; Check status
(hive-mcp-cci-status)
;; => (:backend "claude-code-ide" :completion-mechanism "hivemind" ...)

;; Manual sync from hivemind
(hive-mcp-cci-sync-from-hivemind)
```

See [SWARM_BACKENDS.md](SWARM_BACKENDS.md) for detailed comparison of swarm backends.

---

## org-ai

**Purpose**: Integration with org-ai for AI conversations in org-mode.

**Features**:
- Context sharing with org-ai conversations
- Memory integration for conversation history

---

## addon-template

**Purpose**: Template for creating new addons.

**Features**:
- Complete addon skeleton with lifecycle hooks
- Example customization group
- Minor mode template
- Registration boilerplate

**Usage**:
```bash
cp elisp/addons/hive-mcp-addon-template.el elisp/addons/hive-mcp-my-addon.el
# Edit and customize
```
