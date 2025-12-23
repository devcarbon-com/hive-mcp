# emacs-mcp

MCP (Model Context Protocol) server that allows Claude to interact with a running Emacs instance via `emacsclient`.

## Features

- **eval_elisp** - Execute arbitrary Emacs Lisp code
- **emacs_status** - Check if Emacs server is running
- **list_buffers** - List all open buffers
- **get_buffer_content** - Read content from a buffer
- **current_buffer** - Get current buffer name and file
- **switch_to_buffer** - Switch to a specific buffer
- **find_file** - Open a file in Emacs
- **save_buffer** - Save the current buffer
- **goto_line** - Move cursor to a line number
- **insert_text** - Insert text at cursor
- **project_root** - Get current project root
- **recent_files** - Get recently opened files

## Prerequisites

1. **Emacs** with server mode enabled:
   ```elisp
   (server-start)
   ```

2. **Clojure CLI** (deps.edn)

3. **emacsclient** in your PATH

## Installation

### 1. Clone the repository

```bash
git clone https://github.com/yourusername/emacs-mcp.git
cd emacs-mcp
```

### 2. Add to Claude MCP config

Add to `~/.claude/mcp.json`:

```json
{
  "mcpServers": {
    "emacs": {
      "command": "/path/to/emacs-mcp/start-mcp.sh",
      "args": []
    }
  }
}
```

Or for Claude Desktop, add to `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "emacs": {
      "command": "/bin/bash",
      "args": ["-c", "cd /path/to/emacs-mcp && clojure -X:mcp"]
    }
  }
}
```

## Development

### Start nREPL for development

```bash
clojure -M:nrepl
```

### Run tests

```bash
clojure -M:test
```

### Run MCP server directly

```bash
clojure -X:mcp
```

## Architecture

```
┌─────────────┐     MCP/STDIO      ┌─────────────┐
│   Claude    │ ◄────────────────► │  emacs-mcp  │
│   (AI)      │                    │  (Clojure)  │
└─────────────┘                    └──────┬──────┘
                                          │
                                          │ emacsclient --eval
                                          ▼
                                   ┌─────────────┐
                                   │   Emacs     │
                                   │  (daemon)   │
                                   └─────────────┘
```

## License

MIT
