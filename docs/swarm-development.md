# Swarm Orchestration Development Guide

A comprehensive guide for using and extending the Claude swarm orchestration system in hive-mcp.

## Overview

The swarm system enables a **Master Claude** to spawn and control multiple **Slave Claude** instances for parallel task execution. This is useful for:

- Parallel test execution
- Multi-file refactoring
- Code review with multiple perspectives
- Research tasks across different files/domains
- Any task that benefits from parallel processing

## Architecture

```
Master Claude (you)
       │
       │ MCP tools (swarm_spawn, swarm_dispatch, etc.)
       ↓
┌─────────────────────────────────────────────────┐
│          hive-mcp-swarm.el                     │
│  - Session management                           │
│  - Preset loading                               │
│  - Task queue                                   │
└─────────────────────────────────────────────────┘
       │
       │ vterm-send-string / eat-term-send-string
       ↓
┌─────────────────────────────────────────────────┐
│              Slave Terminals                    │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐        │
│  │ slave-1 │  │ slave-2 │  │ slave-3 │  ...   │
│  │ (vterm) │  │ (vterm) │  │ (vterm) │        │
│  │ claude  │  │ claude  │  │ claude  │        │
│  └─────────┘  └─────────┘  └─────────┘        │
└─────────────────────────────────────────────────┘
```

## Quick Start

### 1. Enable the Addon

```elisp
;; In your Emacs config or interactively
(require 'hive-mcp-swarm)
(hive-mcp-swarm-mode 1)
```

### 2. Spawn a Slave

```elisp
;; Basic spawn
(hive-mcp-swarm-spawn "tester")

;; Spawn with presets
(hive-mcp-swarm-spawn "reviewer" :presets '("solid" "clarity"))

;; Spawn with a predefined role
(hive-mcp-swarm-spawn "my-tester" :role "tester")
```

### 3. Dispatch a Task

```elisp
(hive-mcp-swarm-dispatch "swarm-tester-1234567" "Run all tests in src/")
```

### 4. Collect the Response

```elisp
(hive-mcp-swarm-collect "task-tester-001" 30000)  ;; 30 second timeout
```

## Presets System

Presets are markdown files that configure slave behavior with specialized system prompts.

### Built-in Presets

| Preset | Purpose |
|--------|---------|
| `clarity` | CLARITY framework (Compose, Layer, Architectural, Represent, Input, Telemetry, Yield) |
| `solid` | SOLID principles for OOP/module design |
| `ddd` | Domain-Driven Design patterns |
| `tdd` | Test-Driven Development workflow |
| `tester` | Test execution specialist |
| `reviewer` | Code review specialist |
| `documenter` | Documentation writer |
| `refactorer` | Refactoring specialist |
| `researcher` | Codebase research |
| `fixer` | Bug fixing specialist |
| `minimal` | No special constraints |

### Predefined Roles

Roles map to preset combinations:

```elisp
;; These are equivalent:
(hive-mcp-swarm-spawn "test" :role "reviewer")
(hive-mcp-swarm-spawn "test" :presets '("reviewer" "solid" "clarity"))
```

| Role | Presets Applied |
|------|-----------------|
| `tester` | tester, tdd |
| `reviewer` | reviewer, solid, clarity |
| `documenter` | documenter |
| `refactorer` | refactorer, solid, clarity |
| `researcher` | researcher |
| `fixer` | fixer, tdd |
| `clarity-dev` | clarity, solid, ddd, tdd |

### Custom Presets

1. Create a directory for your presets
2. Add markdown files (`.md` extension)
3. Register the directory:

```elisp
(add-to-list 'hive-mcp-swarm-custom-presets-dirs "~/my-presets/")
(hive-mcp-swarm-reload-presets)
```

Preset format:

```markdown
# My Custom Preset

You are a specialist in [DOMAIN].

## Guidelines

- Guideline 1
- Guideline 2

## Constraints

- Must always do X
- Never do Y
```

## MCP Tools Reference

Use these from the Master Claude:

### swarm_spawn

```json
{
  "name": "my-worker",
  "presets": ["tdd", "solid"],
  "cwd": "/path/to/project",
  "role": "tester"
}
```

### swarm_dispatch

```json
{
  "slave_id": "swarm-my-worker-1234567",
  "prompt": "Run all unit tests",
  "timeout_ms": 60000
}
```

### swarm_collect

```json
{
  "task_id": "task-my-worker-001",
  "timeout_ms": 30000
}
```

### swarm_status

```json
{}  // Returns all slaves
{"slave_id": "swarm-xxx"}  // Specific slave
```

### swarm_broadcast

```json
{
  "prompt": "Report your current status"
}
```

### swarm_kill

```json
{"slave_id": "swarm-xxx"}  // Kill specific
{"slave_id": "all"}        // Kill all
```

## Safety Features

### Recursion Prevention

Slaves cannot spawn their own slaves beyond a configurable depth:

```elisp
(setq hive-mcp-swarm-max-depth 2)  ;; Default
```

Environment variable `CLAUDE_SWARM_DEPTH` tracks current depth.

### Slave Limits

```elisp
(setq hive-mcp-swarm-max-slaves 5)  ;; Maximum concurrent slaves
```

### Timeouts

```elisp
(setq hive-mcp-swarm-default-timeout 300000)  ;; 5 minutes default
```

## Example Workflows

### Parallel Test Runner

```elisp
;; Spawn test slaves for different test suites
(let ((slaves (list
               (hive-mcp-swarm-spawn "unit" :role "tester")
               (hive-mcp-swarm-spawn "integration" :role "tester")
               (hive-mcp-swarm-spawn "e2e" :role "tester"))))

  ;; Dispatch tests
  (hive-mcp-swarm-dispatch (nth 0 slaves) "Run unit tests")
  (hive-mcp-swarm-dispatch (nth 1 slaves) "Run integration tests")
  (hive-mcp-swarm-dispatch (nth 2 slaves) "Run e2e tests")

  ;; Collect results...
  )
```

### Code Review Swarm

```elisp
;; Multiple reviewers with different focuses
(hive-mcp-swarm-spawn "security" :presets '("reviewer" "security"))
(hive-mcp-swarm-spawn "perf" :presets '("reviewer" "performance"))
(hive-mcp-swarm-spawn "style" :presets '("reviewer" "solid"))

;; Broadcast the review request
(hive-mcp-swarm-broadcast "Review the changes in PR #123")
```

## Troubleshooting

### Slave Not Starting

1. Check if vterm is installed: `(require 'vterm)`
2. Verify Claude CLI is in PATH: `which claude`
3. Check buffer exists: `(get-buffer "*swarm-name*")`

### Collection Timing Out

1. Increase timeout: `:timeout 60000`
2. Check slave buffer for errors
3. Verify Claude responded (look for `●` marker)

### Presets Not Loading

1. Check directory exists: `hive-mcp-swarm-presets-dir`
2. Reload: `M-x hive-mcp-swarm-reload-presets`
3. List loaded: `M-x hive-mcp-swarm-list-presets`

## Keybindings

With `hive-mcp-swarm-mode` enabled:

| Key | Action |
|-----|--------|
| `C-c s` | Open swarm transient menu |

Transient menu provides quick access to all swarm commands.
