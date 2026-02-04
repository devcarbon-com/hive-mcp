# Swarm API Reference

Complete API reference for the hive-mcp swarm orchestration system.

## Elisp API

### Core Functions

#### `hive-mcp-swarm-spawn`

Spawn a new Claude slave instance.

```elisp
(hive-mcp-swarm-spawn NAME &key PRESETS CWD ROLE)
```

**Parameters:**
- `NAME` (string): Name for the slave, used in buffer naming
- `:presets` (list of strings): Preset names to apply
- `:cwd` (string): Working directory (default: project root)
- `:role` (string): Predefined role that maps to presets

**Returns:** `slave-id` (string)

**Example:**
```elisp
(hive-mcp-swarm-spawn "worker" :presets '("tdd" "solid") :cwd "/project")
;; => "swarm-worker-1704067890"
```

---

#### `hive-mcp-swarm-dispatch`

Send a prompt to a slave.

```elisp
(hive-mcp-swarm-dispatch SLAVE-ID PROMPT &key TIMEOUT PRIORITY CONTEXT)
```

**Parameters:**
- `SLAVE-ID` (string): Target slave identifier
- `PROMPT` (string): The prompt/task to send
- `:timeout` (integer): Timeout in milliseconds
- `:priority` (symbol): `'critical`, `'high`, `'normal`, `'low`
- `:context` (plist): Additional context data

**Returns:** `task-id` (string)

**Example:**
```elisp
(hive-mcp-swarm-dispatch "swarm-worker-123" "Run all tests" :timeout 60000)
;; => "task-worker-123-001"
```

---

#### `hive-mcp-swarm-collect`

Collect response from a dispatched task.

```elisp
(hive-mcp-swarm-collect TASK-ID &optional TIMEOUT-MS)
```

**Parameters:**
- `TASK-ID` (string): Task identifier from dispatch
- `TIMEOUT-MS` (integer): Max wait time (default: 5000)

**Returns:** Task plist with `:status`, `:result`, `:error`

**Example:**
```elisp
(hive-mcp-swarm-collect "task-worker-001" 10000)
;; => (:task-id "task-worker-001" :status completed :result "All tests passed")
```

---

#### `hive-mcp-swarm-broadcast`

Send a prompt to all active slaves.

```elisp
(hive-mcp-swarm-broadcast PROMPT &optional SLAVE-FILTER)
```

**Parameters:**
- `PROMPT` (string): Prompt to send
- `SLAVE-FILTER` (plist): Filter criteria (e.g., `(:role "tester")`)

**Returns:** List of task-ids

---

#### `hive-mcp-swarm-status`

Get swarm status.

```elisp
(hive-mcp-swarm-status &optional SLAVE-ID)
```

**Parameters:**
- `SLAVE-ID` (string, optional): Get specific slave status

**Returns:** Status plist

**Example:**
```elisp
(hive-mcp-swarm-status)
;; => (:session-id "session-20251229-a1b2"
;;     :status "active"
;;     :slaves (:total 3 :idle 2 :working 1 :error 0)
;;     :tasks (:total 5)
;;     :slaves-detail [...])
```

---

#### `hive-mcp-swarm-kill`

Kill a specific slave.

```elisp
(hive-mcp-swarm-kill SLAVE-ID)
```

---

#### `hive-mcp-swarm-kill-all`

Kill all slaves.

```elisp
(hive-mcp-swarm-kill-all)
```

---

### Preset Functions

#### `hive-mcp-swarm-list-presets`

List available preset names.

```elisp
(hive-mcp-swarm-list-presets)
;; => ("clarity" "ddd" "documenter" "fixer" ...)
```

---

#### `hive-mcp-swarm-reload-presets`

Reload presets from all directories.

```elisp
(hive-mcp-swarm-reload-presets)
```

---

#### `hive-mcp-swarm-add-custom-presets-dir`

Add a custom presets directory.

```elisp
(hive-mcp-swarm-add-custom-presets-dir DIR)
```

---

### API Functions (for MCP tools)

These return JSON-serializable data:

| Function | Description |
|----------|-------------|
| `hive-mcp-swarm-api-spawn` | Spawn and return slave-id |
| `hive-mcp-swarm-api-dispatch` | Dispatch and return task-id |
| `hive-mcp-swarm-api-status` | Get status as plist |
| `hive-mcp-swarm-api-collect` | Collect with structured result |
| `hive-mcp-swarm-api-list-presets` | List preset names |
| `hive-mcp-swarm-api-kill` | Kill slave |
| `hive-mcp-swarm-api-kill-all` | Kill all |

---

## MCP Tools

### swarm_spawn

Spawn a new Claude slave instance.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "name": {
      "type": "string",
      "description": "Name for the slave (used in buffer name)"
    },
    "presets": {
      "type": "array",
      "items": {"type": "string"},
      "description": "List of preset names to apply"
    },
    "cwd": {
      "type": "string",
      "description": "Working directory for the slave"
    },
    "role": {
      "type": "string",
      "description": "Predefined role (tester, reviewer, etc.)"
    }
  },
  "required": ["name"]
}
```

**Returns:** JSON with `slave_id`

---

### swarm_dispatch

Send a prompt to a slave.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "slave_id": {
      "type": "string",
      "description": "ID of the slave to send prompt to"
    },
    "prompt": {
      "type": "string",
      "description": "The prompt/task to send"
    },
    "timeout_ms": {
      "type": "integer",
      "description": "Optional timeout in milliseconds"
    }
  },
  "required": ["slave_id", "prompt"]
}
```

**Returns:** JSON with `task_id`

---

### swarm_status

Get swarm status.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "slave_id": {
      "type": "string",
      "description": "Optional: get status of specific slave"
    }
  }
}
```

**Returns:** JSON status object

---

### swarm_collect

Collect response from a task.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "task_id": {
      "type": "string",
      "description": "ID of the task to collect"
    },
    "timeout_ms": {
      "type": "integer",
      "description": "How long to wait (default: 5000)"
    }
  },
  "required": ["task_id"]
}
```

**Returns:** JSON with `status`, `result`, `error`

---

### swarm_list_presets

List available presets.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {}
}
```

**Returns:** JSON array of preset names

---

### swarm_kill

Kill a slave or all slaves.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "slave_id": {
      "type": "string",
      "description": "ID of slave to kill, or \"all\""
    }
  },
  "required": ["slave_id"]
}
```

---

### swarm_broadcast

Broadcast prompt to all slaves.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "prompt": {
      "type": "string",
      "description": "Prompt to broadcast"
    }
  },
  "required": ["prompt"]
}
```

**Returns:** JSON array of task_ids

---

## Data Structures

### Slave State

```elisp
(:slave-id "swarm-name-1234567"
 :name "name"
 :role "tester"
 :presets ("tdd" "solid")
 :status idle         ; starting | idle | working | error
 :buffer #<buffer>
 :cwd "/path/to/project"
 :current-task nil
 :task-queue ()
 :tasks-completed 5
 :tasks-failed 0
 :spawned-at "2025-12-29T10:30:00Z"
 :last-activity "2025-12-29T10:35:00Z")
```

### Task State

```elisp
(:task-id "task-name-001"
 :slave-id "swarm-name-1234567"
 :prompt "Run tests"
 :status dispatched   ; dispatched | completed | timeout | error
 :priority normal
 :timeout 300000
 :context nil
 :dispatched-at "2025-12-29T10:30:00Z"
 :completed-at nil
 :result nil
 :error nil)
```

### Swarm Status

```elisp
(:session-id "session-20251229-a1b2"
 :status "active"
 :slaves (:total 3 :idle 2 :working 1 :error 0)
 :tasks (:total 5)
 :slaves-detail
 ((:slave-id "swarm-a-123" :name "a" :status idle :current-task nil :tasks-completed 2)
  (:slave-id "swarm-b-456" :name "b" :status working :current-task "task-b-001" :tasks-completed 1)))
```

---

## Customization Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hive-mcp-swarm-terminal` | `'vterm` | Terminal emulator (`'vterm` or `'eat`) |
| `hive-mcp-swarm-presets-dir` | `"presets/"` | Built-in presets directory |
| `hive-mcp-swarm-custom-presets-dirs` | `nil` | List of custom preset directories |
| `hive-mcp-swarm-claude-command` | `"claude"` | Claude CLI command |
| `hive-mcp-swarm-max-slaves` | `5` | Maximum concurrent slaves |
| `hive-mcp-swarm-max-depth` | `2` | Maximum recursion depth |
| `hive-mcp-swarm-default-timeout` | `300000` | Default timeout (ms) |
| `hive-mcp-swarm-prompt-marker` | `"❯"` | Prompt ready marker |
| `hive-mcp-swarm-buffer-prefix` | `"*swarm-"` | Buffer name prefix |

---

## Environment Variables

Set automatically for slave processes:

| Variable | Description |
|----------|-------------|
| `CLAUDE_SWARM_DEPTH` | Current recursion depth |
| `CLAUDE_SWARM_MASTER` | Master session ID |
| `CLAUDE_SWARM_SLAVE_ID` | This slave's ID |
