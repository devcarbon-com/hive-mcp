# End-of-Session Wrap-up (Memory-Integrated)

Session wrap-up that stores context to project memory with appropriate duration categories.

## IMPORTANT: Directory Scoping

**All MCP tools that accept a `directory` parameter MUST receive your current working directory** to ensure operations target YOUR project, not the MCP server's directory.

Get your working directory from:
- Your prompt path (e.g., `~/PP/funeraria/sisf-web$`)
- Run `pwd` in bash

Pass it to: `mcp_memory_add`, `mcp_mem_kanban_*`, `magit_*` tools.

## Instructions

### 1. Document Progress to Memory

Store session accomplishments in project memory with appropriate durations:

**a) Session accomplishments (short-term memory):**
```
mcp__emacs-mcp__mcp_memory_add
  type: "note"
  content: "Session YYYY-MM-DD: [accomplishments summary]"
  tags: ["session-log", "progress"]
  directory: "/path/to/your/project"
```

**b) Important decisions (long-term/permanent memory):**
For architectural or significant decisions made:
```
mcp__emacs-mcp__mcp_memory_add
  type: "decision"
  content: "Decision: [what was decided and why]"
  tags: ["architecture", "decision"]
  directory: "/path/to/your/project"
```
Then promote to longer duration if significant:
```
mcp__emacs-mcp__mcp_memory_promote
  id: [decision_id]
```

**c) Code conventions discovered (long-term memory):**
```
mcp__emacs-mcp__mcp_memory_add
  type: "convention"
  content: "[convention description]"
  tags: ["code-style", "pattern"]
  directory: "/path/to/your/project"
```

**d) Useful code snippets (long-term memory):**
```
mcp__emacs-mcp__mcp_memory_add
  type: "snippet"
  content: "[code snippet with context]"
  tags: ["reusable", "category"]
  directory: "/path/to/your/project"
```

### 2. Sync with Kanbans

**a) Sync In-Memory Kanban first:**

IMPORTANT: Pass `directory` to kanban tools to ensure correct project scoping.

```
mcp__emacs-mcp__mcp_mem_kanban_stats directory:"/path/to/your/project"
mcp__emacs-mcp__mcp_mem_kanban_list status:"doing" directory:"/path/to/your/project"
```

For each DOING task, ask user:
- "Task X is DOING. Complete it?" → move to done (auto-deletes)
- "Keep as DOING?" → leave as-is
- "Move back to TODO?" → move to todo

Include kanban stats in session summary.

**b) Check dynamic-kanban status:**
```
mcp__dynamic-kanban__kanban_status
```

**c) For each task worked on today:**
- Move completed tasks to `done`
- Update in-progress tasks with notes
- Add any new tasks discovered during implementation

**d) Check vibe-kanban for project-level tasks:**
```
mcp__vibe_kanban__list_tasks
```

### 3. Memory Maintenance

**a) Cleanup expired entries:**
```
mcp__emacs-mcp__mcp_memory_cleanup_expired
```

**b) Check entries expiring soon (optional review):**
```
mcp__emacs-mcp__mcp_memory_expiring_soon
  days: 7
```
If important entries are expiring, promote them:
```
mcp__emacs-mcp__mcp_memory_promote
  id: [entry_id]
```

### 4. Create Session Summary

Store session summary in memory as a note with short-term duration:
```
mcp__emacs-mcp__mcp_memory_add
  type: "note"
  content: |
    ## Session Summary: YYYY-MM-DD

    ### Completed
    - Task 1: Brief description
    - Task 2: Brief description

    ### In Progress
    - Task: Current state, next steps

    ### Decisions Made
    - Decision 1: Rationale

    ### Files Changed
    - `path/to/file` - What changed

    ### Next Session
    - Recommended starting point
    - Priority tasks
  tags: ["session-summary", "wrap"]
  directory: "/path/to/your/project"
```

### 5. Git Status Check

**Use magit MCP tools with your working directory:**

IMPORTANT: Always pass `directory` parameter to ensure git operations target YOUR project, not the MCP server's directory. Use your current working directory (from your prompt path or run `pwd`).

```
mcp__emacs-mcp__magit_status directory:"/path/to/your/project"
mcp__emacs-mcp__magit_branches directory:"/path/to/your/project"
mcp__emacs-mcp__magit_log directory:"/path/to/your/project"
mcp__emacs-mcp__magit_feature_branches directory:"/path/to/your/project"
```

If there are unmerged branches, ask:
- "Do you want me to run `/ship` to merge these to staging?"
- "Or `/ship-pr` to create PRs for review?"

### 6. Write SESSION_CONTEXT.json

Write context file for `/catchup` to read:
```json
{
  "date": "YYYY-MM-DD",
  "focus_area": "e.g., memory duration feature",
  "branches_created": ["feature/xxx"],
  "key_files": ["path/to/file.clj"],
  "completed_tasks": ["Task 1", "Task 2"],
  "in_progress_tasks": ["Task with state"],
  "blocking_issues": [],
  "next_actions": ["Action 1", "Action 2"],
  "memory_ids": {
    "session_summary": "id-of-session-summary",
    "decisions": ["decision-id-1", "decision-id-2"]
  }
}
```

## Output Format

Present all information in this order:
1. Memory entries created (show IDs and types)
2. Memory maintenance (expired cleanup, promoted entries)
3. Kanban sync status
4. Session summary
5. Git status and branch recommendations
6. Confirm SESSION_CONTEXT.json written
