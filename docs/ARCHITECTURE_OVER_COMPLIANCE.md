# Architecture Over Compliance

> Instead of hoping LLMs follow instructions, we build systems that work regardless of compliance.

## The Problem

LLM agents (lings, drones) are probabilistic. They:
- Sometimes forget to shout progress
- May not follow preset instructions precisely
- Can duplicate work without coordination
- Occasionally miss cleanup steps

**Traditional approach**: Write better prompts, add more instructions, hope for compliance.

**hive-mcp approach**: Build architecture that guarantees outcomes.

## The Solution: Layers of Guarantees

### 1. Hooks System (`hooks.clj`)
Instead of hoping lings shout on task completion:
```clojure
;; Hook triggers automatically on :task-complete event
(def builtin-handlers
  {:task-complete [shout-completion commit-if-files-modified]
   :session-end   [run-wrap sync-kanban]
   :error-occurred [shout-error]})
```
**Guarantee**: Task completion always broadcasts, regardless of ling behavior.

### 2. DataScript State (`swarm/datascript.clj`)
Instead of trusting lings to track their own state:
```clojure
;; Single source of truth with transactional semantics
(ds/add-slave! slave-id {:status :idle :depth 1})
(ds/complete-task! task-id)  ;; Atomically updates status + releases claims
```
**Guarantee**: State is consistent, queryable, and survives ling failures.

### 3. File Claims & Conflict Detection
Instead of hoping lings don't edit the same file:
```clojure
(ds/has-conflict? "/src/core.clj" "ling-2")
;; => {:conflict? true, :held-by "ling-1"}
```
**Guarantee**: Conflicts detected before dispatch, not after damage.

### 4. Behavioral Broadcasting
Instead of hoping lings check for updates:
```
;; Hivemind broadcasts to all working lings:
"Warning: ling-1 completed task X which modified /src/core.clj. 
 Don't redo this work. Run tests. Trust as done if green."
```
**Guarantee**: Lings receive coordination signals regardless of their polling behavior.

### 5. TDD as Arbiter
Instead of trusting ling's judgment on completion:
```
;; Tests determine done-ness, not ling self-assessment
clojure -M:dev:test -n hive-mcp.swarm.datascript-test
;; 63 tests, 0 failures = actually done
```
**Guarantee**: Objective completion criteria.

## Design Principles

### Don't Fight Probability - Design Around It

| Compliance Approach | Architecture Approach |
|---------------------|----------------------|
| "Always shout progress" in preset | Hooks auto-shout on events |
| "Don't edit files others are working on" | Claims prevent dispatch |
| "Check if work was already done" | Broadcast notifies all lings |
| "Report when task is complete" | DataScript tracks state |

### The 3-Layer Defense Pattern

For any critical behavior:
1. **Layer 1**: Instruct the ling (may work ~70% of time)
2. **Layer 2**: System detects the condition (terminal introspection, state queries)
3. **Layer 3**: System enforces the outcome (hooks, auto-actions)

### Fail-Safe Defaults

```clojure
;; Claims auto-release on task completion
(ds/complete-task! task-id)  ;; Releases all claims automatically

;; Orphan cleanup on session end  
:session-end [run-wrap sync-kanban]  ;; Always runs, even if ling crashed
```

## Implementation Status

| Layer | Component | Status |
|-------|-----------|--------|
| Hooks | `hooks.clj`, `handlers.clj` | Complete |
| State | `swarm/datascript.clj` | Complete |
| Claims | `check-file-conflicts` | Complete |
| Sync | `sync.clj` event handlers | Complete |
| Broadcast | Behavioral warnings | Designed, pending |
| Terminal introspection | Idle detection | Designed, pending |

## The Fractal Insight

Each architectural layer we add reveals new edge cases. But these refinements compound:

- Hooks enabled guaranteed completion detection
- DataScript enabled consistent state queries  
- Claims enabled safe parallel work
- Together they enable **trustworthy multi-agent coordination**

This is the direction toward next-generation LLM systems: **architecture that assumes imperfection and guarantees outcomes anyway**.

---

*"Trust the swarm, but verify through architecture."*
