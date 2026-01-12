# gptel-agent Architecture

This document describes the architecture of gptel-agent for developers who want to understand the codebase, contribute, or extend the functionality.

## Module Overview

gptel-agent is organized into several focused modules:

```
gptel-agent/
├── gptel-agent.el           # Core: Agent setup, sub-agent parsing
├── gptel-agent-tools.el     # Built-in tools (Read, Write, Edit, Bash, etc.)
├── gptel-agent-tools-introspection.el  # Emacs introspection tools
├── gptel-agent-permissions.el   # Tool permission system
├── gptel-agent-safety.el    # Project boundary enforcement
├── gptel-agent-transient.el # Tool preview UI
├── gptel-agent-compaction.el # Context window management
├── gptel-agent-sessions.el  # Session persistence
├── gptel-agent-stats.el     # Token usage tracking
├── gptel-agent-checkpoints.el # State checkpointing
├── gptel-agent-skills.el    # Skill loading
└── gptel-agent-modes.el     # Mode switching
```

## Core Module (gptel-agent.el)

The core module handles:

### Agent Definition Parsing

Sub-agents are defined in Markdown or Org files with frontmatter:

```
┌─────────────────────────────────────────────────────────┐
│  Agent File (*.md or *.org)                              │
├─────────────────────────────────────────────────────────┤
│  YAML/Properties Block                                   │
│  - name: agent name                                      │
│  - description: what this agent does                     │
│  - tools: [tool1, tool2, ...]                            │
│  - pre: (elisp form to run before)                       │
├─────────────────────────────────────────────────────────┤
│  System Prompt                                           │
│  (The actual instructions for the LLM)                   │
└─────────────────────────────────────────────────────────┘
```

Key functions:
- `gptel-agent-parse-markdown-frontmatter` - Parse .md files
- `gptel-agent-parse-org-properties` - Parse .org files
- `gptel-agent-update` - Scan directories and register agents

### Template Expansion

Agent files support template variables: `{{AGENTS}}` expands to the list of available sub-agents.

### Preset Integration

Agents are registered as gptel presets via `gptel-make-preset`.

## Tool System (gptel-agent-tools.el)

### Tool Registration

Tools are registered using `gptel-make-tool`:

```elisp
(gptel-make-tool
 :name "ToolName"
 :description "What the tool does"
 :args '((:name "arg1" :type string :description "...")
         (:name "arg2" :type integer :optional t))
 :category "gptel-agent"
 :function (lambda (arg1 &optional arg2) ...))
```

### Built-in Tools

| Tool | Purpose |
|------|---------|
| Read | Read file contents |
| Write | Create/overwrite files |
| Edit | Make targeted edits |
| Bash | Execute shell commands |
| Grep | Search file contents |
| Glob | Find files by pattern |
| WebSearch | Search the web |
| FetchURL | Retrieve web content |
| Agent | Delegate to sub-agent |

### Tool Categories

Tools are organized into categories:
- `gptel-agent` - Full agent capabilities
- `gptel-plan` - Read-only tools for planning

## Data Flow

```
User Input
    │
    ▼
┌─────────────────┐
│  gptel-send     │  User sends message
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  LLM Response   │  May include tool calls
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Tool Execution  │  Execute requested tools
│ Pipeline        │
├─────────────────┤
│ 1. Parse call   │
│ 2. Check perms  │  (gptel-agent-permissions.el)
│ 3. Safety check │  (gptel-agent-safety.el)
│ 4. Preview?     │  (gptel-agent-transient.el)
│ 5. Execute      │
│ 6. Track stats  │  (gptel-agent-stats.el)
│ 7. Checkpoint?  │  (gptel-agent-checkpoints.el)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Tool Result     │  Sent back to LLM
└─────────────────┘
```

## Permission System (gptel-agent-permissions.el)

### Permission Configuration

Permissions are defined in `.gptel-agent.el` at the project root:

```elisp
(:tool-permissions
 (:allow ("Read" "Grep" "Glob")   ; Always execute
  :deny ("Bash")                   ; Never execute
  :ask ("Write" "Edit")))          ; Require approval
```

### Permission Resolution

1. Check session cache for prior approval
2. Match tool name against deny patterns
3. Match tool name against allow patterns
4. Match tool name against ask patterns
5. Default: require approval

### Glob Pattern Matching

Permission patterns support glob-style matching:
- `*` matches any characters
- `Read*` matches "Read", "ReadFile", etc.

## Safety System (gptel-agent-safety.el)

### Project Boundary Enforcement

Prevents modifications outside the project root:

```
Project Root: /home/user/project/
├── src/         ✓ Allowed
├── docs/        ✓ Allowed
└── external →   ? Based on policy
```

### External Access Policy

- `deny` - Never allow external paths
- `ask` - Prompt for each external path
- `allow` - Allow all (not recommended)

### Whitelist Support

Trusted paths can be whitelisted:

```elisp
(setq gptel-agent-external-whitelist
      '("~/common-libs/" "/usr/local/share/"))
```

## Session Persistence (gptel-agent-sessions.el)

### Storage Backend

```
┌─────────────────────────────────────────┐
│           Storage Layer                  │
├─────────────────────────────────────────┤
│  SQLite (primary, when available)        │
│  - sessions table                        │
│  - checkpoints table                     │
│  - WAL mode for crash resistance         │
├─────────────────────────────────────────┤
│  JSON (fallback)                         │
│  - ~/.emacs.d/gptel-agent-sessions/      │
│  - Individual JSON files per session     │
└─────────────────────────────────────────┘
```

### Session Data

Each session stores:
- Conversation messages
- System prompt
- Model configuration
- Metadata (project, timestamps, etc.)

### Auto-Save

Sessions auto-save after idle time via `run-with-idle-timer`.

## Checkpoint System (gptel-agent-checkpoints.el)

### State Serialization

Checkpoints capture:
- Message history
- Pending tool calls
- Todo list state
- Buffer position and tool count

### Automatic Checkpoints

Triggered every N tool calls (configurable).

### Retention Policy

Older checkpoints are cleaned up based on `gptel-agent-checkpoint-retention`.

## Token Statistics (gptel-agent-stats.el)

### Tracking

- Input/output tokens per response
- Session totals and cumulative totals
- Cost calculation based on model pricing

### Budget Monitoring

Configurable budget limits with actions:
- `warn` - Display warning
- `confirm` - Require confirmation
- `stop` - Prevent further requests

## Context Compaction (gptel-agent-compaction.el)

### Strategies

- `summarize` - Use LLM to summarize older messages
- `truncate` - Remove oldest messages
- `hybrid` - Summarize, then truncate if needed

### Preservation

Recent messages are always preserved (configurable count).

## Extension Points

### Adding Custom Tools

```elisp
(gptel-make-tool
 :name "MyTool"
 :description "Custom tool description"
 :args '((:name "input" :type string))
 :category "gptel-agent"
 :include t
 :function #'my-tool-function)
```

### Creating Custom Sub-Agents

1. Create file in `gptel-agent-dirs`
2. Add frontmatter with description
3. Write system prompt
4. Call `gptel-agent-update`

### Adding Custom Modes

```elisp
(setq gptel-agent-custom-modes
      '((review . (:tools (Read Grep Glob)
                   :prompt "You are a code reviewer..."))))
```

### Extending Permissions

Add patterns to `.gptel-agent.el` or set globally:

```elisp
(setq gptel-agent-auto-approve '("Read" "Grep" "Glob"))
```

## Buffer-Local State

Each agent session maintains independent state via buffer-local variables:

| Variable | Purpose |
|----------|---------|
| `gptel-agent--current-session-id` | Session identifier |
| `gptel-agent--checkpoint-tool-count` | Tool calls since checkpoint |
| `gptel-agent--todos` | Current todo list |
| `gptel-agent--pending-tool-calls` | Queued tool calls |
| `gptel-agent--session-stats` | Token usage tracking |
| `gptel-agent--permission-cache` | Cached approvals |

## Testing

Tests are located in `test/` and use ERT (Emacs Lisp Regression Testing):

```bash
# Run all tests
eask test ert test/*.el

# Run specific test file
emacs -batch -l test/run-tests.el
```

Each module has a corresponding test file:
- `gptel-agent-permissions-test.el`
- `gptel-agent-safety-test.el`
- `gptel-agent-sessions-test.el`
- etc.

## Dependencies

- **gptel** (>= 0.9.9) - Core LLM interface
- **compat** (>= 30.1.0.0) - Compatibility library
- **emacs** (>= 29.1) - For built-in SQLite support
