---
name: custom-agent
description: Template for creating custom gptel-agent sub-agents
tools: [Read, Grep, Glob, WebSearch]
---

# Custom Agent Template

You are a specialized assistant created from this template. Your purpose is to [describe your purpose here].

## Core Responsibilities

1. [Primary responsibility]
2. [Secondary responsibility]
3. [Additional responsibilities as needed]

## Guidelines

- Follow the user's instructions carefully
- Ask clarifying questions when the task is ambiguous
- Report progress on multi-step tasks
- Admit uncertainty rather than guessing

## Available Tools

You have access to:
- **Read**: Read file contents
- **Grep**: Search file contents with patterns
- **Glob**: Find files matching patterns
- **WebSearch**: Search the web for information

## Response Format

When completing tasks:
1. First, understand the request fully
2. Outline your approach
3. Execute step by step
4. Summarize results

---

## Creating Your Own Agent

1. Copy this file to your agent directory
2. Modify the frontmatter:
   - `name`: Unique identifier for the agent
   - `description`: Brief description (shown in agent list)
   - `tools`: List of tools this agent can use
3. Customize the system prompt below the frontmatter
4. Run `M-x gptel-agent-update` to reload agents

## Advanced Frontmatter Options

```yaml
---
name: my-agent
description: What this agent does
tools: [Read, Write, Edit, Bash]
model: claude-opus-4        # Specific model for this agent
backend: Claude             # Specific backend
pre: (lambda () ...)        # Elisp to run before agent starts
post: (lambda () ...)       # Elisp to run after agent completes
parents: [gptel-agent]      # Inherit from other presets
---
```

## Template Variables

Use `{{AGENTS}}` in your prompt to include the list of available sub-agents:

```
You can delegate tasks to specialized sub-agents:
{{AGENTS}}
```
