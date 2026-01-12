# Changelog

All notable changes to gptel-agent are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2025-01-12

### Added

#### Tool Permissions (gptel-agent-permissions.el)
- Project-level tool permission configuration via `.gptel-agent.el`
- Allow/deny/ask patterns for fine-grained control
- Glob pattern matching for tool names
- Session-level permission caching

#### Tool Preview (gptel-agent-transient.el)
- Interactive tool call preview using transient.el
- Diff-style preview for file modifications
- Approve/deny/modify options before execution
- Toggle with `C-c C-a t` or `M-x gptel-agent-tool-preview-mode`

#### External Directory Safety (gptel-agent-safety.el)
- Project boundary detection and enforcement
- Configurable external access policies
- Whitelist support for trusted external paths
- Path normalization and validation

#### Context Compaction (gptel-agent-compaction.el)
- Automatic prompt compaction to prevent context overflow
- Multiple strategies: summarize, truncate, hybrid
- Configurable thresholds and preserved message counts
- Non-blocking async summarization

#### Session Persistence (gptel-agent-sessions.el)
- SQLite-backed session storage (WAL mode for crash resistance)
- Session create, save, load, list, delete operations
- JSON fallback when SQLite unavailable
- Auto-save with idle timer
- Configurable retention policies
- Markdown export for sharing
- Interactive session browser

#### Token Statistics (gptel-agent-stats.el)
- Real-time token usage tracking (input/output separately)
- Cost calculation with configurable model pricing
- Header-line display showing compact usage
- Budget alerts with configurable limits
- Data export to CSV/JSON
- Session persistence integration

#### Skills (gptel-agent-skills.el)
- Skill loading from SKILL.md/SKILL.org files
- Prompt composition with prepend/append positions
- Tool allow/deny lists per skill
- Dependency resolution with cycle detection
- Version compatibility checking

#### Mode Switching (gptel-agent-modes.el)
- Enhanced mode cycling with `C-c C-a m`
- Agent mode (full tools) and Plan mode (read-only)
- Custom mode definitions via `gptel-agent-custom-modes`
- Mode-specific faces and visual indicators
- Automatic tool filtering per mode

#### Checkpoints (gptel-agent-checkpoints.el)
- Checkpoint creation for long-running tasks
- Automatic checkpoints after N tool calls
- Manual checkpoint command with descriptions
- Recovery interface with checkpoint browser
- Auto-prompt for recovery on interrupted sessions
- Configurable retention policy

### Changed
- Enhanced README.org with quick start guide
- Added comprehensive troubleshooting section
- Updated Eask with all new modules

## [0.1.0] - 2025-01-01

### Added
- Initial release
- Core agent functionality with gptel integration
- Tool registration and execution framework
- Sub-agent delegation support
- Markdown and Org agent file parsing
- Built-in agents: executor, researcher, introspector
- Basic safety controls with approval requirements
