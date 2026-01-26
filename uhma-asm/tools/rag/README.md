# UHMA Tools

Context injection and MCP control interface for Claude Code.

## Components

### 1. MCP Server (`server.py`)

Full UHMA control interface via Model Context Protocol. Exposes 27 tools for command, control, and communication with UHMA.

**Setup**: Create `.mcp.json` in project root (NOT `~/.claude/mcp.json`):

```json
{
  "mcpServers": {
    "uhma": {
      "command": "python3",
      "env": {"PYTHONUNBUFFERED": "1"},
      "args": ["/path/to/uhma-asm/tools/rag/server.py"],
      "cwd": "/path/to/uhma-asm"
    }
  }
}
```

**Important**: Restart Claude Code after modifying `.mcp.json`. Verify with `/mcp` command.

**Tools Exposed**:
| Category | Tools |
|----------|-------|
| Input | `input` (process text), `raw` (escape hatch) |
| Status | `help`, `status`, `self`, `metacog`, `debugger`, `genes`, `subroutines`, `regions`, `presence`, `drives` |
| Debug | `why`, `misses`, `receipts`, `listen`, `trace` |
| Actions | `dream`, `observe`, `compact`, `reset` |
| I/O | `save`, `load`, `eat` (digest file) |
| Hive | `hive`, `share`, `colony`, `export`, `import_gene` |
| Other | `geom`, `web_fetch`, `quit` |

UHMA auto-spawns on first tool call if not running.

### 2. PreToolUse Hook (`hook.py`)

Injects context before Claude Code edits/reads `.asm` files:
- File descriptions, entry points, gotchas
- Dependency information
- Session context and memory entries

**Config**: In `~/.claude/settings.json`:
```json
{
  "hooks": {
    "preToolUse": [
      {
        "matcher": {"toolName": "Edit|Write|Read|Grep|Glob"},
        "hooks": [{"command": "python3 /path/to/tools/rag/hook.py"}]
      }
    ]
  }
}
```

### 3. RAG Index (`index.json`)

Pre-built index of UHMA codebase:
- 29 files with descriptions, entry points, gotchas
- 223+ functions with signatures
- Full dependency graph

**Rebuild**:
```bash
python3 build.py
```

### 4. Semantic Memory (`memory.py`)

Cross-session persistence for findings, failures, insights:
- TF-IDF search
- Theme clustering
- Session tracking

**Files**: `memory/entries.json`, `memory/current_state.md`

## File Reference

| File | Purpose |
|------|---------|
| `server.py` | MCP server (UHMA control interface) |
| `hook.py` | PreToolUse hook (context injection) |
| `build.py` | RAG index builder |
| `context.py` | Context generation functions |
| `query.py` | CLI for querying index |
| `memory.py` | Semantic memory system |
| `index.json` | Generated index |
