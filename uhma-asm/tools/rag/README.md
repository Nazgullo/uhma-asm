# UHMA Tools

Context injection and MCP control interface for Claude Code.

**Note**: Core tools (MCP server, feeder, holographic memory) are now pure x86-64 assembly. Python files in this directory are legacy/support utilities.

## Claude Holographic Memory (Dual Purpose)

### 1. Chat Sessions Memory (6GB Surface)
11 categories for cross-session persistence:
- finding, failed, success, insight, warning, session
- location, question, todo, context, request

### 2. 3-Layer Code RAG
| Category | Decay | Fidelity | Content |
|----------|-------|----------|---------|
| code_high | 0.98 | High | Architecture, file purposes, system overview |
| code_mid | 0.96 | Medium | Function signatures, @entry points, @calls |
| code_low | 0.92 | Low | Implementation gotchas, patterns, snippets |

### Architecture
```
┌─────────────────────────────────────────────────────────────┐
│  MCP Interface (../mcp_server)                              │
│  - Claude Code ←→ JSON-RPC ←→ UHMA TCP                      │
│  - mem_add category="code_high|code_mid|code_low|finding|.."│
├─────────────────────────────────────────────────────────────┤
│  Holographic Memory (holo_mem.asm)                          │
│  - 14 categories (11 chat + 3 code RAG)                     │
│  - VSA similarity search (1024-dim f64 vectors)             │
│  - Category traces for resonance queries                    │
├─────────────────────────────────────────────────────────────┤
│  UHMA Surface (8GB memory-mapped)                           │
│  - 4096 entries × 2KB = 8MB entry storage                   │
│  - 14 category traces × 8KB = 112KB trace storage           │
└─────────────────────────────────────────────────────────────┘
```

## Assembly Tools (Primary)

| Tool | Location | Purpose |
|------|----------|---------|
| `mcp_server` | `../mcp_server` | MCP JSON-RPC server for Claude Code |
| `feeder` | `../feeder` | Training client for UHMA |
| `holo_mem.asm` | `../../holo_mem.asm` | Holographic memory (integrated in UHMA) |

### MCP Server Setup

Create `.mcp.json` in project root:

```json
{
  "mcpServers": {
    "uhma": {
      "command": "/path/to/uhma-asm/tools/mcp_server",
      "cwd": "/path/to/uhma-asm"
    }
  }
}
```

**Important**: UHMA must be running before MCP server starts (connects to TCP ports 9997/9996).

Restart Claude Code after changes. Verify with `/mcp`.

### MCP Tools Exposed

| Category | Tools |
|----------|-------|
| Input | `input` (process text), `raw` (escape hatch) |
| Status | `help`, `status`, `self`, `metacog`, `intro`, `presence`, `drives`, `regions` |
| Debug | `why`, `misses`, `receipts`, `trace`, `listen` |
| Actions | `dream`, `observe`, `compact`, `reset` |
| I/O | `save`, `load`, `eat` (digest file) |
| Memory | `mem_add`, `mem_query`, `mem_outcome`, `mem_state`, `mem_summary`, `mem_recent` |
| Hive | `hive`, `colony`, `export`, `import_gene` |
| Other | `geom`, `web_fetch`, `quit` |

## Python Support Files (Legacy)

These are optional utilities, not required for core operation:

| File | Purpose |
|------|---------|
| `hook.py` | PreToolUse hook (context injection) |
| `context.py` | Context generation functions |
| `query.py` | CLI for querying RAG index |
| `capture.py` | Session capture utilities |

### PreToolUse Hook (Optional)

Injects context before Claude Code edits/reads `.asm` files:
- File descriptions, entry points, gotchas
- Dependency information

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

## Memory Files

```
memory/
├── holo_entries.json   # JSON entries (backup/export)
├── holo_surface.dat    # VSA surface (mmap'd)
├── holo_traces.npz     # Category trace vectors
└── holo_state.json     # System state
```

## Archived Python Tools

Original Python implementations moved to `archives/`:
- `server.py` → `tools/mcp_server` (assembly)
- `holo_memory.py` → `holo_mem.asm` (assembly)
- `feed.sh` → `tools/feeder` (assembly)

---

*Last updated: 2026-02-01*
