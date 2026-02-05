# HOLO-memory: Claude's Holographic Memory

Persistent cross-session memory for Claude Code, stored as holographic vectors.

## Current Implementation

The memory system is now **pure x86-64 assembly** (`holo_mem.asm`), linked into the MCP server (`tools/mcp_server`). No Python dependencies.

### How It Works

1. Claude Code sends MCP tool calls (`mem_add`, `mem_query`, etc.)
2. MCP server processes requests using `holo_mem.asm` functions
3. Content is encoded to 1024-dim f64 vectors via MPNet embeddings (`embed/*.asm`)
4. Vectors are stored in a 6GB memory-mapped surface file
5. Queries use cosine similarity against category traces

### Surface File

`memory/holo_surface.dat` — 6GB sparse file (only written pages use disk).

Layout:
- **Entries**: 4096 max, 2KB each (id, category, content, context, holographic vector)
- **Category traces**: 14 traces x 8KB (superposition of all entries in category)
- **State**: Entry count, next ID, timestamps

### Categories

| # | Category | Purpose | Decay |
|---|----------|---------|-------|
| 0 | finding | Confirmed facts | 0.95 |
| 1 | failed | What didn't work | 0.90 |
| 2 | success | What worked | 0.95 |
| 3 | insight | Aha moments | 0.95 |
| 4 | warning | Gotchas to remember | 0.92 |
| 5 | session | Session summaries | 0.85 |
| 6 | location | Code locations | 0.98 |
| 7 | question | Open questions | 0.80 |
| 8 | todo | Tasks | 0.85 |
| 9 | context | Temporary context | 0.70 |
| 10 | request | User requests | 0.80 |
| 11 | code_high | Raw source code | 0.98 |
| 12 | code_mid | Function signatures | 0.96 |
| 13 | code_low | Summaries, gotchas | 0.92 |

### MCP Tools

| Tool | Description |
|------|-------------|
| `mem_add` | Add entry (category + content + optional context) |
| `mem_query` | Semantic similarity search |
| `mem_state` | Cognitive state (entry counts) |
| `mem_recent` | Recent entries |
| `mem_summary` | Summary statistics |

### Architecture

This is Claude's memory, NOT UHMA's. UHMA has its own surface and cognition. The holographic memory lives in the MCP server process and communicates with Claude Code via JSON-RPC.

```
Claude Code ←→ JSON-RPC ←→ MCP Server (tools/mcp_server)
                              ├── holo_mem.asm (memory)
                              ├── embed/*.asm (MPNet embeddings)
                              └── TCP gateway → UHMA (port 9999)
```

### Previous Implementation

The original Python version (`holo_memory.py`) using sentence-transformers has been archived to `archives/`. The assembly version provides the same functionality without Python dependencies.

---

*Last updated: 2026-02-05*
