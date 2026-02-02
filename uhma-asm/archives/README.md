# Archived Python Tools

These Python tools have been replaced by pure x86-64 assembly implementations.

## Replaced Tools

| Python | Assembly Replacement | Purpose |
|--------|---------------------|---------|
| `server.py` | `tools/mcp_server` | MCP JSON-RPC server for Claude Code |
| `holo_memory.py` | `holo_mem.asm` (integrated) | Holographic memory for cross-session persistence |
| `feed.sh` | `tools/feeder` | Training client for UHMA |
| `hub_claude.py` | `hub` (assembly) | Claude hub client |
| `hub_gemini.py` | `hub` (assembly) | Gemini hub client |

## Why Assembly?

- Direct control, no runtime surprises
- No Python dependency hell
- Faster startup (no interpreter)
- Integrated with UHMA's memory-mapped surface
- Same codebase, same patterns

## Using the New Tools

```bash
# MCP server (auto-started by Claude Code via .mcp.json)
./tools/mcp_server

# Feeder for training
./tools/feeder --corpus corpus/ --cycles 1

# Holographic memory (via UHMA REPL)
> mem_add finding "rcx is caller-saved"
> mem_query "register"
> mem_state
> mem_recent
```

## Date Archived

2026-02-01
