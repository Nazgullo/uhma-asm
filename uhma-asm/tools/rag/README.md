# UHMA RAG System

Context injection for Claude Code. Automatically provides gotchas, dependencies, and entry points before editing files.

## Setup

Add to `~/.claude/settings.json`:

```json
{
  "mcpServers": {
    "uhma-rag": {
      "command": "python3",
      "args": ["/home/peter/Desktop/STARWARS/uhma-asm/tools/rag/server.py"],
      "cwd": "/home/peter/Desktop/STARWARS/uhma-asm/tools/rag"
    }
  }
}
```

Or for project-specific, add to `.claude/settings.json` in the project root.

## Tools Exposed

| Tool | Description |
|------|-------------|
| `uhma_before_edit` | Context before editing a .asm file (gotchas, deps, entries) |
| `uhma_before_call` | Context for a function (signature, gotchas) |
| `uhma_search` | Search files, functions, concepts |
| `uhma_gotchas` | List all gotchas |

## Manual Usage (without MCP)

```bash
# Before editing
python context.py before-edit dispatch

# List gotchas
python context.py gotchas

# Search
python context.py search "token"
```

## Rebuilding Index

After modifying file headers:

```bash
python build.py
```

## Files

- `build.py` - Parses headers, generates index.json
- `query.py` - CLI for querying
- `context.py` - Context generation functions
- `server.py` - MCP server for Claude Code integration
- `index.json` - Generated index (29 files, 40 functions, 27 gotchas)
