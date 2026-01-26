# UHMA-asm Current State
Generated: 2026-01-26

## Project Overview
UHMA (Unified Holographic Memory Architecture) - self-modifying x86-64 assembly system that learns patterns through holographic/VSA encoding.

## Recent Sessions

### Session 1 (2026-01-26) - RAG + Memory Integration
**Done:**
- Fixed preToolUse hooks to read JSON from stdin
- Expanded hooks to Read/Grep/Glob (was just Edit/Write)
- Rewrote MCP server as full UHMA control interface (27 tools)
- Integrated semantic memory system from MINION project
- Added forced injection hooks for memory persistence

**Key Files Modified:**
- .claude/settings.json - hook configuration
- tools/rag/hook.py - context injection
- tools/rag/server.py - MCP UHMA control
- tools/rag/memory.py - semantic memory system

## Hard-Learned Lessons (from CLAUDE.md)

### Registers
- **rcx is caller-saved** - don't use in loops that call functions, use r10-r15
- **Restore r14 after fault recovery** - process_token clobbers r14 (SURFACE_BASE)

### Context
- **ctx = hash(prev_token) ONLY** - no somatic XOR, breaks pattern identity

### Token Abstraction
- Must match in BOTH process_input AND digest_file
- Hex literals → TOKEN_HEX, Numbers → TOKEN_NUM

### Fault Handling
- Fault handler longjmps to REPL
- Save fault_safe_rsp/rip BEFORE calls that might fault

### Unified Trace
- 8 dimensions: event, ctx, actual, predicted, region, aux, tracer, time
- `why` and `misses` commands query the trace

## Pending Items
(none yet)

## Methodology
- USE HOLOGRAPHIC PARADIGM FIRST - don't build separate data structures
- Use built-in debugging: `why`, `misses`, `status`, `receipts`
- Short test FIRST (tiny_test.txt), then scale up
- Check ALL code paths - digest_file vs process_input behavior
