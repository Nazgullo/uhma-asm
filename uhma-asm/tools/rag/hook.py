#!/usr/bin/env python3
"""
Hook script for Claude Code preToolUse.

Called automatically before Edit/Write/Read/Grep/Glob operations.
- Injects memory context on first operation of session
- Injects file-specific context for .asm files
- Injects all gotchas for .asm glob patterns
"""

import sys
import json
from pathlib import Path
from datetime import datetime

SCRIPT_DIR = Path(__file__).parent
SESSION_MARKER = SCRIPT_DIR / 'memory' / '.session_active'
DEBUG_LOG = SCRIPT_DIR / 'hook_debug.log'

def log_debug(msg):
    """Append debug message to log file."""
    with open(DEBUG_LOG, 'a') as f:
        f.write(f"[{datetime.now().isoformat()}] {msg}\n")

def inject_session_context():
    """Inject memory context at session start (first operation)."""
    if SESSION_MARKER.exists():
        return None  # Already injected this session

    # Mark session as active
    SESSION_MARKER.parent.mkdir(exist_ok=True)
    SESSION_MARKER.write_text('')

    # Load current state
    current_state_file = SCRIPT_DIR / 'memory' / 'current_state.md'
    if not current_state_file.exists():
        return None

    # Load recent memory entries
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from memory import Memory
        mem = Memory()
        recent = mem.recent(10)

        output = []
        output.append("<session-context>")
        output.append("## Current State Summary")
        output.append(current_state_file.read_text()[:2000])  # First 2K chars

        if recent:
            output.append("\n## Recent Memory Entries")
            for entry in recent[:5]:
                icon = {'finding': '✓', 'hypothesis': '?', 'failed': '✗',
                        'success': '★', 'insight': '💡'}.get(entry.category, '•')
                output.append(f"[{icon}] {entry.content[:100]}")

        output.append("</session-context>")
        return '\n'.join(output)
    except Exception as e:
        return f"<session-context>Error loading memory: {e}</session-context>"


def inject_file_context(filename):
    """Inject context for a specific .asm file."""
    sys.path.insert(0, str(SCRIPT_DIR))
    from context import context_before_edit

    context = context_before_edit(filename)
    if context and 'Unknown file' not in context and 'not built' not in context:
        return context
    return None


def inject_all_gotchas():
    """Inject all gotchas for glob patterns targeting .asm."""
    sys.path.insert(0, str(SCRIPT_DIR))
    from context import get_all_gotchas

    gotchas = get_all_gotchas()
    if gotchas and 'not built' not in gotchas:
        return gotchas
    return None


# Main execution
log_debug("Hook invoked")
try:
    hook_data = json.load(sys.stdin)
    log_debug(f"Received: {json.dumps(hook_data)[:500]}")
except (json.JSONDecodeError, EOFError) as e:
    log_debug(f"JSON parse error: {e}")
    sys.exit(0)

output_parts = []

# Always try to inject session context on first operation
session_ctx = inject_session_context()
if session_ctx:
    output_parts.append(session_ctx)

# Extract tool_input (the nested structure from Claude Code)
tool_input = hook_data.get('tool_input', hook_data)

# Extract file path from various tool formats
file_path = tool_input.get('file_path') or tool_input.get('path') or ''
pattern = tool_input.get('pattern', '')

# For Glob with .asm pattern - inject all gotchas
if pattern and '.asm' in pattern and not file_path:
    gotchas = inject_all_gotchas()
    if gotchas:
        output_parts.append(gotchas)

# For specific .asm file operations
elif file_path and file_path.endswith('.asm') and 'uhma-asm' in file_path:
    # Extract relative path from uhma-asm (e.g., "gui/visualizer.asm" or "dispatch.asm")
    p = Path(file_path)
    try:
        idx = p.parts.index('uhma-asm')
        rel_path = '/'.join(p.parts[idx + 1:])
    except ValueError:
        rel_path = p.name
    file_ctx = inject_file_context(rel_path)
    if file_ctx:
        output_parts.append(file_ctx)

# Output all collected context as JSON with additionalContext
if output_parts:
    output = '\n\n'.join(output_parts)
    log_debug(f"Outputting {len(output)} chars")
    # For PreToolUse hooks, must use JSON additionalContext to inject into Claude's context
    # Plain stdout only shows in verbose mode, not visible to Claude
    result = {
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "additionalContext": output
        }
    }
    print(json.dumps(result))
else:
    log_debug("No output produced")
