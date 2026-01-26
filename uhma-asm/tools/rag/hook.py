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

SCRIPT_DIR = Path(__file__).parent
SESSION_MARKER = SCRIPT_DIR / 'memory' / '.session_active'

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
try:
    tool_input = json.load(sys.stdin)
except (json.JSONDecodeError, EOFError):
    sys.exit(0)

output_parts = []

# Always try to inject session context on first operation
session_ctx = inject_session_context()
if session_ctx:
    output_parts.append(session_ctx)

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
    filename = Path(file_path).name
    file_ctx = inject_file_context(filename)
    if file_ctx:
        output_parts.append(file_ctx)

# Output all collected context
if output_parts:
    print('\n\n'.join(output_parts))
