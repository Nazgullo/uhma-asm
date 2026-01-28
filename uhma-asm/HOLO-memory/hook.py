#!/usr/bin/env python3
"""
hook.py — Claude Code preToolUse hook for holographic context injection.

@entry (main script execution via stdin JSON)
@calls holo_memory.py:HoloMemory, context.py:context_before_edit

FLOW: Claude Code → PreToolUse event → this script → JSON additionalContext → Claude
CONFIG: Configured in .claude/settings.json hooks section

INJECTION TYPES:
  - Holographic memory: Query relevant memories based on tool context
  - Cognitive warnings: Alert if confused, going in circles, etc.
  - Session context: current_state.md on first operation
  - File context: File description, entry points, gotchas for .asm files

OUTPUT FORMAT:
  {"hookSpecificOutput": {"hookEventName": "PreToolUse", "additionalContext": "..."}}

GOTCHAS:
  - Must read JSON from stdin (hook_data), not command-line args
  - SESSION_MARKER (.session_active) tracks first-op-of-session
  - Plain stdout only visible in verbose mode; must use additionalContext JSON
  - Holographic memory uses ISON format for token efficiency
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
    try:
        with open(DEBUG_LOG, 'a') as f:
            f.write(f"[{datetime.now().isoformat()}] {msg}\n")
    except:
        pass


def extract_context_from_tool(hook_data: dict) -> str:
    """
    Extract semantic context from the tool being called.
    This is used to query holographic memory for relevant entries.
    """
    tool_input = hook_data.get('tool_input', hook_data)
    tool_name = hook_data.get('tool_name', '')

    parts = []

    # Tool name tells us what Claude is trying to do
    if tool_name:
        parts.append(tool_name)

    # File path gives domain context
    file_path = tool_input.get('file_path') or tool_input.get('path') or ''
    if file_path:
        # Extract meaningful parts
        p = Path(file_path)
        parts.append(p.stem)  # filename without extension
        if p.suffix:
            parts.append(p.suffix[1:])  # extension without dot

    # Command gives intent
    command = tool_input.get('command', '')
    if command:
        # Extract key words from command (first few tokens)
        cmd_parts = command.split()[:5]
        parts.extend(cmd_parts)

    # Pattern for glob/grep
    pattern = tool_input.get('pattern', '')
    if pattern:
        parts.append(pattern)

    # Query text
    query = tool_input.get('query', '')
    if query:
        parts.append(query)

    # Old/new string for edits
    old_string = tool_input.get('old_string', '')
    if old_string:
        parts.append(old_string[:100])

    return ' '.join(parts)


def inject_holo_context(context: str) -> str:
    """
    Query holographic memory and return ISON-formatted relevant memories.
    """
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from holo_memory import HoloMemory
        mem = HoloMemory()

        output = []

        # Get cognitive state warnings FIRST (most important)
        state = mem.get_state()
        warnings = state.get('warnings', [])
        if warnings:
            output.append("<cognitive-warnings>")
            for w in warnings:
                output.append(f"! {w}")
            output.append("</cognitive-warnings>")

        # Query relevant memories
        if context:
            ison = mem.to_ison(context=context, limit=5, include_state=False)
            if ison and 'table.memories' in ison:
                output.append("<holo-memory format='ISON'>")
                output.append(ison)
                output.append("</holo-memory>")

        # Check for failed approaches in this context
        failed = mem.failed_approaches(context=context, limit=3)
        if failed:
            output.append("<previous-failures>")
            for entry in failed:
                output.append(f"- {entry.content[:150]}")
            output.append("</previous-failures>")

        return '\n'.join(output) if output else None

    except Exception as e:
        log_debug(f"Holo memory error: {e}")
        return None


def inject_session_context():
    """Inject memory context at session start (first operation)."""
    if SESSION_MARKER.exists():
        return None  # Already injected this session

    # Mark session as active
    SESSION_MARKER.parent.mkdir(exist_ok=True)
    SESSION_MARKER.write_text(datetime.now().isoformat())

    # Load current state
    current_state_file = SCRIPT_DIR / 'memory' / 'current_state.md'
    if not current_state_file.exists():
        return None

    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        output = []
        output.append("<session-context>")
        output.append("## Current State Summary")
        output.append(current_state_file.read_text()[:2000])  # First 2K chars

        # Also inject holographic memory summary
        try:
            from holo_memory import HoloMemory
            mem = HoloMemory()

            # Get recent entries from holo memory
            recent = mem.recent(5)
            if recent:
                output.append("\n## Recent Holographic Memory")
                for entry in recent:
                    icon = {'finding': '✓', 'failed': '✗', 'success': '★',
                            'insight': '💡', 'warning': '⚠'}.get(entry.category, '•')
                    output.append(f"[{icon}] {entry.content[:100]}")

            # State summary
            state = mem.get_state()
            output.append(f"\n## Cognitive State")
            output.append(f"Confidence: {state['confidence']:.2f} | Confusion: {state['confusion']:.2f}")
            output.append(f"Entries: {state['entry_count']} | Hit ratio: {state['hit_ratio']:.2f}")

        except Exception as e:
            log_debug(f"Holo memory session inject error: {e}")

        output.append("</session-context>")
        return '\n'.join(output)
    except Exception as e:
        return f"<session-context>Error loading state: {e}</session-context>"


def inject_file_context(filename):
    """Inject context for a specific .asm file."""
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from context import context_before_edit
        context = context_before_edit(filename)
        if context and 'Unknown file' not in context and 'not built' not in context:
            return context
    except Exception as e:
        log_debug(f"File context error: {e}")
    return None


def inject_all_gotchas():
    """Inject all gotchas for glob patterns targeting .asm."""
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from context import get_all_gotchas
        gotchas = get_all_gotchas()
        if gotchas and 'not built' not in gotchas:
            return gotchas
    except Exception as e:
        log_debug(f"Gotchas error: {e}")
    return None


# ============================================================================
# Main execution
# ============================================================================

log_debug("Hook invoked")
try:
    hook_data = json.load(sys.stdin)
    log_debug(f"Received: {json.dumps(hook_data)[:500]}")
except (json.JSONDecodeError, EOFError) as e:
    log_debug(f"JSON parse error: {e}")
    sys.exit(0)

output_parts = []

# 1. Session context on first operation
session_ctx = inject_session_context()
if session_ctx:
    output_parts.append(session_ctx)

# 2. Extract context from tool operation
tool_context = extract_context_from_tool(hook_data)
log_debug(f"Tool context: {tool_context[:200]}")

# 3. Query holographic memory for relevant entries (ALWAYS)
if tool_context:
    holo_ctx = inject_holo_context(tool_context)
    if holo_ctx:
        output_parts.append(holo_ctx)

# 4. File-specific context for .asm files
tool_input = hook_data.get('tool_input', hook_data)
file_path = tool_input.get('file_path') or tool_input.get('path') or ''
pattern = tool_input.get('pattern', '')

# For Glob with .asm pattern - inject all gotchas
if pattern and '.asm' in pattern and not file_path:
    gotchas = inject_all_gotchas()
    if gotchas:
        output_parts.append(gotchas)

# For specific .asm file operations
elif file_path and file_path.endswith('.asm') and 'uhma-asm' in file_path:
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
    result = {
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "additionalContext": output
        }
    }
    print(json.dumps(result))
else:
    log_debug("No output produced")
