#!/usr/bin/env python3
"""
hook.py — PreToolUse hook for context injection

Injects at session start:
- Git status (uncommitted changes - CRITICAL)
- Recent failed approaches
- Cognitive warnings

Injects on Edit/Write:
- Relevant memories for current context
- Cognitive warnings
"""
import sys
import json
import subprocess
from pathlib import Path
from datetime import datetime

SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent
SESSION_MARKER = SCRIPT_DIR / 'memory' / '.session_active'
MODIFYING_TOOLS = {'Edit', 'Write', 'NotebookEdit'}

def log_debug(msg):
    try:
        with open(SCRIPT_DIR / 'hook_debug.log', 'a') as f:
            f.write(f"[{datetime.now().isoformat()}] {msg}\n")
    except:
        pass

def get_git_status():
    """Get uncommitted changes - CRITICAL for catching debug debris."""
    try:
        # Check for uncommitted changes to .asm files
        result = subprocess.run(
            ['git', 'diff', '--stat', '*.asm'],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True,
            timeout=5
        )
        asm_changes = result.stdout.strip()

        # Also get general status
        result2 = subprocess.run(
            ['git', 'status', '--short'],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True,
            timeout=5
        )
        status = result2.stdout.strip()

        if asm_changes:
            return f"UNCOMMITTED .asm CHANGES:\n{asm_changes}\n\nFull status:\n{status[:500]}"
        elif status:
            modified = [l for l in status.split('\n') if l.startswith(' M') or l.startswith('M')]
            if modified:
                return f"Modified files:\n" + '\n'.join(modified[:10])
        return None
    except Exception as e:
        log_debug(f"Git status error: {e}")
        return None

def get_failed_approaches():
    """Get recent failed approaches to avoid repeating mistakes."""
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from holo_memory import HoloMemory
        mem = HoloMemory()
        failed = mem.failed_approaches(limit=5)
        if failed:
            lines = ["RECENT FAILURES (don't repeat):"]
            for entry in failed:
                lines.append(f"  - {entry.content[:100]}")
            return '\n'.join(lines)
        return None
    except Exception as e:
        log_debug(f"Failed approaches error: {e}")
        return None

def get_cognitive_state():
    """Get cognitive warnings."""
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from holo_memory import HoloMemory
        mem = HoloMemory()
        state = mem.get_state()
        warnings = state.get('warnings', [])
        if warnings:
            return "COGNITIVE WARNINGS:\n" + '\n'.join(f"  ! {w}" for w in warnings)
        return None
    except:
        return None

def get_relevant_memories(context):
    """Get memories relevant to current context."""
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from holo_memory import HoloMemory
        mem = HoloMemory()
        results = mem.query(context, limit=3, threshold=0.4)
        if results:
            lines = ["RELEVANT MEMORIES:"]
            for entry, sim in results:
                lines.append(f"  [{entry.category}] {entry.content[:80]}")
            return '\n'.join(lines)
        return None
    except:
        return None

def extract_context(hook_data):
    """Extract context string from hook data."""
    ti = hook_data.get('tool_input', {})
    parts = [hook_data.get('tool_name', '')]

    for key in ['file_path', 'path', 'pattern', 'query']:
        if val := ti.get(key):
            if 'path' in key:
                parts.append(Path(val).stem)
            else:
                parts.append(str(val)[:100])

    if cmd := ti.get('command', ''):
        parts.extend(cmd.split()[:5])

    return ' '.join(p for p in parts if p)

def inject_session_start():
    """Inject context at session start. Returns None if not first tool call."""
    if SESSION_MARKER.exists():
        return None

    SESSION_MARKER.parent.mkdir(exist_ok=True)
    SESSION_MARKER.write_text(datetime.now().isoformat())

    parts = []

    # CRITICAL: Git status first
    if git := get_git_status():
        parts.append(f"<git-status>\n{git}\n</git-status>")

    # Failed approaches
    if failed := get_failed_approaches():
        parts.append(f"<failed-approaches>\n{failed}\n</failed-approaches>")

    # Cognitive state
    if cog := get_cognitive_state():
        parts.append(f"<cognitive-state>\n{cog}\n</cognitive-state>")

    if parts:
        return '\n\n'.join(parts)
    return None

def inject_for_modification(context):
    """Inject context for Edit/Write operations."""
    parts = []

    # Cognitive warnings
    if cog := get_cognitive_state():
        parts.append(cog)

    # Relevant memories
    if mem := get_relevant_memories(context):
        parts.append(mem)

    if parts:
        return '\n\n'.join(parts)
    return None

# Main
log_debug("Hook invoked")

try:
    hook_data = json.load(sys.stdin)
    log_debug(f"Tool: {hook_data.get('tool_name', 'unknown')}")
except Exception as e:
    log_debug(f"JSON parse error: {e}")
    sys.exit(0)

tool_name = hook_data.get('tool_name', '')
is_modifying = tool_name in MODIFYING_TOOLS
context = extract_context(hook_data)

output_parts = []

# Session start injection (first tool call)
if session_ctx := inject_session_start():
    output_parts.append(session_ctx)

# Modification-specific injection
if is_modifying:
    if mod_ctx := inject_for_modification(context):
        output_parts.append(mod_ctx)

# Output
if output_parts:
    output = '\n\n'.join(output_parts)
    log_debug(f"Outputting {len(output)} chars")
    print(json.dumps({
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "additionalContext": output
        }
    }))
else:
    log_debug("No output produced")
