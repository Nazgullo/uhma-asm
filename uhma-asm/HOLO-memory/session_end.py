#!/usr/bin/env python3
"""
Session end hook - decay traces and clear session marker.

Actions:
1. Apply decay to holographic memory traces
2. Clear session marker for next session
"""

import sys
import json
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent
SESSION_MARKER = SCRIPT_DIR / 'memory' / '.session_active'

sys.path.insert(0, str(SCRIPT_DIR))

def decay_memory():
    """Apply decay to holographic memory traces."""
    try:
        from holo_memory import get_memory
        mem = get_memory()
        mem.decay_traces()
        return True
    except Exception as e:
        return False

def clear_session():
    """Clear session marker so next session gets context injection."""
    if SESSION_MARKER.exists():
        SESSION_MARKER.unlink()

# Main execution
try:
    tool_input = json.load(sys.stdin)
except (json.JSONDecodeError, EOFError):
    tool_input = {}

# Decay traces at session end
decay_memory()
clear_session()

# Output reminder
print(json.dumps({
    "hookSpecificOutput": {
        "hookEventName": "PostToolUse",
        "additionalContext": "Session ending - holographic memory traces decayed."
    }
}))
