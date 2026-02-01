#!/usr/bin/env python3
"""
session_end.py — Session end hook for holographic memory maintenance

@entry main()              Called at session end
@entry decay_memory()      Apply decay to all holographic traces
@entry clear_marker()      Clear session marker for next session

@calls holo_memory.py:HoloMemory.decay_traces()
@calledby Claude Code session end (configured in settings)

ACTIONS:
  1. Apply decay to holographic memory traces (category-specific rates)
  2. Clear SESSION_MARKER for next session detection

DECAY RATES (per category):
  finding=0.95, failed=0.90, success=0.95, insight=0.95, warning=0.92,
  session=0.85, location=0.98, question=0.80, todo=0.85, context=0.70

GOTCHAS:
  - SESSION_MARKER at tools/rag/memory/.session_active
  - Decay is multiplicative (trace *= decay_rate)
  - Fast-decaying categories: question (0.80), context (0.70)
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
