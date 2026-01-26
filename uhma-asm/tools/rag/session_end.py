#!/usr/bin/env python3
"""
Session end hook - called after certain operations to ensure persistence.

Actions:
1. Rebuild RAG index if .asm files changed
2. Prompt for memory entries (findings, failures, insights)
3. Update current_state.md
4. Clear session marker
"""

import sys
import json
import subprocess
from pathlib import Path
from datetime import datetime

SCRIPT_DIR = Path(__file__).parent
SESSION_MARKER = SCRIPT_DIR / 'memory' / '.session_active'
CURRENT_STATE = SCRIPT_DIR / 'memory' / 'current_state.md'

def rebuild_rag():
    """Rebuild RAG index."""
    build_script = SCRIPT_DIR / 'build.py'
    if build_script.exists():
        result = subprocess.run(
            ['python3', str(build_script)],
            capture_output=True,
            text=True,
            cwd=str(SCRIPT_DIR.parent.parent)  # uhma-asm root
        )
        return result.returncode == 0
    return False

def clear_session():
    """Clear session marker so next session gets context injection."""
    if SESSION_MARKER.exists():
        SESSION_MARKER.unlink()

def prompt_memory_save():
    """Output reminder to save memory entries."""
    output = []
    output.append("<session-end-reminder>")
    output.append("## Session Ending - Memory Check")
    output.append("")
    output.append("Before ending, consider recording:")
    output.append("- **Findings**: What facts did you confirm?")
    output.append("- **Failures**: What didn't work? (prevents repeating)")
    output.append("- **Insights**: Any aha moments or connections?")
    output.append("- **TODOs**: What should be done next session?")
    output.append("")
    output.append("Use memory.py to save:")
    output.append("```")
    output.append("python3 tools/rag/memory.py add finding 'description'")
    output.append("python3 tools/rag/memory.py add failed 'what failed and why'")
    output.append("python3 tools/rag/memory.py add insight 'connection discovered'")
    output.append("```")
    output.append("")
    output.append("Or update current_state.md directly.")
    output.append("</session-end-reminder>")
    return '\n'.join(output)

# Main execution
try:
    tool_input = json.load(sys.stdin)
except (json.JSONDecodeError, EOFError):
    tool_input = {}

# Check what tool was used
tool_name = tool_input.get('tool_name', '')

# Rebuild RAG after Write/Edit operations on .asm files
file_path = tool_input.get('file_path', '')
if file_path.endswith('.asm') and 'uhma-asm' in file_path:
    rebuild_rag()

# Always output the memory reminder
print(prompt_memory_save())
