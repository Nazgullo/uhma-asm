#!/usr/bin/env python3
"""
capture.py — PostToolUse hook for automatic memory capture.

Analyzes tool results and stores learnings in holographic memory:
- Edit/Write failures → 'failed' category
- Successful patterns → 'finding' category
- Error messages → extracted and stored
- File context → associated with entries

This is the WRITE side of memory. hook.py is the READ side.
"""

import sys
import json
import re
from pathlib import Path
from datetime import datetime

SCRIPT_DIR = Path(__file__).parent
DEBUG_LOG = SCRIPT_DIR / 'capture_debug.log'

def log_debug(msg):
    try:
        with open(DEBUG_LOG, 'a') as f:
            f.write(f"[{datetime.now().isoformat()}] {msg}\n")
    except:
        pass

def extract_error(text: str) -> str:
    """Extract error message from tool output."""
    if not text:
        return None

    # Common error patterns
    patterns = [
        r'error:\s*(.+?)(?:\n|$)',
        r'Error:\s*(.+?)(?:\n|$)',
        r'failed:\s*(.+?)(?:\n|$)',
        r'FAILED:\s*(.+?)(?:\n|$)',
        r'exception:\s*(.+?)(?:\n|$)',
        r'Traceback.*?(\w+Error: .+?)(?:\n|$)',
    ]

    for pattern in patterns:
        match = re.search(pattern, text, re.IGNORECASE | re.DOTALL)
        if match:
            return match.group(1).strip()[:200]

    return None

def extract_context(hook_data: dict) -> str:
    """Extract context from tool data."""
    tool_name = hook_data.get('tool_name', '')
    tool_input = hook_data.get('tool_input', {})

    parts = [tool_name]

    # File path
    file_path = tool_input.get('file_path') or tool_input.get('path', '')
    if file_path:
        parts.append(Path(file_path).name)

    # Command
    command = tool_input.get('command', '')
    if command:
        parts.append(command[:50])

    return ' '.join(parts)

def store_memory(category: str, content: str, context: str, source: str = None):
    """Store entry in holographic memory."""
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from holo_memory import get_memory
        mem = get_memory()
        entry = mem.add(category, content, context=context, source=source)
        log_debug(f"Stored [{category}]: {content[:50]}...")
        return entry.id
    except Exception as e:
        log_debug(f"Store failed: {e}")
        return None

def analyze_and_capture(hook_data: dict):
    """Analyze tool result and capture learnings."""
    tool_name = hook_data.get('tool_name', '')
    tool_input = hook_data.get('tool_input', {})
    tool_result = hook_data.get('tool_result', {})

    # Get result content
    result_content = ''
    if isinstance(tool_result, dict):
        result_content = tool_result.get('content', '')
        if isinstance(result_content, list):
            result_content = ' '.join(str(x) for x in result_content)
    elif isinstance(tool_result, str):
        result_content = tool_result

    context = extract_context(hook_data)
    file_path = tool_input.get('file_path') or tool_input.get('path', '')
    source = Path(file_path).name if file_path else tool_name

    # Check for errors
    error = extract_error(str(result_content))
    if error:
        store_memory('failed', error, context=context, source=source)
        return

    # Check for explicit failure indicators
    result_str = str(result_content).lower()
    if any(x in result_str for x in ['error', 'failed', 'exception', 'traceback']):
        # Extract meaningful part
        lines = str(result_content).split('\n')
        error_lines = [l for l in lines if any(x in l.lower() for x in ['error', 'failed', 'exception'])]
        if error_lines:
            store_memory('failed', error_lines[0][:200], context=context, source=source)
        return

    # For Edit operations - capture what was changed
    if tool_name == 'Edit':
        old_string = tool_input.get('old_string', '')[:100]
        new_string = tool_input.get('new_string', '')[:100]
        if old_string and new_string and 'success' in result_str:
            # Successful edit - store as finding if it looks significant
            if len(new_string) > 20:
                content = f"Changed in {source}: {old_string[:50]} → {new_string[:50]}"
                store_memory('finding', content, context=context, source=source)

    # For Bash - capture successful commands with interesting output
    elif tool_name == 'Bash':
        command = tool_input.get('command', '')
        if command and len(result_content) > 50:
            # Don't store trivial outputs
            if not any(x in command for x in ['echo', 'cat', 'ls']):
                # Store command patterns that worked
                if 'error' not in result_str and 'failed' not in result_str:
                    pass  # Don't auto-store all bash - too noisy

# Main execution
log_debug("Capture hook invoked")

try:
    hook_data = json.load(sys.stdin)
    log_debug(f"Received: {json.dumps(hook_data)[:300]}")
except (json.JSONDecodeError, EOFError) as e:
    log_debug(f"JSON parse error: {e}")
    sys.exit(0)

analyze_and_capture(hook_data)

# No output needed for PostToolUse capture
