#!/usr/bin/env python3
"""
session_capture.py — Stop hook for full session recording.

Reads the complete session transcript and stores in holographic memory:
- User requests (what was asked)
- Files modified (what changed)
- Key conversations (decisions, insights)
- Session summary

This ensures future Claude instances can "excavate" past sessions.
"""

import sys
import json
from pathlib import Path
from datetime import datetime

SCRIPT_DIR = Path(__file__).parent
DEBUG_LOG = SCRIPT_DIR / 'capture_debug.log'
SESSION_MARKER = SCRIPT_DIR / 'memory' / '.session_active'

def log_debug(msg):
    try:
        with open(DEBUG_LOG, 'a') as f:
            f.write(f"[{datetime.now().isoformat()}] SESSION_CAPTURE: {msg}\n")
    except:
        pass

def get_memory():
    """Get holographic memory instance."""
    sys.path.insert(0, str(SCRIPT_DIR))
    from holo_memory import get_memory as gm
    return gm()

def extract_session_data(transcript_path: str) -> dict:
    """Extract key data from session transcript."""
    data = {
        'user_messages': [],
        'files_modified': set(),
        'files_read': set(),
        'commands_run': [],
        'errors': [],
        'session_id': None,
        'start_time': None,
        'end_time': None,
    }

    transcript = Path(transcript_path)
    if not transcript.exists():
        log_debug(f"Transcript not found: {transcript_path}")
        return data

    with open(transcript, 'r') as f:
        for line in f:
            try:
                entry = json.loads(line.strip())
                if not isinstance(entry, dict):
                    continue
            except json.JSONDecodeError:
                continue

            # Session metadata
            if not data['session_id']:
                data['session_id'] = entry.get('sessionId')
            if not data['start_time']:
                data['start_time'] = entry.get('timestamp')
            data['end_time'] = entry.get('timestamp')

            # User messages (type == 'user')
            if entry.get('type') == 'user':
                msg = entry.get('message', {})
                if not isinstance(msg, dict):
                    continue
                content = msg.get('content')
                if isinstance(content, str) and len(content) > 10:
                    data['user_messages'].append(content[:500])
                elif isinstance(content, list):
                    for item in content:
                        if isinstance(item, dict) and item.get('type') == 'text':
                            text = item.get('text', '')
                            if len(text) > 10:
                                data['user_messages'].append(text[:500])

            # Tool results for files (successful edits/writes)
            tool_result = entry.get('toolUseResult')
            if isinstance(tool_result, dict):
                file_path = tool_result.get('filePath', '')
                if file_path and 'success' in str(tool_result).lower():
                    data['files_modified'].add(file_path)

            # Assistant tool_use entries
            if entry.get('type') == 'assistant':
                msg = entry.get('message', {})
                if not isinstance(msg, dict):
                    continue
                content = msg.get('content', [])
                if not isinstance(content, list):
                    continue

                for item in content:
                    if not isinstance(item, dict):
                        continue
                    if item.get('type') != 'tool_use':
                        continue

                    name = item.get('name', '')
                    inp = item.get('input', {})
                    if not isinstance(inp, dict):
                        continue

                    # Track reads
                    if name == 'Read':
                        fp = inp.get('file_path', '')
                        if fp:
                            data['files_read'].add(fp)

                    # Track edits/writes
                    elif name in ('Edit', 'Write'):
                        fp = inp.get('file_path', '')
                        if fp:
                            data['files_modified'].add(fp)

                    # Track commands
                    elif name == 'Bash':
                        cmd = inp.get('command', '')
                        if cmd and len(cmd) > 5:
                            data['commands_run'].append(cmd[:200])

    # Convert sets to lists
    data['files_modified'] = list(data['files_modified'])
    data['files_read'] = list(data['files_read'])

    return data

def create_session_summary(data: dict) -> str:
    """Create human-readable session summary."""
    parts = []

    # User requests
    if data['user_messages']:
        parts.append("USER REQUESTS:")
        for msg in data['user_messages'][:10]:  # Limit
            parts.append(f"  - {msg[:200]}")

    # Files modified
    if data['files_modified']:
        parts.append("\nFILES MODIFIED:")
        for f in data['files_modified'][:20]:
            parts.append(f"  - {Path(f).name}")

    # Key commands
    if data['commands_run']:
        unique_cmds = list(dict.fromkeys(data['commands_run']))[:10]
        parts.append("\nKEY COMMANDS:")
        for cmd in unique_cmds:
            parts.append(f"  - {cmd[:100]}")

    return '\n'.join(parts)

def store_session(data: dict):
    """Store session data in holographic memory."""
    mem = get_memory()
    session_id = data.get('session_id', 'unknown')[:12]

    # Store session summary
    summary = create_session_summary(data)
    if summary:
        mem.add(
            category='session',
            content=summary[:2000],
            context=f"session {session_id}",
            source=f"session:{session_id}"
        )
        log_debug(f"Stored session summary ({len(summary)} chars)")

    # Store individual user requests for better search
    for msg in data['user_messages'][:5]:
        mem.add(
            category='request',
            content=msg[:500],
            context=f"user request in session {session_id}",
            source=f"session:{session_id}"
        )

    # Store files modified as location entries
    for f in data['files_modified'][:10]:
        fname = Path(f).name
        mem.add(
            category='location',
            content=f"Modified: {fname}",
            context=f"file change in session {session_id}",
            source=fname
        )

    # Decay old traces to prevent saturation
    mem.decay_traces()

    log_debug(f"Session {session_id} captured: {len(data['user_messages'])} msgs, {len(data['files_modified'])} files")

def clear_session_marker():
    """Clear session marker for next session."""
    if SESSION_MARKER.exists():
        SESSION_MARKER.unlink()

# Main execution
log_debug("Session capture hook invoked")

try:
    hook_data = json.load(sys.stdin)
    log_debug(f"Received hook data: {list(hook_data.keys())}")
except (json.JSONDecodeError, EOFError) as e:
    log_debug(f"No hook data: {e}")
    hook_data = {}

# Get transcript path
transcript_path = hook_data.get('transcript_path', '')

if transcript_path:
    log_debug(f"Capturing from: {transcript_path}")
    session_data = extract_session_data(transcript_path)
    store_session(session_data)
else:
    log_debug("No transcript_path in hook data")

# Always clear session marker
clear_session_marker()

log_debug("Session capture complete")
