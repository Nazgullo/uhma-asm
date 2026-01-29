#!/usr/bin/env python3
"""
session_capture.py — Stop hook for SEMANTIC session recording.

Extracts meaningful learnings from conversation, not just file touches:
- Bugs mentioned → 'bug' category
- Fixes applied → 'fix' category
- Insights/gotchas → 'insight' category
- Failed approaches → 'failed' category
- Successful patterns → 'success' category

This ensures future Claude instances learn from past sessions.
"""

import sys
import json
import re
from pathlib import Path
from datetime import datetime

SCRIPT_DIR = Path(__file__).parent
DEBUG_LOG = SCRIPT_DIR / 'capture_debug.log'
SESSION_MARKER = SCRIPT_DIR / 'memory' / '.session_active'

# Patterns for semantic extraction
BUG_PATTERNS = [
    r'bug[:\s]+(.{20,200})',
    r'error[:\s]+(.{20,200})',
    r'problem[:\s]+(.{20,200})',
    r'issue[:\s]+(.{20,200})',
    r'broken[:\s]+(.{20,200})',
    r'corrupted[:\s]+(.{20,200})',
    r'wrong[:\s]+(.{20,200})',
    r"doesn't work[:\s]*(.{10,200})",
    r"isn't working[:\s]*(.{10,200})",
    r'INT64_MIN|INT64_MAX|uninitialized|segfault|crash',
]

FIX_PATTERNS = [
    r'fix[:\s]+(.{20,200})',
    r'fixed[:\s]+(.{20,200})',
    r'solution[:\s]+(.{20,200})',
    r'resolved[:\s]+(.{20,200})',
    r'the fix is[:\s]+(.{20,200})',
    r'to fix this[:\s]+(.{20,200})',
    r'changed.*?to.*?(?:fix|solve|resolve)',
]

INSIGHT_PATTERNS = [
    r'gotcha[:\s]+(.{20,200})',
    r'learned[:\s]+(.{20,200})',
    r'insight[:\s]+(.{20,200})',
    r'key insight[:\s]+(.{20,200})',
    r'important[:\s]+(.{20,200})',
    r'remember[:\s]+(.{20,200})',
    r'always (.{20,100})',
    r'never (.{20,100})',
    r'must (.{20,100})',
]

FAILED_PATTERNS = [
    r"didn't work[:\s]*(.{10,200})",
    r"doesn't help[:\s]*(.{10,200})",
    r'failed approach[:\s]+(.{20,200})',
    r'wrong approach[:\s]+(.{20,200})',
    r'that was wrong[:\s]*(.{10,200})',
]

def log_debug(msg):
    try:
        with open(DEBUG_LOG, 'a') as f:
            f.write(f"[{datetime.now().isoformat()}] SESSION_CAPTURE: {msg}\n")
    except:
        pass

def extract_semantic_content(text: str) -> list:
    """Extract bugs, fixes, insights from text."""
    findings = []
    text_lower = text.lower()

    # Extract bugs
    for pattern in BUG_PATTERNS:
        for match in re.finditer(pattern, text, re.IGNORECASE):
            content = match.group(1) if match.lastindex else match.group(0)
            content = content.strip()[:200]
            if len(content) > 15:
                findings.append(('bug', content))

    # Extract fixes
    for pattern in FIX_PATTERNS:
        for match in re.finditer(pattern, text, re.IGNORECASE):
            content = match.group(1) if match.lastindex else match.group(0)
            content = content.strip()[:200]
            if len(content) > 15:
                findings.append(('fix', content))

    # Extract insights
    for pattern in INSIGHT_PATTERNS:
        for match in re.finditer(pattern, text, re.IGNORECASE):
            content = match.group(1) if match.lastindex else match.group(0)
            content = content.strip()[:200]
            if len(content) > 15:
                findings.append(('insight', content))

    # Extract failed approaches
    for pattern in FAILED_PATTERNS:
        for match in re.finditer(pattern, text, re.IGNORECASE):
            content = match.group(1) if match.lastindex else match.group(0)
            content = content.strip()[:200]
            if len(content) > 15:
                findings.append(('failed', content))

    # Deduplicate by content similarity
    seen = set()
    unique = []
    for cat, content in findings:
        key = content[:50].lower()
        if key not in seen:
            seen.add(key)
            unique.append((cat, content))

    return unique[:20]  # Limit per session

def get_memory():
    """Get holographic memory instance."""
    sys.path.insert(0, str(SCRIPT_DIR))
    from holo_memory import get_memory as gm
    return gm()

def extract_session_data(transcript_path: str) -> dict:
    """Extract key data from session transcript."""
    data = {
        'user_messages': [],
        'assistant_messages': [],  # NEW: capture assistant text for semantic extraction
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

            # Assistant messages - extract text for semantic analysis
            if entry.get('type') == 'assistant':
                msg = entry.get('message', {})
                if not isinstance(msg, dict):
                    continue
                content = msg.get('content', [])
                if isinstance(content, str) and len(content) > 20:
                    data['assistant_messages'].append(content[:1000])
                elif isinstance(content, list):
                    for item in content:
                        if isinstance(item, dict) and item.get('type') == 'text':
                            text = item.get('text', '')
                            if len(text) > 20:
                                data['assistant_messages'].append(text[:1000])

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
    """Store session data in holographic memory with SEMANTIC extraction."""
    mem = get_memory()
    session_id = data.get('session_id', 'unknown')[:12]

    # Combine all text for semantic extraction
    all_text = '\n'.join(data.get('user_messages', []))
    all_text += '\n' + '\n'.join(data.get('assistant_messages', []))

    # SEMANTIC EXTRACTION - the important part!
    semantic_findings = extract_semantic_content(all_text)
    for category, content in semantic_findings:
        mem.add(
            category=category,
            content=content,
            context=f"session {session_id}",
            source=f"session:{session_id}"
        )
        log_debug(f"Stored [{category}]: {content[:50]}...")

    # Store session summary (but briefer)
    summary = create_session_summary(data)
    if summary and len(semantic_findings) == 0:
        # Only store raw summary if no semantic content found
        mem.add(
            category='session',
            content=summary[:1000],
            context=f"session {session_id}",
            source=f"session:{session_id}"
        )

    # Store user requests ONLY if they look like tasks (not chit-chat)
    task_keywords = ['fix', 'add', 'create', 'implement', 'debug', 'find', 'check', 'update', 'change']
    for msg in data['user_messages'][:5]:
        msg_lower = msg.lower()
        if any(kw in msg_lower for kw in task_keywords) and len(msg) > 20:
            mem.add(
                category='request',
                content=msg[:300],
                context=f"task request in session {session_id}",
                source=f"session:{session_id}"
            )

    # DON'T store "Modified: filename" spam - useless
    # Instead, store files only if they had significant changes
    if len(data.get('files_modified', [])) <= 3:
        for f in data['files_modified']:
            fname = Path(f).name
            mem.add(
                category='location',
                content=f"Modified: {fname}",
                context=f"session {session_id}",
                source=fname
            )

    # Decay old traces to prevent saturation
    mem.decay_traces()

    log_debug(f"Session {session_id}: {len(semantic_findings)} semantic, {len(data.get('user_messages',[]))} msgs")

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
