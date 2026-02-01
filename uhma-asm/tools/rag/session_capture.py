#!/usr/bin/env python3
"""
session_capture.py — 3-layer holographic session recording

@entry capture_session(transcript) -> None  Parse and store session
@entry extract_requests(text) -> list       Extract user requests
@entry extract_learnings(text) -> list      Extract findings/failures

@calls holo_memory.py:HoloMemory.add()
@calledby hook.py (on "holo" trigger or auto-save)

3 FIDELITY LAYERS:
  session_high: Brief session summary (what happened overall)
  session_mid:  User requests + key responses (the dialogue substance)
  session_low:  Findings/successes/failures (the learnings)

PARADIGM (from UHMA):
  encode → superpose → let decay handle forgetting
  NO regex compression - VSA encoding IS the compression
  Semantic search is FREE via cosine similarity

GOTCHAS:
  - Reads transcript from ~/.claude/projects/*/uuid.jsonl
  - Parses JSON lines format (one message per line)
  - Decay handles forgetting - no manual pruning needed
  - session category has 0.85 decay rate
"""

import sys
import json
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Tuple, Optional

SCRIPT_DIR = Path(__file__).parent
DEBUG_LOG = SCRIPT_DIR / 'capture_debug.log'
SESSION_MARKER = SCRIPT_DIR / 'memory' / '.session_active'

# Max entries per layer per session (prevent flooding)
MAX_HIGH = 1      # One summary per session
MAX_MID = 20      # Key exchanges
MAX_LOW = 30      # Detailed learnings


def log_debug(msg):
    try:
        with open(DEBUG_LOG, 'a') as f:
            f.write(f"[{datetime.now().isoformat()}] STENO: {msg}\n")
    except:
        pass


def get_memory():
    """Get holographic memory instance."""
    sys.path.insert(0, str(SCRIPT_DIR))
    from holo_memory import get_memory as gm
    return gm()


def extract_transcript(transcript_path: str) -> Dict:
    """Extract conversation from transcript into structured data."""
    data = {
        'user_messages': [],      # What user asked
        'assistant_texts': [],    # What I said (text only, not tool calls)
        'tool_actions': [],       # What I did (Edit, Write, Bash)
        'session_id': None,
        'start_time': None,
        'end_time': None,
    }

    transcript = Path(transcript_path)
    if not transcript.exists():
        log_debug(f"Transcript not found: {transcript_path}")
        return data

    try:
        lines = transcript.read_text().strip().split('\n')
    except Exception as e:
        log_debug(f"Read error: {e}")
        return data

    for line in lines:
        try:
            entry = json.loads(line.strip())
        except:
            continue

        if not isinstance(entry, dict):
            continue

        # Session metadata
        if not data['session_id']:
            data['session_id'] = entry.get('sessionId', '')[:12]

        timestamp = entry.get('timestamp', '')
        if timestamp:
            if not data['start_time']:
                data['start_time'] = timestamp
            data['end_time'] = timestamp

        entry_type = entry.get('type', '')

        # User messages - capture fully (these are instructions)
        if entry_type == 'user':
            msg = entry.get('message', {})
            content = msg.get('content', '')
            if isinstance(content, str) and len(content.strip()) > 10:
                # Skip tool results and system messages
                if not content.startswith('<') and not content.startswith('{'):
                    data['user_messages'].append(content.strip())
            elif isinstance(content, list):
                for item in content:
                    if isinstance(item, dict) and item.get('type') == 'text':
                        text = item.get('text', '').strip()
                        if len(text) > 10 and not text.startswith('<'):
                            data['user_messages'].append(text)

        # Assistant messages - capture text (not tool calls)
        elif entry_type == 'assistant':
            msg = entry.get('message', {})
            content = msg.get('content', [])

            if isinstance(content, str) and len(content.strip()) > 30:
                data['assistant_texts'].append(content.strip())
            elif isinstance(content, list):
                for item in content:
                    if isinstance(item, dict):
                        if item.get('type') == 'text':
                            text = item.get('text', '').strip()
                            if len(text) > 30:
                                data['assistant_texts'].append(text)
                        elif item.get('type') == 'tool_use':
                            # Track meaningful tool actions
                            tool_name = item.get('name', '')
                            tool_input = item.get('input', {})
                            if tool_name in ('Edit', 'Write', 'Bash'):
                                action = format_tool_action(tool_name, tool_input)
                                if action:
                                    data['tool_actions'].append(action)

    return data


def format_tool_action(tool_name: str, tool_input: Dict) -> Optional[str]:
    """Format tool action concisely."""
    if tool_name == 'Edit':
        fp = tool_input.get('file_path', '')
        fname = Path(fp).name if fp else '?'
        return f"Edited {fname}"
    elif tool_name == 'Write':
        fp = tool_input.get('file_path', '')
        fname = Path(fp).name if fp else '?'
        return f"Wrote {fname}"
    elif tool_name == 'Bash':
        cmd = tool_input.get('command', '')[:60]
        if cmd:
            return f"Ran: {cmd}"
    return None


def generate_session_summary(data: Dict) -> str:
    """Generate high-level session summary from data."""
    parts = []

    # What was the session about?
    if data['user_messages']:
        # First user message often sets the topic
        first_msg = data['user_messages'][0][:200]
        parts.append(f"Started with: {first_msg}")

    # What was done?
    if data['tool_actions']:
        unique_files = set()
        for action in data['tool_actions']:
            if 'Edited' in action or 'Wrote' in action:
                parts_action = action.split()
                if len(parts_action) >= 2:
                    unique_files.add(parts_action[1])
        if unique_files:
            parts.append(f"Modified: {', '.join(list(unique_files)[:5])}")

    # How many exchanges?
    parts.append(f"Exchanges: {len(data['user_messages'])} user, {len(data['assistant_texts'])} assistant")

    return " | ".join(parts) if parts else "Empty session"


def extract_learnings(assistant_texts: List[str]) -> List[Tuple[str, str]]:
    """
    Extract learnings from assistant text.
    Returns list of (category, content) tuples.

    NO regex compression - we store the actual substance and let
    holographic encoding handle similarity/compression.
    """
    learnings = []

    for text in assistant_texts:
        # Look for explicit statements (these are the substance)
        text_lower = text.lower()

        # Problems found
        if any(marker in text_lower for marker in ['the problem is', 'the issue is', 'the bug is', 'found that', 'discovered']):
            # Store the whole paragraph that contains the finding
            learnings.append(('finding', text[:500]))

        # Solutions applied
        if any(marker in text_lower for marker in ['fixed by', 'solved by', 'the fix is', 'changed to', 'updated to']):
            learnings.append(('success', text[:500]))

        # Things that didn't work
        if any(marker in text_lower for marker in ["didn't work", "failed", "still broken", "doesn't fix"]):
            learnings.append(('failed', text[:500]))

        # Insights/understanding
        if any(marker in text_lower for marker in ['because', 'the reason', 'this means', 'insight:']):
            learnings.append(('insight', text[:500]))

    return learnings


def store_holographic(data: Dict):
    """
    Store session data using 3-layer holographic approach.

    Layer 1 (session_high): One summary entry
    Layer 2 (session_mid): User requests + key assistant responses
    Layer 3 (session_low): Extracted learnings (findings, successes, failures)
    """
    mem = get_memory()
    session_id = data.get('session_id', 'unknown')
    ctx = f"session:{session_id}"
    stored = {'high': 0, 'mid': 0, 'low': 0}

    # === Layer 1: Session Summary (session_high) ===
    summary = generate_session_summary(data)
    if summary:
        mem.add(
            category='session',
            content=summary,
            context=ctx,
            source='session_capture:high'
        )
        stored['high'] = 1
        log_debug(f"HIGH: {summary[:80]}")

    # === Layer 2: User Requests + Key Tool Actions (session_mid) ===
    # Store user messages as requests
    for msg in data['user_messages'][:MAX_MID]:
        mem.add(
            category='request',
            content=msg[:500],
            context=ctx,
            source='session_capture:mid'
        )
        stored['mid'] += 1

    # Store significant tool actions
    for action in data['tool_actions'][:10]:
        mem.add(
            category='location',
            content=action,
            context=ctx,
            source='session_capture:mid'
        )
        stored['mid'] += 1

    log_debug(f"MID: {stored['mid']} entries")

    # === Layer 3: Learnings (session_low) ===
    learnings = extract_learnings(data['assistant_texts'])
    for category, content in learnings[:MAX_LOW]:
        mem.add(
            category=category,
            content=content,
            context=ctx,
            source='session_capture:low'
        )
        stored['low'] += 1

    log_debug(f"LOW: {stored['low']} entries")

    # Apply decay to all traces (natural forgetting)
    mem.decay_traces()

    log_debug(f"Stored: high={stored['high']}, mid={stored['mid']}, low={stored['low']}")
    return stored


def clear_session_marker():
    """Clear session marker for next session."""
    if SESSION_MARKER.exists():
        SESSION_MARKER.unlink()


# === Main execution ===
log_debug("Holographic session capture invoked")

try:
    hook_data = json.load(sys.stdin)
    log_debug(f"Hook data keys: {list(hook_data.keys())}")
except (json.JSONDecodeError, EOFError) as e:
    log_debug(f"No hook data: {e}")
    hook_data = {}

transcript_path = hook_data.get('transcript_path', '')

if transcript_path:
    log_debug(f"Capturing from: {transcript_path}")
    data = extract_transcript(transcript_path)
    store_holographic(data)
else:
    log_debug("No transcript_path provided")

clear_session_marker()
log_debug("Holographic session capture complete")
