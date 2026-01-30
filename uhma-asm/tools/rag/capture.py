#!/usr/bin/env python3
"""
capture.py — PostToolUse hook for circuit breaker + memory capture.

Records action-outcome pairs for intelligent loop detection:
- Tracks (action_signature, outcome_signature, success) tuples
- Feeds hook.py circuit breaker via shared state file
- Stores semantic failures in holographic memory
"""

import sys
import json
import re
import hashlib
from pathlib import Path
from datetime import datetime

SCRIPT_DIR = Path(__file__).parent
MEMORY_DIR = SCRIPT_DIR / 'memory'
DEBUG_LOG = SCRIPT_DIR / 'capture_debug.log'
STATE_FILE = MEMORY_DIR / '.hook_state.json'

# Tools that matter for loop detection (not Read/Grep/Glob)
TRACKED_TOOLS = {'Edit', 'Write', 'NotebookEdit', 'Bash'}

def log_debug(msg):
    try:
        with open(DEBUG_LOG, 'a') as f:
            f.write(f"[{datetime.now().isoformat()}] {msg}\n")
    except:
        pass

# =============================================================================
# Semantic Hashing (must match hook.py)
# =============================================================================

def semantic_hash(text, granularity=3):
    """Create semantic hash for fuzzy matching."""
    if not text:
        return ""
    text = text.lower().strip()
    noise = {'the', 'a', 'an', 'in', 'on', 'at', 'to', 'for', 'of', 'with', 'is', 'was', 'be'}
    tokens = [t for t in text.split() if t not in noise and len(t) > 2]
    shingles = set()
    joined = ' '.join(tokens[:20])
    for i in range(len(joined) - granularity + 1):
        shingles.add(joined[i:i+granularity])
    return hashlib.md5('|'.join(sorted(shingles)).encode()).hexdigest()[:12]

def action_signature(tool_name, tool_input):
    """Create semantic signature of an action."""
    parts = [tool_name]
    if fp := tool_input.get('file_path') or tool_input.get('path'):
        parts.append(Path(fp).name)
    if old := tool_input.get('old_string'):
        parts.append(f"old:{semantic_hash(old)}")
    if new := tool_input.get('new_string'):
        parts.append(f"new:{semantic_hash(new)}")
    if cmd := tool_input.get('command'):
        parts.append(f"cmd:{semantic_hash(cmd)}")
    return '|'.join(parts)

def outcome_signature(error_msg):
    """Create semantic signature of an outcome."""
    if not error_msg:
        return "success"
    return f"err:{semantic_hash(error_msg)}"

# =============================================================================
# State Management
# =============================================================================

def load_state():
    """Load hook state."""
    default = {
        'momentum': 0.5,
        'action_outcomes': [],
        'warning_issued': False,
        'hard_stopped': False
    }
    try:
        if STATE_FILE.exists():
            state = json.loads(STATE_FILE.read_text())
            for k in default:
                if k not in state:
                    state[k] = default[k]
            return state
    except:
        pass
    return default

def save_state(state):
    """Save hook state."""
    try:
        MEMORY_DIR.mkdir(exist_ok=True)
        STATE_FILE.write_text(json.dumps(state, indent=2))
    except Exception as e:
        log_debug(f"Failed to save state: {e}")

def record_outcome(state, action_sig, outcome_sig, success):
    """Record action-outcome for circuit breaker."""
    state['action_outcomes'].append({
        'action': action_sig,
        'outcome': outcome_sig,
        'success': success,
        'time': datetime.now().isoformat()
    })
    state['action_outcomes'] = state['action_outcomes'][-20:]  # Keep last 20

    # Update momentum
    if success:
        state['momentum'] = min(1.0, state.get('momentum', 0.5) + 0.1)
        state['warning_issued'] = False
    else:
        state['momentum'] = max(0.0, state.get('momentum', 0.5) * 0.9 - 0.05)

    log_debug(f"Recorded: {action_sig[:30]} -> {outcome_sig} (momentum: {state['momentum']:.2f})")
    return state

# =============================================================================
# Error Extraction
# =============================================================================

def extract_error(text: str) -> str:
    """Extract error message from tool output."""
    if not text:
        return None

    patterns = [
        r'error:\s*(.+?)(?:\n|$)',
        r'Error:\s*(.+?)(?:\n|$)',
        r'failed:\s*(.+?)(?:\n|$)',
        r'FAILED:\s*(.+?)(?:\n|$)',
        r'exception:\s*(.+?)(?:\n|$)',
        r'Traceback.*?(\w+Error: .+?)(?:\n|$)',
        r'command not found',
        r'No such file or directory',
        r'Permission denied',
    ]

    for pattern in patterns:
        match = re.search(pattern, text, re.IGNORECASE | re.DOTALL)
        if match:
            return match.group(1).strip()[:200] if match.lastindex else match.group(0)[:200]

    return None

def is_failure(result_content: str) -> tuple:
    """Check if result indicates failure. Returns (is_failure, error_msg)."""
    if not result_content:
        return False, None

    result_str = str(result_content)

    # Extract specific error
    error = extract_error(result_str)
    if error:
        return True, error

    # Check for failure indicators
    lower = result_str.lower()
    if any(x in lower for x in ['error:', 'failed:', 'exception:', 'traceback']):
        lines = result_str.split('\n')
        for line in lines:
            if any(x in line.lower() for x in ['error', 'failed', 'exception']):
                return True, line[:200]

    return False, None

# =============================================================================
# Memory Storage
# =============================================================================

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

# =============================================================================
# Main Processing
# =============================================================================

def process_tool_result(hook_data: dict):
    """Process tool result: record outcome + optionally store memory."""
    tool_name = hook_data.get('tool_name', '')
    tool_input = hook_data.get('tool_input', {})
    tool_result = hook_data.get('tool_result', {})

    # Only track modifying tools for circuit breaker
    if tool_name not in TRACKED_TOOLS:
        return

    # Get result content
    result_content = ''
    if isinstance(tool_result, dict):
        result_content = tool_result.get('content', '')
        if isinstance(result_content, list):
            result_content = ' '.join(str(x) for x in result_content)
    elif isinstance(tool_result, str):
        result_content = tool_result

    # Determine if failure
    failed, error_msg = is_failure(result_content)

    # Create signatures
    action_sig = action_signature(tool_name, tool_input)
    outcome_sig = outcome_signature(error_msg if failed else None)

    # Record outcome for circuit breaker
    state = load_state()
    state = record_outcome(state, action_sig, outcome_sig, success=not failed)
    save_state(state)

    # Store failures in holographic memory
    if failed and error_msg:
        file_path = tool_input.get('file_path') or tool_input.get('path', '')
        source = Path(file_path).name if file_path else tool_name
        context = f"{tool_name} {source}"
        store_memory('failed', error_msg, context=context, source=source)

# =============================================================================
# Main
# =============================================================================

log_debug("Capture hook invoked")

try:
    hook_data = json.load(sys.stdin)
    log_debug(f"Tool: {hook_data.get('tool_name', 'unknown')}")
except (json.JSONDecodeError, EOFError) as e:
    log_debug(f"JSON parse error: {e}")
    sys.exit(0)

process_tool_result(hook_data)
