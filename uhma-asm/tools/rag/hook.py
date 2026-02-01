#!/usr/bin/env python3
"""
hook.py — PreToolUse hook for Claude Code context injection + circuit breaker

@entry main()                          Called by Claude Code on every tool use
@entry inject_session_context() -> str Inject git status, recent sessions, learnings
@entry check_holo_trigger(msg) -> str  Detect "holo" in user message, save session
@entry reset_state()                   Reset circuit breaker state
@entry inject_file_context(file) -> str Inject summary + failures for file

@calls holo_memory.py:HoloMemory (session saves, context queries)
@calls failures.py:get_failures_for_file, format_failure_warning
@calls summaries.py:inject_summary
@calledby Claude Code PreToolUse hook (configured in .claude/settings.json)

CONFIG: ~/.claude/settings.json hooks.preToolUse = ["python3", "tools/rag/hook.py"]

CIRCUIT BREAKER:
  - Tracks (action, outcome) pairs with semantic similarity
  - Detects REPEATED FAILED ATTEMPTS at same thing (3+ failures)
  - Momentum: successes raise threshold, failures lower it
  - Warns before hard stop, allows one recovery chance
  - Reset: rm tools/rag/memory/.hook_state.json

TRIGGERS:
  - Session start: Injects git status, recent sessions, failed approaches
  - "holo" in message: Manual save to holographic memory
  - Auto-save: Every 30 min of activity

GOTCHAS:
  - Reads from stdin (JSON), writes additionalContext to stdout (JSON)
  - STATE_FILE tracks momentum/failures across tool calls
  - Only Edit/Bash outcomes count for loop detection (not Read/Grep)
"""
import sys
import json
import subprocess
import hashlib
from pathlib import Path
from datetime import datetime
from collections import Counter

SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent
MEMORY_DIR = SCRIPT_DIR / 'memory'
SESSION_MARKER = MEMORY_DIR / '.session_active'
STATE_FILE = MEMORY_DIR / '.hook_state.json'

# Thresholds
AUTO_SAVE_MINUTES = 30
LOOP_SIMILARITY_THRESHOLD = 0.8  # How similar must failed attempts be
MIN_FAILURES_FOR_LOOP = 3  # Minimum failures before loop detection
MOMENTUM_DECAY = 0.9  # How fast momentum decays

def log_debug(msg):
    try:
        with open(SCRIPT_DIR / 'hook_debug.log', 'a') as f:
            f.write(f"[{datetime.now().isoformat()}] {msg}\n")
    except:
        pass

# =============================================================================
# State Management
# =============================================================================

def load_state():
    """Load persistent state across tool calls."""
    default = {
        'session_start': datetime.now().isoformat(),
        'last_save': datetime.now().isoformat(),
        'tool_calls': 0,
        'momentum': 0.5,  # 0=stuck, 1=flowing. Starts neutral.
        'action_outcomes': [],  # [(action_hash, outcome_hash, success, timestamp), ...]
        'warning_issued': False,  # Warned before hard stop?
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
    """Persist state."""
    try:
        MEMORY_DIR.mkdir(exist_ok=True)
        STATE_FILE.write_text(json.dumps(state, indent=2))
    except Exception as e:
        log_debug(f"Failed to save state: {e}")

def reset_state():
    """Reset state for new session."""
    state = {
        'session_start': datetime.now().isoformat(),
        'last_save': datetime.now().isoformat(),
        'tool_calls': 0,
        'momentum': 0.5,
        'action_outcomes': [],
        'warning_issued': False,
        'hard_stopped': False
    }
    save_state(state)
    return state

# =============================================================================
# Semantic Hashing (lightweight similarity)
# =============================================================================

def semantic_hash(text, granularity=3):
    """
    Create a semantic hash that groups similar strings.
    Uses n-gram shingles for fuzzy matching.
    """
    if not text:
        return ""
    text = text.lower().strip()
    # Extract key tokens (remove noise words)
    noise = {'the', 'a', 'an', 'in', 'on', 'at', 'to', 'for', 'of', 'with', 'is', 'was', 'be'}
    tokens = [t for t in text.split() if t not in noise and len(t) > 2]
    # Create shingles
    shingles = set()
    joined = ' '.join(tokens[:20])  # Limit length
    for i in range(len(joined) - granularity + 1):
        shingles.add(joined[i:i+granularity])
    # Hash the sorted shingles
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
    """Create semantic signature of an error."""
    if not error_msg:
        return "success"
    return f"err:{semantic_hash(error_msg)}"

def signatures_similar(sig1, sig2):
    """Check if two signatures indicate the same thing."""
    if sig1 == sig2:
        return True
    # Compare hash parts
    parts1 = set(sig1.split('|'))
    parts2 = set(sig2.split('|'))
    if not parts1 or not parts2:
        return False
    overlap = len(parts1 & parts2) / max(len(parts1), len(parts2))
    return overlap >= LOOP_SIMILARITY_THRESHOLD

# =============================================================================
# Smart Circuit Breaker
# =============================================================================

def record_outcome(state, action_sig, outcome_sig, success):
    """Record an action-outcome pair."""
    state['action_outcomes'].append({
        'action': action_sig,
        'outcome': outcome_sig,
        'success': success,
        'time': datetime.now().isoformat()
    })
    # Keep last 20 only
    state['action_outcomes'] = state['action_outcomes'][-20:]

    # Update momentum
    if success:
        state['momentum'] = min(1.0, state['momentum'] + 0.1)
        state['warning_issued'] = False  # Reset warning on success
    else:
        state['momentum'] = max(0.0, state['momentum'] * MOMENTUM_DECAY - 0.05)

    return state

def detect_loop(state):
    """
    Detect if we're in a loop of similar failed attempts.
    Returns (is_loop, description) tuple.
    """
    outcomes = state.get('action_outcomes', [])
    failures = [o for o in outcomes if not o.get('success')]

    if len(failures) < MIN_FAILURES_FOR_LOOP:
        return False, None

    # Look at recent failures
    recent_failures = failures[-5:]

    # Group by similar action signatures
    action_groups = {}
    for f in recent_failures:
        action = f['action']
        found_group = False
        for key in action_groups:
            if signatures_similar(action, key):
                action_groups[key].append(f)
                found_group = True
                break
        if not found_group:
            action_groups[action] = [f]

    # Check if any group has repeated similar failures
    for action_key, group in action_groups.items():
        if len(group) >= MIN_FAILURES_FOR_LOOP:
            # Check if outcomes are also similar (same error repeating)
            outcome_counts = Counter(f['outcome'] for f in group)
            most_common_outcome, count = outcome_counts.most_common(1)[0]
            if count >= MIN_FAILURES_FOR_LOOP:
                return True, f"Same action '{action_key[:50]}' failing with same error {count}x"

    # Also check momentum - very low momentum = stuck
    if state.get('momentum', 0.5) < 0.1 and len(failures) >= 5:
        return True, f"Momentum critically low ({state['momentum']:.2f}) - no progress"

    return False, None

def check_circuit_breaker(state):
    """
    Smart circuit breaker with warning before hard stop.
    Returns injection string or None.
    """
    is_loop, description = detect_loop(state)

    if not is_loop:
        return None

    # First time: warning (allow recovery attempt)
    if not state.get('warning_issued'):
        state['warning_issued'] = True
        return generate_warning(description)

    # Second time: hard stop
    return generate_hard_stop("LOOP CONFIRMED", description)

def generate_warning(description):
    """Generate a warning - one chance to recover."""
    return f'''<CIRCUIT-BREAKER type="warning" priority="HIGH">
POTENTIAL LOOP DETECTED: {description}

You are showing signs of repeating failed attempts. Before continuing:

1. STOP and reflect: What have you tried? Why isn't it working?
2. Try a DIFFERENT approach - not a variation of the same thing
3. If stuck, ASK the user for guidance
4. Consider: Is this the right file/function? Is there missing context?

Your momentum is low. A success will reset this warning.
Another similar failure will trigger HARD STOP.

Take a breath. Think differently.
</CIRCUIT-BREAKER>'''

def generate_hard_stop(reason, details):
    """Generate HARD STOP - save and halt."""
    return f'''<HARD-STOP type="circuit-breaker" priority="EMERGENCY">
{reason}: {details}

AUTONOMOUS OPERATION HALTED - Loop confirmed after warning.

You MUST:
1. STOP all current work immediately
2. Save session state (summarize what you learned, what failed)
3. DO NOT make any more tool calls after saving
4. Wait for human input

Call mcp__uhma__mem_add with:
- category="warning"
- content="LOOP: [what you were trying] FAILED: [why it kept failing] TRIED: [approaches attempted]"
- context="HARD STOP {datetime.now().strftime('%Y-%m-%d %H:%M')}"

Then STOP.
</HARD-STOP>'''

# =============================================================================
# Transcript Reading (for holo trigger)
# =============================================================================

def get_transcript_path():
    """Find current session transcript."""
    try:
        project_dir = Path.home() / '.claude' / 'projects'
        cwd_encoded = str(PROJECT_ROOT).replace('/', '-')
        project_path = project_dir / cwd_encoded

        if not project_path.exists():
            log_debug(f"Project path not found: {project_path}")
            return None

        transcripts = sorted(project_path.glob('*.jsonl'),
                           key=lambda p: p.stat().st_mtime, reverse=True)
        return transcripts[0] if transcripts else None
    except Exception as e:
        log_debug(f"Transcript path error: {e}")
        return None

def get_last_user_message():
    """Read last user message from transcript."""
    transcript = get_transcript_path()
    if not transcript or not transcript.exists():
        return None

    try:
        lines = transcript.read_text().strip().split('\n')
        for line in reversed(lines[-100:]):
            try:
                entry = json.loads(line)
                if entry.get('type') == 'user':
                    msg = entry.get('message', {})
                    content = msg.get('content', '')
                    if isinstance(content, str):
                        return content
                    elif isinstance(content, list):
                        for item in content:
                            if isinstance(item, dict) and item.get('type') == 'tool_result':
                                break
                        else:
                            for item in content:
                                if isinstance(item, dict) and item.get('type') == 'text':
                                    return item.get('text', '')
            except:
                continue
        return None
    except Exception as e:
        log_debug(f"Transcript read error: {e}")
        return None

def check_holo_trigger():
    """Check if user said 'holo' - returns injection if so."""
    user_msg = get_last_user_message()
    if not user_msg:
        return None

    msg_lower = user_msg.lower().strip()
    if msg_lower == 'holo' or msg_lower.startswith('holo ') or msg_lower.startswith('holo:'):
        note = user_msg[4:].strip() if len(user_msg) > 4 else ""
        return generate_save_injection("MANUAL HOLO TRIGGER", note)
    return None

def check_auto_save(state):
    """Check if auto-save threshold reached."""
    try:
        last_save = datetime.fromisoformat(state['last_save'])
        elapsed = (datetime.now() - last_save).total_seconds() / 60
        if elapsed >= AUTO_SAVE_MINUTES:
            return generate_save_injection(
                f"AUTO-SAVE ({int(elapsed)} minutes elapsed)",
                "Periodic checkpoint"
            )
    except:
        pass
    return None

def generate_save_injection(trigger_reason, note=""):
    """Generate MANDATORY instruction to save session."""
    note_part = f"\nUser note: {note}" if note else ""

    return f'''<MANDATORY-ACTION type="holo-save" priority="CRITICAL">
{trigger_reason}{note_part}

You MUST NOW summarize this session and save it. Do this BEFORE any other action.

STEPS:
1. Summarize the ENTIRE session - what was investigated, discovered, fixed, decided
2. Include code examples where relevant (wrong vs right)
3. Compress to semantic meaning - NO noise like "read file X"

FORMAT your summary:
- INVESTIGATED: [problems looked at]
- ROOT CAUSE: [what was actually wrong]
- FIXED: [what was done, with code snippets if relevant]
- LEARNED: [gotchas, rules, insights]
- PENDING: [what's still broken/todo]

4. Call mcp__uhma__mem_add with:
   - category="session"
   - content=[your compressed summary]
   - context="session {datetime.now().strftime('%Y-%m-%d %H:%M')}"

5. For specific learnings, also save with category="insight" or "warning"

DO THIS NOW.
</MANDATORY-ACTION>'''

# =============================================================================
# Original Context Injection Functions
# =============================================================================

def get_git_status():
    """Get uncommitted changes."""
    try:
        result = subprocess.run(
            ['git', 'diff', '--stat', '*.asm'],
            cwd=PROJECT_ROOT, capture_output=True, text=True, timeout=5
        )
        asm_changes = result.stdout.strip()

        result2 = subprocess.run(
            ['git', 'status', '--short'],
            cwd=PROJECT_ROOT, capture_output=True, text=True, timeout=5
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
    """Get recent failed approaches."""
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

def get_recent_sessions():
    """Get recent session summaries to bring Claude up to speed."""
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from holo_memory import HoloMemory
        mem = HoloMemory()
        # Get recent session entries
        results = mem.query("session", category="session", limit=3, threshold=0.0)
        if results:
            lines = ["RECENT SESSIONS (what was happening):"]
            for entry, _ in results:
                # Show context (date) and content preview
                ctx = entry.context[:30] if entry.context else "unknown"
                lines.append(f"  [{ctx}] {entry.content[:150]}...")
            return '\n'.join(lines)
        return None
    except Exception as e:
        log_debug(f"Recent sessions error: {e}")
        return None

def get_recent_insights():
    """Get recent insights/learnings."""
    sys.path.insert(0, str(SCRIPT_DIR))
    try:
        from holo_memory import HoloMemory
        mem = HoloMemory()
        results = mem.query("insight warning", limit=5, threshold=0.0)
        if results:
            lines = ["RECENT LEARNINGS:"]
            for entry, _ in results:
                lines.append(f"  [{entry.category}] {entry.content[:100]}")
            return '\n'.join(lines)
        return None
    except Exception as e:
        log_debug(f"Recent insights error: {e}")
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
    """Inject context at session start - FORCE Claude to engage."""
    if SESSION_MARKER.exists():
        return None

    MEMORY_DIR.mkdir(exist_ok=True)
    SESSION_MARKER.write_text(datetime.now().isoformat())

    reset_state()

    parts = []

    # Priority 1: Recent sessions - what was happening
    if sessions := get_recent_sessions():
        parts.append(f"<recent-sessions>\n{sessions}\n</recent-sessions>")

    # Priority 2: Recent insights/learnings
    if insights := get_recent_insights():
        parts.append(f"<recent-learnings>\n{insights}\n</recent-learnings>")

    # Priority 3: Git status - current state
    if git := get_git_status():
        parts.append(f"<git-status>\n{git}\n</git-status>")

    # Priority 4: Failed approaches - don't repeat
    if failed := get_failed_approaches():
        parts.append(f"<failed-approaches>\n{failed}\n</failed-approaches>")

    # Priority 5: Cognitive warnings
    if cog := get_cognitive_state():
        parts.append(f"<cognitive-state>\n{cog}\n</cognitive-state>")

    if parts:
        header = """<MANDATORY-ACTION type="session-start" priority="CRITICAL">
YOU JUST STARTED A NEW SESSION. YOU MUST DO THE FOLLOWING BEFORE ANYTHING ELSE.

THIS IS NOT A SUGGESTION. THIS IS A REQUIREMENT. DO NOT SKIP THIS.

STEP 1: Read the context below
STEP 2: In your FIRST response, you MUST:
   - State what the last session was working on
   - State what is currently broken or pending
   - State what uncommitted changes exist
   - Ask the user what they want to do

DO NOT:
- Wait for user input first
- Say "let me check" and then use tools
- Ignore this and respond to whatever the user said
- Be passive

YOU SPEAK FIRST. YOU SUMMARIZE. YOU ENGAGE. NOW.
</MANDATORY-ACTION>"""
        return header + '\n\n' + '\n\n'.join(parts)
    return None

def inject_file_context(file_path):
    """Inject summary + failure history for a specific file."""
    parts = []

    # File summary (what this file does)
    try:
        from summaries import inject_summary
        if summary := inject_summary(file_path):
            parts.append(summary)
    except Exception as e:
        log_debug(f"Summary injection error: {e}")

    # Failure history (what went wrong before)
    try:
        from failures import get_failures_for_file, format_failure_warning
        failures = get_failures_for_file(file_path)
        if failures:
            warning = format_failure_warning(failures)
            if warning:
                parts.append(warning)
    except Exception as e:
        log_debug(f"Failure injection error: {e}")

    return '\n\n'.join(parts) if parts else None

def inject_for_modification(context):
    """Inject context for Edit/Write operations."""
    parts = []
    if cog := get_cognitive_state():
        parts.append(cog)
    if mem := get_relevant_memories(context):
        parts.append(mem)
    if parts:
        return '\n\n'.join(parts)
    return None

# =============================================================================
# Main
# =============================================================================

log_debug("Hook invoked")

try:
    hook_data = json.load(sys.stdin)
    log_debug(f"Tool: {hook_data.get('tool_name', 'unknown')}")
except Exception as e:
    log_debug(f"JSON parse error: {e}")
    sys.exit(0)

tool_name = hook_data.get('tool_name', '')
tool_input = hook_data.get('tool_input', {})
is_modifying = tool_name in {'Edit', 'Write', 'NotebookEdit', 'Bash'}
context = extract_context(hook_data)

# Load state
state = load_state()
state['tool_calls'] = state.get('tool_calls', 0) + 1

output_parts = []
session_ctx = None  # Will be set if this is session start

# Check if already hard stopped
if state.get('hard_stopped'):
    output_parts.append('''<HARD-STOP type="already-stopped" priority="EMERGENCY">
Session was previously HARD STOPPED due to confirmed loop.
DO NOT proceed. Wait for human to reset session.
Run: rm tools/rag/memory/.hook_state.json
</HARD-STOP>''')
else:
    # Priority 1: Circuit breaker (only for modifying tools)
    if is_modifying:
        if breaker := check_circuit_breaker(state):
            if 'HARD-STOP' in breaker:
                state['hard_stopped'] = True
            output_parts.insert(0, breaker)

    # Priority 2: Manual holo trigger
    if not output_parts:
        if holo := check_holo_trigger():
            state['last_save'] = datetime.now().isoformat()
            output_parts.insert(0, holo)

    # Priority 3: Auto-save
    if not output_parts:
        if auto := check_auto_save(state):
            state['last_save'] = datetime.now().isoformat()
            output_parts.insert(0, auto)

    # Session start injection
    if session_ctx := inject_session_start():
        output_parts.append(session_ctx)

    # Modification-specific injection
    if is_modifying and not state.get('hard_stopped'):
        if mod_ctx := inject_for_modification(context):
            output_parts.append(mod_ctx)

    # File-specific context (summary + failures) for Read/Edit/Write
    file_path = tool_input.get('file_path') or tool_input.get('path')
    if file_path and tool_name in {'Read', 'Edit', 'Write'}:
        if file_ctx := inject_file_context(file_path):
            output_parts.append(file_ctx)

# Save updated state
save_state(state)

# Determine if we need to BLOCK vs just inject context
should_block = False
block_reason = ""

# BLOCK on session start (first tool call) - force engagement
if state.get('tool_calls', 0) == 1 and session_ctx:
    should_block = True
    block_reason = f"""STOP. New session started - you must engage first.

Read this context, then respond to the user WITHOUT using tools:

{session_ctx}

Summarize what was happening, what's pending, and ask the user what they want.
DO NOT call any tools until you have engaged with the user."""

# BLOCK on hard stop
if state.get('hard_stopped'):
    should_block = True
    block_reason = "Session HARD STOPPED due to loop. Wait for user to reset."

# BLOCK only on MANUAL holo trigger (user typed "holo"), not auto-save
# Auto-save should inject context, not block
if check_holo_trigger():  # Only if user explicitly said "holo"
    should_block = True
    block_reason = f"""STOP. User triggered 'holo' - save session first.

{output_parts[0]}

Save your learnings before doing anything else."""

# Output
if should_block:
    log_debug(f"BLOCKING tool: {block_reason[:50]}...")
    print(json.dumps({
        "decision": "block",
        "reason": block_reason
    }))
elif output_parts:
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
