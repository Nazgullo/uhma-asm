#!/usr/bin/env python3
"""
failures.py — Structured failure tracking with automatic injection

@entry log_failure(file, action, error, context) -> None
@entry get_failures_for_file(file) -> list
@entry get_recent_failures(n=10) -> list
@entry clear_old_failures(days=7) -> int

@calls (standalone JSON storage)
@calledby hook.py (injection before Edit/Write)

STORAGE: tools/rag/memory/failures.json

FORMAT:
{
  "failures": [
    {
      "file": "emit.asm",
      "action": "Edit old_string not found",
      "error": "old_string not unique in file",
      "context": "trying to update header",
      "timestamp": "2026-02-01T10:30:00",
      "count": 3
    }
  ]
}

GOTCHAS:
  - Failures with same file+action+error get count incremented, not duplicated
  - Injected as BLOCKED warning before touching same file
  - Auto-expires after 7 days
"""

import json
from pathlib import Path
from datetime import datetime, timedelta
from typing import List, Dict, Optional

SCRIPT_DIR = Path(__file__).parent
FAILURES_FILE = SCRIPT_DIR / 'memory' / 'failures.json'

def _load() -> Dict:
    """Load failures from disk."""
    try:
        if FAILURES_FILE.exists():
            return json.loads(FAILURES_FILE.read_text())
    except:
        pass
    return {"failures": []}

def _save(data: Dict):
    """Save failures to disk."""
    FAILURES_FILE.parent.mkdir(exist_ok=True)
    FAILURES_FILE.write_text(json.dumps(data, indent=2))

def _normalize_file(file_path: str) -> str:
    """Normalize file path to just filename."""
    return Path(file_path).name if file_path else ""

def log_failure(file: str, action: str, error: str, context: str = "") -> None:
    """
    Log a failure. Increments count if similar failure exists.

    Args:
        file: File that was being modified
        action: What was attempted (e.g., "Edit old_string not found")
        error: The error message
        context: What the user was trying to do
    """
    data = _load()
    file = _normalize_file(file)
    now = datetime.now().isoformat()

    # Look for existing similar failure
    for f in data["failures"]:
        if f["file"] == file and f["action"] == action and f["error"] == error:
            f["count"] = f.get("count", 1) + 1
            f["timestamp"] = now
            f["context"] = context or f.get("context", "")
            _save(data)
            return

    # New failure
    data["failures"].append({
        "file": file,
        "action": action,
        "error": error,
        "context": context,
        "timestamp": now,
        "count": 1
    })

    # Keep last 100
    data["failures"] = data["failures"][-100:]
    _save(data)

def get_failures_for_file(file: str) -> List[Dict]:
    """Get all failures related to a specific file."""
    data = _load()
    file = _normalize_file(file)
    return [f for f in data["failures"] if f["file"] == file]

def get_recent_failures(n: int = 10) -> List[Dict]:
    """Get most recent failures."""
    data = _load()
    return sorted(data["failures"], key=lambda x: x.get("timestamp", ""), reverse=True)[:n]

def clear_old_failures(days: int = 7) -> int:
    """Remove failures older than N days. Returns count removed."""
    data = _load()
    cutoff = (datetime.now() - timedelta(days=days)).isoformat()
    original_count = len(data["failures"])
    data["failures"] = [f for f in data["failures"] if f.get("timestamp", "") > cutoff]
    _save(data)
    return original_count - len(data["failures"])

def format_failure_warning(failures: List[Dict]) -> str:
    """Format failures as injection warning."""
    if not failures:
        return ""

    lines = ["<FAILURE-HISTORY priority=\"HIGH\">",
             "PREVIOUS FAILURES on this file (don't repeat):"]

    for f in failures[:5]:
        count = f.get("count", 1)
        lines.append(f"  - [{count}x] {f['action']}: {f['error']}")
        if f.get("context"):
            lines.append(f"    Context: {f['context']}")

    lines.append("")
    lines.append("BEFORE editing, verify:")
    lines.append("  1. You've READ the file first")
    lines.append("  2. Your old_string EXACTLY matches file content")
    lines.append("  3. You're not repeating a known-failed approach")
    lines.append("</FAILURE-HISTORY>")

    return "\n".join(lines)

# CLI interface
if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        cmd = sys.argv[1]
        if cmd == "list":
            for f in get_recent_failures(20):
                print(f"[{f.get('count',1)}x] {f['file']}: {f['action']} - {f['error'][:50]}")
        elif cmd == "clear":
            n = clear_old_failures(0)  # Clear all
            print(f"Cleared {n} failures")
        elif cmd == "file" and len(sys.argv) > 2:
            for f in get_failures_for_file(sys.argv[2]):
                print(f"[{f.get('count',1)}x] {f['action']}: {f['error']}")
    else:
        print("Usage: failures.py [list|clear|file <name>]")
