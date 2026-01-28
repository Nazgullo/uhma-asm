#!/usr/bin/env python3
"""
Query holographic memory for context about files, functions, or concepts.

Usage:
  python query.py search <query>       # Search holographic memory
  python query.py gotchas [filter]     # List all gotchas/warnings
  python query.py recent [n]           # Show recent entries
  python query.py state                # Show cognitive state
  python query.py summary              # Full memory summary
  python query.py failed [context]     # Show failed approaches
  python query.py success [context]    # Show successful approaches

All queries now use holographic memory (not index.json).
"""

import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent
sys.path.insert(0, str(SCRIPT_DIR))

from holo_memory import HoloMemory, get_memory


def cmd_search(query: str):
    """Search holographic memory."""
    mem = get_memory()
    results = mem.query(query, limit=20, threshold=0.15)

    if not results:
        print(f"No results for: {query}")
        return

    print(f"=== Search: {query} ===\n")
    for entry, sim in results:
        print(f"[{sim:.2f}] [{entry.category}] {entry.content[:100]}")
        if entry.source:
            print(f"       source: {entry.source}")


def cmd_gotchas(filter_text: str = None):
    """List all gotchas/warnings."""
    mem = get_memory()
    warnings = mem.by_category('warning', limit=100)

    print("=== GOTCHAS ===\n")

    for entry in warnings:
        if filter_text and filter_text.lower() not in entry.content.lower():
            continue
        source = f"[{entry.source}] " if entry.source else ""
        print(f"- {source}{entry.content[:120]}")

    print(f"\nTotal: {len(warnings)} warnings")


def cmd_recent(n: int = 10):
    """Show recent entries."""
    mem = get_memory()
    entries = mem.recent(n)

    print(f"=== Recent {n} Entries ===\n")

    for entry in entries:
        icon = {'warning': '!!', 'failed': 'X', 'success': '+', 'finding': '*', 'insight': '!'}.get(entry.category, '>')
        print(f"{icon} [{entry.category}] {entry.content[:80]}")


def cmd_state():
    """Show cognitive state."""
    mem = get_memory()
    state = mem.get_state()

    print("=== Cognitive State ===\n")
    for k, v in state.items():
        print(f"  {k}: {v}")

    surface = mem.surface_stats()
    print("\n=== Surface Stats ===\n")
    for k, v in surface.items():
        print(f"  {k}: {v}")


def cmd_summary():
    """Full memory summary."""
    mem = get_memory()
    print(mem.summary())


def cmd_failed(context: str = None):
    """Show failed approaches."""
    mem = get_memory()
    entries = mem.failed_approaches(context, limit=20)

    print("=== Failed Approaches ===\n")
    for entry in entries:
        print(f"X {entry.content[:100]}")
        if entry.context:
            print(f"  context: {entry.context[:50]}")


def cmd_success(context: str = None):
    """Show successful approaches."""
    mem = get_memory()
    entries = mem.successful_approaches(context, limit=20)

    print("=== Successful Approaches ===\n")
    for entry in entries:
        print(f"+ {entry.content[:100]}")
        if entry.context:
            print(f"  context: {entry.context[:50]}")


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        return

    cmd = sys.argv[1]
    arg = ' '.join(sys.argv[2:]) if len(sys.argv) > 2 else None

    if cmd == 'search' and arg:
        cmd_search(arg)
    elif cmd == 'gotchas':
        cmd_gotchas(arg)
    elif cmd == 'recent':
        n = int(arg) if arg and arg.isdigit() else 10
        cmd_recent(n)
    elif cmd == 'state':
        cmd_state()
    elif cmd == 'summary':
        cmd_summary()
    elif cmd == 'failed':
        cmd_failed(arg)
    elif cmd == 'success':
        cmd_success(arg)
    else:
        print(__doc__)


if __name__ == '__main__':
    main()
