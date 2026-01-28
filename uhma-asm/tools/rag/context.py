#!/usr/bin/env python3
"""
Context injection for Claude Code - Holographic Memory Version.

Queries holographic memory for relevant context before:
1. Editing a file
2. Calling a function
3. Searching for something

All context now comes from the holographic memory traces,
not the static index.json.
"""

import sys
from pathlib import Path
from typing import Optional

# Import holographic memory
SCRIPT_DIR = Path(__file__).parent
sys.path.insert(0, str(SCRIPT_DIR))

from holo_memory import HoloMemory, get_memory


def context_before_edit(filename: str) -> str:
    """
    Returns context from holographic memory before editing a file.
    Queries for relevant gotchas, insights, and failures.
    """
    mem = get_memory()

    # Normalize filename
    if not filename.endswith('.asm'):
        filename += '.asm'

    # Build query from filename
    basename = Path(filename).stem
    query = f"{basename} edit gotcha warning"

    lines = [f"<file-context file=\"{filename}\">"]

    # Query holographic memory for relevant entries
    results = mem.query(query, limit=10, threshold=0.2)

    if results:
        # Group by category
        gotchas = [(e, s) for e, s in results if e.category == 'warning']
        findings = [(e, s) for e, s in results if e.category == 'finding']
        failures = [(e, s) for e, s in results if e.category == 'failed']
        successes = [(e, s) for e, s in results if e.category == 'success']

        if gotchas:
            lines.append("\nGOTCHAS (READ THESE):")
            for entry, sim in gotchas:
                lines.append(f"  !! {entry.content[:150]}")

        if failures:
            lines.append("\nPREVIOUS FAILURES (don't repeat):")
            for entry, sim in failures:
                lines.append(f"  - {entry.content[:150]}")

        if findings:
            lines.append("\nRELEVANT FINDINGS:")
            for entry, sim in findings:
                lines.append(f"  * {entry.content[:150]}")

        if successes:
            lines.append("\nWHAT WORKED:")
            for entry, sim in successes:
                lines.append(f"  + {entry.content[:150]}")
    else:
        lines.append("  (no specific context in holographic memory)")

    lines.append("</file-context>")
    return '\n'.join(lines)


def context_before_call(func_name: str) -> str:
    """
    Returns context from holographic memory before calling a function.
    """
    mem = get_memory()

    query = f"{func_name} function call"
    results = mem.query(query, limit=5, threshold=0.2)

    lines = [f"<function-context func=\"{func_name}\">"]

    if results:
        for entry, sim in results:
            icon = {'warning': '!!', 'failed': '-', 'finding': '*', 'success': '+'}.get(entry.category, '>')
            lines.append(f"  {icon} [{entry.category}] {entry.content[:100]}")
    else:
        lines.append("  (no specific context in holographic memory)")

    lines.append("</function-context>")
    return '\n'.join(lines)


def context_search(query: str) -> str:
    """
    Search holographic memory for relevant context.
    """
    mem = get_memory()
    results = mem.query(query, limit=15, threshold=0.15)

    if not results:
        return f"No results for: {query}"

    lines = [f"<search-results query=\"{query}\">"]
    for entry, sim in results:
        lines.append(f"  [{sim:.2f}] [{entry.category}] {entry.content[:80]}")
    lines.append("</search-results>")

    return '\n'.join(lines)


def get_all_gotchas() -> str:
    """Return all warnings/gotchas from holographic memory."""
    mem = get_memory()

    # Get all warning category entries
    warnings = mem.by_category('warning', limit=50)

    if not warnings:
        return "<all-gotchas>\n  (no gotchas in holographic memory)\n</all-gotchas>"

    lines = ["<all-gotchas>"]
    for entry in warnings:
        source = f"[{entry.source}] " if entry.source else ""
        lines.append(f"  - {source}{entry.content[:150]}")
    lines.append("</all-gotchas>")

    return '\n'.join(lines)


# CLI interface
def main():
    if len(sys.argv) < 2:
        print("Usage:")
        print("  python context.py before-edit <file>")
        print("  python context.py before-call <func>")
        print("  python context.py search <query>")
        print("  python context.py gotchas")
        print("\nAll queries now use holographic memory (not index.json)")
        return

    cmd = sys.argv[1]
    arg = ' '.join(sys.argv[2:]) if len(sys.argv) > 2 else ''

    if cmd == 'before-edit':
        print(context_before_edit(arg))
    elif cmd == 'before-call':
        print(context_before_call(arg))
    elif cmd == 'search':
        print(context_search(arg))
    elif cmd == 'gotchas':
        print(get_all_gotchas())
    else:
        print(f"Unknown command: {cmd}")


if __name__ == '__main__':
    main()
