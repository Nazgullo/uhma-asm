#!/usr/bin/env python3
"""
Context injection for Claude Code.

This module provides context that should be injected when:
1. About to edit a file
2. About to call a function
3. Searching for something

Can be used as:
- CLI: python context.py before-edit dispatch.asm
- MCP server endpoint
- Import in other tools

The output is designed to be injected into Claude's context
to prevent mistakes before they happen.
"""

import sys
import json
from pathlib import Path
from typing import Dict, List, Optional

INDEX_FILE = Path(__file__).parent / "index.json"

def load_index() -> Dict:
    if not INDEX_FILE.exists():
        return None
    return json.loads(INDEX_FILE.read_text())

def context_before_edit(filename: str) -> str:
    """
    Returns context to inject before editing a file.
    This is the CRITICAL function - it prevents mistakes.
    """
    index = load_index()
    if not index:
        return "RAG index not built. Run: python tools/rag/build.py"

    if not filename.endswith('.asm'):
        filename += '.asm'

    if filename not in index['files']:
        return f"Unknown file: {filename}"

    data = index['files'][filename]
    deps = index['dependencies'].get(filename, {})

    lines = [
        f"<file-context file=\"{filename}\">",
        f"DESCRIPTION: {data['description']}",
        ""
    ]

    # Entry points
    if data['entries']:
        lines.append("ENTRY POINTS:")
        for e in data['entries']:
            ret = f" -> {e['returns']}" if e['returns'] != 'void' else ''
            lines.append(f"  {e['name']}({e['args']}){ret}")

    # Flow
    if data['flow']:
        lines.append(f"\nFLOW: {data['flow']}")

    # State
    if data['state']:
        lines.append(f"\nSTATE: {', '.join(data['state'])}")

    # This file's gotchas - CRITICAL
    if data['gotchas']:
        lines.append("\nGOTCHAS (READ THESE):")
        for g in data['gotchas']:
            lines.append(f"  !! {g}")

    # Dependencies
    if deps.get('depends_on'):
        lines.append(f"\nCALLS INTO: {', '.join(deps['depends_on'])}")
    if deps.get('depended_by'):
        lines.append(f"\nCALLED FROM: {', '.join(deps['depended_by'])}")

    # Related gotchas - also critical
    related_gotchas = []
    for dep in deps.get('depends_on', []) + deps.get('depended_by', []):
        dep_data = index['files'].get(dep, {})
        for g in dep_data.get('gotchas', []):
            # Only include gotchas that might affect this file
            if any(keyword in g.lower() for keyword in [
                filename.replace('.asm', ''),
                'all files', 'caller', 'must', 'always', 'never'
            ]):
                related_gotchas.append((dep, g))

    if related_gotchas:
        lines.append("\nRELATED GOTCHAS (from dependencies):")
        for dep, g in related_gotchas:
            lines.append(f"  [{dep}] {g}")

    lines.append("</file-context>")

    return '\n'.join(lines)

def context_before_call(func_name: str) -> str:
    """
    Returns context to inject before calling a function.
    """
    index = load_index()
    if not index:
        return "RAG index not built."

    if func_name not in index['functions']:
        # Try partial match
        matches = [f for f in index['functions'] if func_name in f]
        if matches:
            return f"Function '{func_name}' not found. Similar: {', '.join(matches[:5])}"
        return f"Unknown function: {func_name}"

    files = index['functions'][func_name]
    lines = [f"<function-context func=\"{func_name}\">"]

    for filename in files:
        data = index['files'][filename]
        for e in data['entries']:
            if e['name'] == func_name:
                ret = f" -> {e['returns']}" if e['returns'] != 'void' else ''
                lines.append(f"SIGNATURE: {e['name']}({e['args']}){ret}")
                lines.append(f"DEFINED IN: {filename}")
                break

        # File's gotchas
        if data['gotchas']:
            lines.append(f"\nGOTCHAS ({filename}):")
            for g in data['gotchas']:
                lines.append(f"  !! {g}")

    lines.append("</function-context>")
    return '\n'.join(lines)

def context_search(query: str) -> str:
    """
    Search for files/functions matching a query.
    Returns relevant context.
    """
    index = load_index()
    if not index:
        return "RAG index not built."

    query_lower = query.lower()
    results = []

    # Search files
    for filename, data in index['files'].items():
        if query_lower in filename.lower() or query_lower in data['description'].lower():
            results.append(('file', filename, data['description']))

    # Search functions
    for func_name, files in index['functions'].items():
        if query_lower in func_name.lower():
            results.append(('func', func_name, f"in {', '.join(files)}"))

    # Search gotchas
    for filename, data in index['files'].items():
        for g in data['gotchas']:
            if query_lower in g.lower():
                results.append(('gotcha', filename, g))

    if not results:
        return f"No results for: {query}"

    lines = [f"<search-results query=\"{query}\">"]
    for type_, name, desc in results[:20]:
        lines.append(f"  [{type_}] {name}: {desc}")
    lines.append("</search-results>")

    return '\n'.join(lines)

def get_all_gotchas() -> str:
    """Return all gotchas in a format suitable for context injection."""
    index = load_index()
    if not index:
        return "RAG index not built."

    lines = ["<all-gotchas>"]
    for filename, data in sorted(index['files'].items()):
        if data['gotchas']:
            lines.append(f"\n{filename}:")
            for g in data['gotchas']:
                lines.append(f"  - {g}")
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
        return

    cmd = sys.argv[1]
    arg = sys.argv[2] if len(sys.argv) > 2 else ''

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
