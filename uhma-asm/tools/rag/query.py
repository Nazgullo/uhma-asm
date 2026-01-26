#!/usr/bin/env python3
"""
Query the RAG index for context about files, functions, or concepts.

Usage:
  python query.py file dispatch.asm      # Get context for a file
  python query.py func process_token     # Get context for a function
  python query.py deps dispatch.asm      # Get dependency chain
  python query.py gotchas                # List all gotchas
  python query.py related dispatch.asm   # Files related to this one
  python query.py edit dispatch.asm      # Context needed before editing
"""

import sys
import json
from pathlib import Path
from typing import Dict, List, Set

INDEX_FILE = Path(__file__).parent / "index.json"

def load_index() -> Dict:
    if not INDEX_FILE.exists():
        print("Index not found. Run: python build.py")
        sys.exit(1)
    return json.loads(INDEX_FILE.read_text())

def format_file_context(data: Dict, filename: str) -> str:
    """Format file context for display."""
    lines = [f"=== {filename} ==="]
    lines.append(f"Description: {data['description']}")
    lines.append(f"Lines: {data['line_count']}")
    lines.append("")

    if data['entries']:
        lines.append("ENTRY POINTS:")
        for e in data['entries']:
            ret = f" -> {e['returns']}" if e['returns'] != 'void' else ''
            lines.append(f"  {e['name']}({e['args']}){ret}")
        lines.append("")

    if data['flow']:
        lines.append(f"FLOW: {data['flow']}")
        lines.append("")

    if data['state']:
        lines.append(f"STATE: {', '.join(data['state'])}")
        lines.append("")

    if data['calls']:
        lines.append("CALLS:")
        for c in data['calls']:
            lines.append(f"  {c}")
        lines.append("")

    if data['called_by']:
        lines.append("CALLED BY:")
        for c in data['called_by']:
            lines.append(f"  {c}")
        lines.append("")

    if data['gotchas']:
        lines.append("GOTCHAS:")
        for g in data['gotchas']:
            lines.append(f"  - {g}")
        lines.append("")

    return '\n'.join(lines)

def cmd_file(index: Dict, filename: str):
    """Get context for a file."""
    if not filename.endswith('.asm'):
        filename += '.asm'

    if filename not in index['files']:
        print(f"File not found: {filename}")
        print(f"Available: {', '.join(sorted(index['files'].keys()))}")
        return

    print(format_file_context(index['files'][filename], filename))

def cmd_func(index: Dict, func_name: str):
    """Get context for a function."""
    if func_name not in index['functions']:
        # Try partial match
        matches = [f for f in index['functions'] if func_name in f]
        if matches:
            print(f"Function '{func_name}' not found. Did you mean:")
            for m in matches[:10]:
                print(f"  {m}")
        else:
            print(f"Function '{func_name}' not found.")
        return

    files = index['functions'][func_name]
    print(f"Function: {func_name}")
    print(f"Defined in: {', '.join(files)}")
    print("")

    for filename in files:
        data = index['files'][filename]
        for e in data['entries']:
            if e['name'] == func_name:
                ret = f" -> {e['returns']}" if e['returns'] != 'void' else ''
                print(f"Signature: {e['name']}({e['args']}){ret}")
                break

        if data['gotchas']:
            print(f"\nGOTCHAS ({filename}):")
            for g in data['gotchas']:
                print(f"  - {g}")

def cmd_deps(index: Dict, filename: str):
    """Get dependency chain for a file."""
    if not filename.endswith('.asm'):
        filename += '.asm'

    if filename not in index['dependencies']:
        print(f"File not found: {filename}")
        return

    deps = index['dependencies'][filename]

    print(f"=== Dependencies for {filename} ===\n")

    print("DEPENDS ON (calls into):")
    if deps['depends_on']:
        for d in deps['depends_on']:
            print(f"  {d}")
    else:
        print("  (none)")

    print("\nDEPENDED BY (called from):")
    if deps['depended_by']:
        for d in deps['depended_by']:
            print(f"  {d}")
    else:
        print("  (none)")

def cmd_gotchas(index: Dict, filename: str = None):
    """List all gotchas, optionally filtered by file."""
    print("=== GOTCHAS ===\n")

    for fname, data in sorted(index['files'].items()):
        if filename and filename not in fname:
            continue
        if data['gotchas']:
            print(f"{fname}:")
            for g in data['gotchas']:
                print(f"  - {g}")
            print("")

def cmd_related(index: Dict, filename: str):
    """Get files related to this one (deps + reverse deps)."""
    if not filename.endswith('.asm'):
        filename += '.asm'

    if filename not in index['dependencies']:
        print(f"File not found: {filename}")
        return

    deps = index['dependencies'][filename]
    related = set(deps['depends_on']) | set(deps['depended_by'])

    print(f"=== Files related to {filename} ===\n")

    for r in sorted(related):
        data = index['files'].get(r, {})
        desc = data.get('description', '')
        print(f"{r}: {desc}")

def cmd_edit(index: Dict, filename: str):
    """Get all context needed before editing a file."""
    if not filename.endswith('.asm'):
        filename += '.asm'

    if filename not in index['files']:
        print(f"File not found: {filename}")
        return

    print(f"=== CONTEXT FOR EDITING {filename} ===\n")

    # File itself
    data = index['files'][filename]
    print(format_file_context(data, filename))

    # Dependencies' gotchas
    deps = index['dependencies'].get(filename, {})

    related_gotchas = []
    for dep in deps.get('depends_on', []) + deps.get('depended_by', []):
        dep_data = index['files'].get(dep, {})
        for g in dep_data.get('gotchas', []):
            related_gotchas.append((dep, g))

    if related_gotchas:
        print("=== RELATED GOTCHAS (from dependencies) ===\n")
        for dep, g in related_gotchas:
            print(f"[{dep}] {g}")
        print("")

def main():
    if len(sys.argv) < 2:
        print(__doc__)
        return

    index = load_index()
    cmd = sys.argv[1]
    arg = sys.argv[2] if len(sys.argv) > 2 else None

    if cmd == 'file' and arg:
        cmd_file(index, arg)
    elif cmd == 'func' and arg:
        cmd_func(index, arg)
    elif cmd == 'deps' and arg:
        cmd_deps(index, arg)
    elif cmd == 'gotchas':
        cmd_gotchas(index, arg)
    elif cmd == 'related' and arg:
        cmd_related(index, arg)
    elif cmd == 'edit' and arg:
        cmd_edit(index, arg)
    else:
        print(__doc__)

if __name__ == '__main__':
    main()
