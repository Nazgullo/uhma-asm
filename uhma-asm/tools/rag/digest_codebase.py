#!/usr/bin/env python3
"""
digest_codebase.py — Ingest entire UHMA codebase into holographic memory.

Creates multi-fidelity layers:
  - code_high: File-level (name, purpose, entry points)
  - code_mid: Function-level (signatures, what it does)
  - code_low: Implementation details (key code patterns)
"""

import sys
import re
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

sys.path.insert(0, str(SCRIPT_DIR))
from holo_memory import get_memory, CATEGORIES

def extract_asm_info(filepath: Path) -> dict:
    info = {'name': filepath.name, 'purpose': '', 'entries': [], 'functions': [], 'gotchas': [], 'state_fields': []}
    try:
        content = filepath.read_text()
    except:
        return info

    lines = content.split('\n')
    in_header = True
    current_func = None

    for i, line in enumerate(lines):
        if in_header and line.startswith(';'):
            if ' — ' in line and not info['purpose']:
                info['purpose'] = line.split(' — ', 1)[1].strip()
            if '@entry' in line:
                match = re.search(r'@entry\s+(\w+\([^)]*\))', line)
                if match:
                    info['entries'].append(match.group(1))
            if 'GOTCHA' in line.upper() or '!!' in line:
                info['gotchas'].append(line.lstrip('; ').strip())
        elif in_header:
            in_header = False

        if line.startswith('global '):
            current_func = {'name': line.split()[1].rstrip(':'), 'line': i + 1}
        elif current_func and ':' in line and not line.startswith((' ', '\t')):
            label = line.split(':')[0].strip()
            if label == current_func['name']:
                info['functions'].append(current_func)
                current_func = None

        if 'ST_' in line:
            info['state_fields'].extend(re.findall(r'ST_\w+', line))

    info['state_fields'] = list(set(info['state_fields']))
    return info

def extract_py_info(filepath: Path) -> dict:
    info = {'name': filepath.name, 'purpose': '', 'functions': [], 'classes': []}
    try:
        content = filepath.read_text()
    except:
        return info

    lines = content.split('\n')
    in_docstring = False
    docstring = []

    for line in lines[:30]:
        if '"""' in line:
            if in_docstring:
                break
            in_docstring = True
            after = line.split('"""', 1)[1]
            if '"""' in after:
                docstring.append(after.split('"""')[0])
                break
            docstring.append(after)
        elif in_docstring:
            docstring.append(line)

    info['purpose'] = ' '.join(docstring).strip()[:300]

    for i, line in enumerate(lines):
        if line.startswith('def '):
            match = re.match(r'def\s+(\w+)\s*\(([^)]*)\)', line)
            if match:
                info['functions'].append({'name': match.group(1), 'args': match.group(2), 'line': i + 1})
        elif line.startswith('class '):
            match = re.match(r'class\s+(\w+)', line)
            if match:
                info['classes'].append({'name': match.group(1), 'line': i + 1})

    return info

def digest_file(filepath: Path, mem):
    if filepath.suffix == '.asm':
        info = extract_asm_info(filepath)
    elif filepath.suffix == '.py':
        info = extract_py_info(filepath)
    else:
        return 0

    count = 0
    fname = info['name']

    # HIGH: File summary
    high = f"{fname}: {info['purpose']}"
    if info.get('entries'):
        high += f" | Entries: {', '.join(info['entries'][:5])}"
    mem.add('code_high', high[:500], context=f"file {fname}", source=fname)
    count += 1

    # MID: Functions
    for func in info.get('functions', [])[:20]:
        args = func.get('args', '')
        mid = f"{fname}:{func.get('line', '?')} {func['name']}({args})"
        mem.add('code_mid', mid[:300], context=f"function in {fname}", source=fname)
        count += 1

    # LOW: Gotchas
    for gotcha in info.get('gotchas', []):
        mem.add('code_low', f"{fname}: {gotcha}", context=f"gotcha in {fname}", source=fname)
        count += 1

    # LOW: State fields
    if info.get('state_fields'):
        fields = ', '.join(info['state_fields'][:10])
        mem.add('code_low', f"{fname} uses: {fields}", context=f"state in {fname}", source=fname)
        count += 1

    return count

def digest_codebase():
    mem = get_memory()
    total = 0

    asm_files = list(PROJECT_ROOT.glob('*.asm'))
    print(f"Found {len(asm_files)} .asm files")
    for fp in asm_files:
        count = digest_file(fp, mem)
        total += count
        print(f"  {fp.name}: {count} entries")

    py_files = list((PROJECT_ROOT / 'tools' / 'rag').glob('*.py'))
    print(f"\nFound {len(py_files)} .py files")
    for fp in py_files:
        count = digest_file(fp, mem)
        total += count
        print(f"  {fp.name}: {count} entries")

    print(f"\nTotal: {total} entries, Memory: {len(mem.entries)}")

if __name__ == '__main__':
    digest_codebase()
