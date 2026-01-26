#!/usr/bin/env python3
"""
Build RAG index from .asm file headers.

Parses @entry, @calls, @calledby, GOTCHAS from standardized headers.
Outputs index.json with structured metadata for each file.

Usage: python build.py [--embeddings]
"""

import os
import re
import json
import hashlib
from pathlib import Path
from typing import Dict, List, Optional
from dataclasses import dataclass, asdict

ASM_DIR = Path(__file__).parent.parent.parent  # uhma-asm/
INDEX_FILE = Path(__file__).parent / "index.json"

@dataclass
class FileEntry:
    path: str
    description: str
    entries: List[Dict[str, str]]  # [{name, args, returns}]
    calls: List[str]               # ["file.asm:func", ...]
    called_by: List[str]           # ["file.asm:func", ...]
    flow: str
    state: List[str]
    gotchas: List[str]
    content_hash: str
    line_count: int

def parse_header(content: str) -> Dict:
    """Extract structured metadata from file header."""
    lines = content.split('\n')

    result = {
        'description': '',
        'entries': [],
        'calls': [],
        'called_by': [],
        'flow': '',
        'state': [],
        'gotchas': []
    }

    in_gotchas = False

    for line in lines[:80]:  # Only scan first 80 lines (header)
        line = line.strip()

        # Stop at section .data or .text
        if line.startswith('section ') or line.startswith('%include'):
            if not line.startswith('; '):
                break

        # Description (first line after filename)
        if line.startswith('; ') and ' — ' in line and not result['description']:
            result['description'] = line.split(' — ', 1)[1] if ' — ' in line else ''
            continue

        # @entry func(args) -> return
        match = re.match(r'^; @entry\s+(\w+)\(([^)]*)\)\s*(?:->)?\s*(.*)', line)
        if match:
            result['entries'].append({
                'name': match.group(1),
                'args': match.group(2).strip(),
                'returns': match.group(3).strip() if match.group(3) else 'void'
            })
            in_gotchas = False
            continue

        # @calls file.asm:func, file.asm:func
        match = re.match(r'^; @calls\s+(.+)', line)
        if match:
            calls = [c.strip() for c in match.group(1).split(',')]
            result['calls'].extend(calls)
            in_gotchas = False
            continue

        # @calledby file.asm:func
        match = re.match(r'^; @calledby\s+(.+)', line)
        if match:
            called_by = [c.strip() for c in match.group(1).split(',')]
            result['called_by'].extend(called_by)
            in_gotchas = False
            continue

        # FLOW: description
        match = re.match(r'^; FLOW:\s*(.+)', line)
        if match:
            result['flow'] = match.group(1)
            in_gotchas = False
            continue

        # STATE: field1, field2
        match = re.match(r'^; STATE:\s*(.+)', line)
        if match:
            state = [s.strip() for s in match.group(1).split(',')]
            result['state'].extend(state)
            in_gotchas = False
            continue

        # GOTCHAS:
        if line == '; GOTCHAS:':
            in_gotchas = True
            continue

        # Gotcha item
        if in_gotchas and line.startswith(';   - '):
            result['gotchas'].append(line[6:])
            continue

        # End of gotchas section
        if in_gotchas and line.startswith(';') and not line.startswith(';   '):
            in_gotchas = False

    return result

def build_index() -> Dict[str, FileEntry]:
    """Build index from all .asm files."""
    index = {}

    for asm_file in sorted(ASM_DIR.glob('*.asm')):
        # Skip test/scratch files
        if asm_file.name.startswith('test_') or asm_file.name == 'visualizer.asm':
            continue

        content = asm_file.read_text()
        content_hash = hashlib.md5(content.encode()).hexdigest()[:12]
        line_count = len(content.split('\n'))

        header = parse_header(content)

        entry = FileEntry(
            path=asm_file.name,
            description=header['description'],
            entries=header['entries'],
            calls=header['calls'],
            called_by=header['called_by'],
            flow=header['flow'],
            state=header['state'],
            gotchas=header['gotchas'],
            content_hash=content_hash,
            line_count=line_count
        )

        index[asm_file.name] = asdict(entry)

    return index

def build_reverse_index(index: Dict) -> Dict[str, List[str]]:
    """Build function -> file mapping."""
    func_to_file = {}

    for filename, data in index.items():
        for entry in data['entries']:
            func_name = entry['name']
            if func_name not in func_to_file:
                func_to_file[func_name] = []
            func_to_file[func_name].append(filename)

    return func_to_file

def build_dependency_graph(index: Dict) -> Dict[str, Dict]:
    """Build bidirectional dependency graph."""
    graph = {}

    for filename, data in index.items():
        graph[filename] = {
            'depends_on': set(),
            'depended_by': set()
        }

    for filename, data in index.items():
        for call in data['calls']:
            if ':' in call:
                target_file = call.split(':')[0]
                if target_file in graph:
                    graph[filename]['depends_on'].add(target_file)
                    graph[target_file]['depended_by'].add(filename)

    # Convert sets to lists for JSON
    for filename in graph:
        graph[filename]['depends_on'] = sorted(graph[filename]['depends_on'])
        graph[filename]['depended_by'] = sorted(graph[filename]['depended_by'])

    return graph

def main():
    print("Building RAG index...")

    index = build_index()
    func_index = build_reverse_index(index)
    dep_graph = build_dependency_graph(index)

    output = {
        'files': index,
        'functions': func_index,
        'dependencies': dep_graph,
        'meta': {
            'file_count': len(index),
            'function_count': len(func_index),
            'total_gotchas': sum(len(f['gotchas']) for f in index.values())
        }
    }

    INDEX_FILE.write_text(json.dumps(output, indent=2))
    print(f"Index written to {INDEX_FILE}")
    print(f"  Files: {output['meta']['file_count']}")
    print(f"  Functions: {output['meta']['function_count']}")
    print(f"  Gotchas: {output['meta']['total_gotchas']}")

if __name__ == '__main__':
    main()
