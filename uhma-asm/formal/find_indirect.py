#!/usr/bin/env python3
"""
find_indirect.py — Find indirect calls/jumps (potential crash sources)

@entry find_indirect_calls(asm_dir) -> list  Scan all .asm for indirect jmp/call
@entry main()                                CLI: prints results

@calledby CLI (python3 formal/find_indirect.py)

DETECTS:
  - call rax, call [rbx+8], call qword [rdi]
  - jmp rax, jmp [table+rcx*8]

GOTCHAS:
  - Indirect calls crash if target corrupted by self-modification
  - Each result shows: file:line, function, instruction
"""

import os
import re

def find_indirect_calls(asm_dir):
    """Find all indirect call/jmp instructions"""
    results = []

    for fname in sorted(os.listdir(asm_dir)):
        if not fname.endswith('.asm'):
            continue
        filepath = os.path.join(asm_dir, fname)

        with open(filepath, 'r') as f:
            lines = f.readlines()

        current_func = None
        for i, line in enumerate(lines, 1):
            orig_line = line
            line = line.strip()

            # Track function context
            if line.endswith(':') and not line.startswith('.') and not line.startswith('%'):
                current_func = line[:-1]

            # Skip comments and directives
            if not line or line.startswith(';') or line.startswith('%'):
                continue

            # Remove inline comments
            if ';' in line:
                line = line.split(';')[0].strip()

            parts = line.replace(',', ' ').split()
            if not parts:
                continue

            op = parts[0].lower()
            args = parts[1:] if len(parts) > 1 else []

            # Check for indirect call
            if op == 'call' and args:
                target = args[0]
                if target.startswith('r') or target.startswith('['):
                    results.append({
                        'file': fname,
                        'line': i,
                        'func': current_func,
                        'type': 'call',
                        'target': target,
                        'context': orig_line.strip()
                    })

            # Check for indirect jmp (not conditional branches)
            if op == 'jmp' and args:
                target = args[0]
                if target.startswith('r') or target.startswith('['):
                    results.append({
                        'file': fname,
                        'line': i,
                        'func': current_func,
                        'type': 'jmp',
                        'target': target,
                        'context': orig_line.strip()
                    })

    return results


def analyze_call_sources(results):
    """Analyze where the call target register comes from"""
    print("="*70)
    print("INDIRECT CALL/JMP ANALYSIS")
    print("="*70)

    for r in results:
        print(f"\n{r['file']}:{r['line']} in {r['func'] or '?'}")
        print(f"  {r['type']} {r['target']}")
        print(f"  Context: {r['context']}")

        # Flag high-risk patterns
        target = r['target']
        if target in ('rdx', 'rax', 'rcx', 'rdi', 'rsi'):
            print(f"  [!] CALLER-SAVED register - value may be clobbered")
        if '[' in target:
            print(f"  [!] MEMORY-INDIRECT - value depends on memory contents")

    print(f"\n{'='*70}")
    print(f"Total indirect calls/jumps: {len(results)}")
    print(f"  Calls: {len([r for r in results if r['type'] == 'call'])}")
    print(f"  Jumps: {len([r for r in results if r['type'] == 'jmp'])}")


if __name__ == "__main__":
    import sys
    asm_dir = sys.argv[1] if len(sys.argv) > 1 else "."
    results = find_indirect_calls(asm_dir)
    analyze_call_sources(results)
