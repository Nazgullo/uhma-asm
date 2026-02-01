#!/usr/bin/env python3
"""
summaries.py — Pre-computed file summaries for instant context injection

@entry build_summaries() -> dict           Build summaries for all files
@entry get_summary(file) -> str            Get 3-line summary for file
@entry inject_summary(file) -> str         Format summary for injection

@calledby hook.py (before any file operation)

STORAGE: tools/rag/memory/summaries.json

Each summary is 3 lines max:
  Line 1: What this file does
  Line 2: Key functions/entry points
  Line 3: Critical gotcha (if any)

GOTCHAS:
  - Summaries extracted from @entry/@calls/GOTCHAS headers
  - Rebuild with: python3 summaries.py build
  - Falls back to filename if no header found
"""

import json
import re
from pathlib import Path
from typing import Dict, Optional

SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent
SUMMARIES_FILE = SCRIPT_DIR / 'memory' / 'summaries.json'

def _extract_header_info(file_path: Path) -> Dict:
    """Extract summary info from file header."""
    try:
        content = file_path.read_text()
        lines = content.split('\n')[:50]  # First 50 lines

        info = {
            "description": "",
            "entries": [],
            "gotchas": []
        }

        for line in lines:
            # Description (first comment line with —)
            if not info["description"] and '—' in line:
                match = re.search(r'[;#]\s*\w+\.\w+\s*—\s*(.+)', line)
                if match:
                    info["description"] = match.group(1).strip()

            # @entry lines
            if '@entry' in line:
                match = re.search(r'@entry\s+(\w+)\([^)]*\)', line)
                if match:
                    info["entries"].append(match.group(1))

            # GOTCHAS section
            if 'GOTCHAS:' in line or 'GOTCHA:' in line:
                # Get next few lines as gotchas
                idx = lines.index(line)
                for g_line in lines[idx+1:idx+5]:
                    if g_line.strip().startswith(('-', ';   -', '#   -')):
                        gotcha = re.sub(r'^[;#\s\-]+', '', g_line).strip()
                        if gotcha and len(gotcha) > 10:
                            info["gotchas"].append(gotcha)
                            break  # Just first gotcha

        return info
    except Exception as e:
        return {"description": "", "entries": [], "gotchas": []}

def build_summaries() -> Dict:
    """Build summaries for all project files."""
    summaries = {}

    # ASM files
    for asm in PROJECT_ROOT.glob('**/*.asm'):
        if 'test' in asm.name.lower():
            continue
        info = _extract_header_info(asm)

        summary_lines = []
        if info["description"]:
            summary_lines.append(info["description"][:100])
        else:
            summary_lines.append(f"{asm.stem}: (no description)")

        if info["entries"]:
            entries_str = ", ".join(info["entries"][:5])
            if len(info["entries"]) > 5:
                entries_str += f" (+{len(info['entries'])-5} more)"
            summary_lines.append(f"Entry: {entries_str}")

        if info["gotchas"]:
            summary_lines.append(f"GOTCHA: {info['gotchas'][0][:80]}")

        summaries[asm.name] = "\n".join(summary_lines)

    # Python files in tools/rag
    for py in (PROJECT_ROOT / 'tools' / 'rag').glob('*.py'):
        info = _extract_header_info(py)

        summary_lines = []
        if info["description"]:
            summary_lines.append(info["description"][:100])
        else:
            # Try docstring
            try:
                content = py.read_text()
                match = re.search(r'"""([^"]+)"""', content)
                if match:
                    first_line = match.group(1).strip().split('\n')[0]
                    summary_lines.append(first_line[:100])
            except:
                pass

        if not summary_lines:
            summary_lines.append(f"{py.stem}: (no description)")

        if info["entries"]:
            entries_str = ", ".join(info["entries"][:3])
            summary_lines.append(f"Entry: {entries_str}")

        summaries[py.name] = "\n".join(summary_lines)

    # Shell scripts
    for sh in PROJECT_ROOT.glob('*.sh'):
        info = _extract_header_info(sh)
        summary_lines = []
        if info["description"]:
            summary_lines.append(info["description"][:100])
        else:
            summary_lines.append(f"{sh.stem}: shell script")
        summaries[sh.name] = "\n".join(summary_lines)

    # Save
    SUMMARIES_FILE.parent.mkdir(exist_ok=True)
    SUMMARIES_FILE.write_text(json.dumps(summaries, indent=2))

    return summaries

def get_summary(file: str) -> Optional[str]:
    """Get summary for a file."""
    try:
        if SUMMARIES_FILE.exists():
            summaries = json.loads(SUMMARIES_FILE.read_text())
            # Try exact match
            if file in summaries:
                return summaries[file]
            # Try filename only
            fname = Path(file).name
            if fname in summaries:
                return summaries[fname]
    except:
        pass
    return None

def inject_summary(file: str) -> str:
    """Format summary for injection."""
    summary = get_summary(file)
    if not summary:
        return ""

    fname = Path(file).name
    return f"<file-summary file=\"{fname}\">\n{summary}\n</file-summary>"

# CLI
if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        cmd = sys.argv[1]
        if cmd == "build":
            summaries = build_summaries()
            print(f"Built {len(summaries)} summaries")
            for name, summary in list(summaries.items())[:5]:
                print(f"\n{name}:\n{summary}")
        elif cmd == "get" and len(sys.argv) > 2:
            s = get_summary(sys.argv[2])
            print(s if s else "No summary found")
        else:
            print("Usage: summaries.py [build|get <file>]")
    else:
        print("Usage: summaries.py [build|get <file>]")
