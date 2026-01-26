#!/usr/bin/env python3
"""
Hook script for Claude Code preToolUse.

Called automatically before Edit/Write/Read/Grep/Glob operations.
Reads tool input from stdin (JSON), extracts file path info,
and outputs context if targeting .asm files in UHMA.
"""

import sys
import json
from pathlib import Path

# Read tool input from stdin (preToolUse provides JSON)
try:
    tool_input = json.load(sys.stdin)
except (json.JSONDecodeError, EOFError):
    sys.exit(0)

# Extract file path from various tool formats
# Edit/Write/Read use 'file_path', Grep/Glob use 'path'
file_path = tool_input.get('file_path') or tool_input.get('path') or ''

# For Glob, check if pattern targets .asm files
pattern = tool_input.get('pattern', '')
if pattern and '.asm' in pattern and not file_path:
    # Glob searching for asm files - show general context
    script_dir = Path(__file__).parent
    sys.path.insert(0, str(script_dir))
    from context import get_all_gotchas
    gotchas = get_all_gotchas()
    if gotchas and 'not built' not in gotchas:
        print(gotchas)
    sys.exit(0)

if not file_path:
    sys.exit(0)

# Only process .asm files
if not file_path.endswith('.asm'):
    sys.exit(0)

# Only process files in uhma-asm directory
if 'uhma-asm' not in file_path:
    sys.exit(0)

# Get filename
filename = Path(file_path).name

# Import and run context
script_dir = Path(__file__).parent
sys.path.insert(0, str(script_dir))

from context import context_before_edit

context = context_before_edit(filename)
if context and 'Unknown file' not in context and 'not built' not in context:
    print(context)
