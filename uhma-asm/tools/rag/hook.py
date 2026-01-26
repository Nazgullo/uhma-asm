#!/usr/bin/env python3
"""
Hook script for Claude Code preToolUse.

Called automatically before Edit/Write operations.
Outputs context if the file is a .asm file in UHMA.
"""

import sys
import os
from pathlib import Path

# Only process .asm files
if len(sys.argv) < 2:
    sys.exit(0)

file_path = sys.argv[1]
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
