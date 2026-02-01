"""
parser.py — NASM Assembly Parser: .asm to structured representation

@entry NASMParser.parse_file(path) -> ParsedFile
@entry NASMParser.parse_function(lines) -> Function
@entry NASMParser.parse_instruction(line) -> Instruction

@calledby analyze.py, symbolic.py

STRUCTURES:
  ParsedFile: filename, functions[], globals[], externs[]
  Function: name, blocks[], entry_block, is_global
  BasicBlock: label, instructions[], successors[]
  Instruction: mnemonic, operands[], type (JUMP/CALL/RET/etc)

GOTCHAS:
  - Functions defined as: global label → ... → ret
  - Basic blocks end at: jmp, jcc, ret, or next label
  - Handles NASM syntax: section, %include, extern, global
"""

import re
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple, Set
from enum import Enum, auto

class InstrType(Enum):
    """Instruction categories for control flow analysis"""
    NORMAL = auto()      # Regular instruction, fall through
    JUMP = auto()        # Unconditional jump (jmp)
    COND_JUMP = auto()   # Conditional jump (je, jne, jg, etc.)
    CALL = auto()        # Function call
    RET = auto()         # Return
    PUSH = auto()        # Push to stack
    POP = auto()         # Pop from stack
    MOV = auto()         # Move (for register tracking)
    LABEL = auto()       # Not an instruction, but a label
    DIRECTIVE = auto()   # Assembler directive
    NOP = auto()         # No operation

@dataclass
class Operand:
    """Represents an instruction operand"""
    raw: str
    is_register: bool = False
    is_memory: bool = False
    is_immediate: bool = False
    is_label: bool = False
    register: Optional[str] = None
    base_reg: Optional[str] = None
    index_reg: Optional[str] = None
    scale: int = 1
    displacement: int = 0
    immediate_value: Optional[int] = None
    label_name: Optional[str] = None

@dataclass
class Instruction:
    """Represents a single instruction"""
    line_num: int
    raw_line: str
    label: Optional[str] = None
    mnemonic: Optional[str] = None
    operands: List[Operand] = field(default_factory=list)
    instr_type: InstrType = InstrType.NORMAL
    comment: Optional[str] = None
    jump_target: Optional[str] = None  # For jumps/calls

@dataclass
class BasicBlock:
    """A sequence of instructions with single entry, single exit"""
    label: str
    instructions: List[Instruction] = field(default_factory=list)
    successors: List[str] = field(default_factory=list)  # Labels of successor blocks
    predecessors: List[str] = field(default_factory=list)

@dataclass
class Function:
    """A function from global label to ret"""
    name: str
    start_line: int
    blocks: Dict[str, BasicBlock] = field(default_factory=dict)
    entry_block: Optional[str] = None
    exit_blocks: List[str] = field(default_factory=list)
    calls: Set[str] = field(default_factory=set)  # Functions this calls
    is_global: bool = True

@dataclass
class ParsedFile:
    """Complete parsed assembly file"""
    filename: str
    functions: Dict[str, Function] = field(default_factory=dict)
    globals: Set[str] = field(default_factory=set)
    externs: Set[str] = field(default_factory=set)
    all_labels: Set[str] = field(default_factory=set)

class NASMParser:
    """Parser for NASM assembly files"""

    # Conditional jump mnemonics
    COND_JUMPS = {
        'je', 'jne', 'jz', 'jnz', 'jg', 'jge', 'jl', 'jle',
        'ja', 'jae', 'jb', 'jbe', 'jo', 'jno', 'js', 'jns',
        'jp', 'jnp', 'jc', 'jnc', 'jecxz', 'jrcxz'
    }

    # All x86-64 registers
    REGISTERS = {
        # 64-bit
        'rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp', 'rsp',
        'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'r15',
        # 32-bit
        'eax', 'ebx', 'ecx', 'edx', 'esi', 'edi', 'ebp', 'esp',
        'r8d', 'r9d', 'r10d', 'r11d', 'r12d', 'r13d', 'r14d', 'r15d',
        # 16-bit
        'ax', 'bx', 'cx', 'dx', 'si', 'di', 'bp', 'sp',
        'r8w', 'r9w', 'r10w', 'r11w', 'r12w', 'r13w', 'r14w', 'r15w',
        # 8-bit
        'al', 'bl', 'cl', 'dl', 'ah', 'bh', 'ch', 'dh',
        'sil', 'dil', 'bpl', 'spl',
        'r8b', 'r9b', 'r10b', 'r11b', 'r12b', 'r13b', 'r14b', 'r15b',
        # SSE/AVX
        'xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5', 'xmm6', 'xmm7',
        'xmm8', 'xmm9', 'xmm10', 'xmm11', 'xmm12', 'xmm13', 'xmm14', 'xmm15',
    }

    # Callee-saved registers (must be preserved across calls)
    CALLEE_SAVED = {'rbx', 'rbp', 'r12', 'r13', 'r14', 'r15'}

    def __init__(self):
        self.current_file: Optional[ParsedFile] = None

    def parse_file(self, filename: str) -> ParsedFile:
        """Parse an assembly file"""
        with open(filename, 'r') as f:
            lines = f.readlines()

        self.current_file = ParsedFile(filename=filename)

        # First pass: collect globals, externs, labels
        self._collect_symbols(lines)

        # Second pass: parse instructions
        instructions = self._parse_instructions(lines)

        # Third pass: build functions and basic blocks
        self._build_functions(instructions)

        return self.current_file

    def _collect_symbols(self, lines: List[str]):
        """Collect global, extern declarations and all labels"""
        # First pass: collect all call targets (local function entry points)
        call_targets = set()
        for line in lines:
            stripped = line.strip()
            if stripped.startswith('call '):
                target = stripped[5:].split()[0].split(';')[0].strip()
                # Not an indirect call and not a local label
                if target and not target.startswith('[') and not target.startswith('.'):
                    call_targets.add(target)

        for line in lines:
            line = line.strip()

            # Skip empty lines and comments
            if not line or line.startswith(';'):
                continue

            # Global declaration
            if line.startswith('global '):
                name = line[7:].split()[0].strip()
                self.current_file.globals.add(name)

            # Extern declaration
            elif line.startswith('extern '):
                name = line[7:].split()[0].strip()
                self.current_file.externs.add(name)

            # Label definition
            elif ':' in line and not line.startswith('%'):
                # Extract label name (before the colon)
                label = line.split(':')[0].strip()
                if label and not label.startswith('.'):
                    self.current_file.all_labels.add(label)
                    # If this label is a call target, treat as function entry
                    if label in call_targets:
                        self.current_file.globals.add(label)

    def _parse_instructions(self, lines: List[str]) -> List[Instruction]:
        """Parse all instructions from lines"""
        instructions = []

        for i, line in enumerate(lines, 1):
            instr = self._parse_line(i, line)
            if instr:
                instructions.append(instr)

        return instructions

    def _parse_line(self, line_num: int, line: str) -> Optional[Instruction]:
        """Parse a single line into an Instruction"""
        raw_line = line
        line = line.strip()

        # Skip empty lines
        if not line:
            return None

        # Extract comment
        comment = None
        if ';' in line:
            # Handle semicolon in strings
            in_string = False
            for i, c in enumerate(line):
                if c in '"\'':
                    in_string = not in_string
                elif c == ';' and not in_string:
                    comment = line[i+1:].strip()
                    line = line[:i].strip()
                    break

        # Skip pure comments
        if not line:
            return None

        # Skip directives we don't care about
        if line.startswith('%') or line.startswith('section '):
            return Instruction(line_num=line_num, raw_line=raw_line,
                             instr_type=InstrType.DIRECTIVE, comment=comment)

        # Skip data definitions
        if any(line.startswith(d) for d in ['db ', 'dw ', 'dd ', 'dq ', 'resb ', 'resw ', 'resd ', 'resq ']):
            return Instruction(line_num=line_num, raw_line=raw_line,
                             instr_type=InstrType.DIRECTIVE, comment=comment)

        # Check for label
        label = None
        if ':' in line:
            parts = line.split(':', 1)
            label = parts[0].strip()
            line = parts[1].strip() if len(parts) > 1 else ''

            if not line:
                # Label-only line
                return Instruction(line_num=line_num, raw_line=raw_line,
                                 label=label, instr_type=InstrType.LABEL, comment=comment)

        # Skip if no instruction after label
        if not line:
            return Instruction(line_num=line_num, raw_line=raw_line,
                             label=label, instr_type=InstrType.LABEL, comment=comment)

        # Parse mnemonic and operands
        parts = line.split(None, 1)
        mnemonic = parts[0].lower()
        operands_str = parts[1] if len(parts) > 1 else ''

        # Parse operands
        operands = self._parse_operands(operands_str) if operands_str else []

        # Determine instruction type
        instr_type, jump_target = self._classify_instruction(mnemonic, operands)

        return Instruction(
            line_num=line_num,
            raw_line=raw_line,
            label=label,
            mnemonic=mnemonic,
            operands=operands,
            instr_type=instr_type,
            comment=comment,
            jump_target=jump_target
        )

    def _parse_operands(self, operands_str: str) -> List[Operand]:
        """Parse operand string into list of Operand objects"""
        operands = []

        # Split by comma, but respect brackets and quotes
        parts = self._split_operands(operands_str)

        for part in parts:
            part = part.strip()
            if not part:
                continue
            operands.append(self._parse_single_operand(part))

        return operands

    def _split_operands(self, s: str) -> List[str]:
        """Split operands by comma, respecting brackets"""
        parts = []
        current = []
        depth = 0
        in_string = False

        for c in s:
            if c in '"\'':
                in_string = not in_string
            elif not in_string:
                if c == '[':
                    depth += 1
                elif c == ']':
                    depth -= 1
                elif c == ',' and depth == 0:
                    parts.append(''.join(current))
                    current = []
                    continue
            current.append(c)

        if current:
            parts.append(''.join(current))

        return parts

    def _parse_single_operand(self, op: str) -> Operand:
        """Parse a single operand"""
        op = op.strip()
        original = op

        # Remove size specifiers
        for size in ['byte ', 'word ', 'dword ', 'qword ', 'xmmword ', 'ymmword ']:
            if op.lower().startswith(size):
                op = op[len(size):].strip()
                break

        operand = Operand(raw=original)

        # Memory operand [...]
        if op.startswith('[') and op.endswith(']'):
            operand.is_memory = True
            inner = op[1:-1].strip()
            self._parse_memory_operand(inner, operand)

        # Register
        elif op.lower() in self.REGISTERS:
            operand.is_register = True
            operand.register = op.lower()

        # Immediate (number or constant)
        elif op.startswith('0x') or op.startswith('-') or op[0].isdigit():
            operand.is_immediate = True
            try:
                operand.immediate_value = int(op, 0)
            except ValueError:
                pass

        # Label/symbol reference
        elif op.startswith('rel '):
            operand.is_label = True
            operand.label_name = op[4:].strip()
        else:
            # Could be a label or constant
            operand.is_label = True
            operand.label_name = op

        return operand

    def _parse_memory_operand(self, inner: str, operand: Operand):
        """Parse the inside of a memory operand [...]"""
        # Handle rel prefix
        if inner.startswith('rel '):
            inner = inner[4:].strip()
            operand.label_name = inner
            operand.is_label = True
            return

        # Try to parse base + index*scale + displacement
        # This is simplified - full parsing would be more complex
        parts = re.split(r'([+\-])', inner)

        for i, part in enumerate(parts):
            part = part.strip()
            if not part or part in ['+', '-']:
                continue

            sign = 1
            if i > 0 and parts[i-1] == '-':
                sign = -1

            # Check for register
            if part.lower() in self.REGISTERS:
                if operand.base_reg is None:
                    operand.base_reg = part.lower()
                else:
                    operand.index_reg = part.lower()

            # Check for scaled index (reg*scale)
            elif '*' in part:
                reg, scale = part.split('*')
                operand.index_reg = reg.strip().lower()
                try:
                    operand.scale = int(scale.strip())
                except ValueError:
                    pass

            # Displacement or constant
            else:
                try:
                    operand.displacement += sign * int(part, 0)
                except ValueError:
                    # Could be a symbol
                    operand.label_name = part
                    operand.is_label = True

    def _classify_instruction(self, mnemonic: str, operands: List[Operand]) -> Tuple[InstrType, Optional[str]]:
        """Classify instruction type and extract jump target if applicable"""
        jump_target = None

        if mnemonic == 'ret':
            return InstrType.RET, None

        if mnemonic == 'jmp':
            if operands and operands[0].is_label:
                jump_target = operands[0].label_name
            return InstrType.JUMP, jump_target

        if mnemonic in self.COND_JUMPS:
            if operands and operands[0].is_label:
                jump_target = operands[0].label_name
            return InstrType.COND_JUMP, jump_target

        if mnemonic == 'call':
            if operands and operands[0].is_label:
                jump_target = operands[0].label_name
            elif operands and operands[0].is_register:
                jump_target = f"indirect:{operands[0].register}"
            return InstrType.CALL, jump_target

        if mnemonic == 'push':
            return InstrType.PUSH, None

        if mnemonic == 'pop':
            return InstrType.POP, None

        if mnemonic == 'mov':
            return InstrType.MOV, None

        if mnemonic == 'nop':
            return InstrType.NOP, None

        return InstrType.NORMAL, None

    def _build_functions(self, instructions: List[Instruction]):
        """Build functions and basic blocks from instructions"""
        # Find function boundaries (global label to next global label or EOF)
        current_func: Optional[Function] = None
        current_block: Optional[BasicBlock] = None
        block_counter = 0
        # Track block that needs fall-through successor added
        prev_block_needs_fallthrough: Optional[BasicBlock] = None

        for instr in instructions:
            # Skip directives
            if instr.instr_type == InstrType.DIRECTIVE:
                continue

            # New function starts at global label
            if instr.label and instr.label in self.current_file.globals:
                if current_func:
                    # Save any pending block before switching functions
                    if current_block:
                        current_func.blocks[current_block.label] = current_block
                    self.current_file.functions[current_func.name] = current_func

                current_func = Function(name=instr.label, start_line=instr.line_num)
                current_block = BasicBlock(label=instr.label)
                current_func.entry_block = instr.label
                block_counter = 0
                prev_block_needs_fallthrough = None

            if not current_func:
                continue

            # New local label starts a new block
            if instr.label and instr.label != current_func.name:
                if current_block:
                    # Save previous block (even if empty - it's a fall-through label)
                    current_func.blocks[current_block.label] = current_block
                    # Previous block falls through to this one (unless it ends in jump/ret)
                    last_instr = current_block.instructions[-1] if current_block.instructions else None
                    if not last_instr or last_instr.instr_type not in [InstrType.JUMP, InstrType.RET]:
                        current_block.successors.append(instr.label)

                # If previous block ended with COND_JUMP, add fall-through
                if prev_block_needs_fallthrough:
                    prev_block_needs_fallthrough.successors.append(instr.label)
                    prev_block_needs_fallthrough = None

                current_block = BasicBlock(label=instr.label)

            if not current_block:
                # If previous block ended with COND_JUMP, add fall-through
                if prev_block_needs_fallthrough:
                    block_counter += 1
                    current_block = BasicBlock(label=f".block_{block_counter}")
                    prev_block_needs_fallthrough.successors.append(current_block.label)
                    prev_block_needs_fallthrough = None
                else:
                    block_counter += 1
                    current_block = BasicBlock(label=f".block_{block_counter}")

            # Add instruction to current block
            if instr.mnemonic:  # Skip label-only lines
                current_block.instructions.append(instr)

                # Track calls
                if instr.instr_type == InstrType.CALL and instr.jump_target:
                    if not instr.jump_target.startswith('indirect:'):
                        current_func.calls.add(instr.jump_target)

                # Block ends at control flow instruction
                if instr.instr_type in [InstrType.JUMP, InstrType.COND_JUMP, InstrType.RET]:
                    # Record successors
                    if instr.instr_type == InstrType.RET:
                        current_func.exit_blocks.append(current_block.label)
                    elif instr.jump_target:
                        current_block.successors.append(instr.jump_target)

                    # Conditional jumps also fall through - mark for next block
                    if instr.instr_type == InstrType.COND_JUMP:
                        prev_block_needs_fallthrough = current_block
                    else:
                        prev_block_needs_fallthrough = None

                    current_func.blocks[current_block.label] = current_block
                    current_block = None

        # Save last function and block
        if current_block:
            current_func.blocks[current_block.label] = current_block
        if current_func:
            self.current_file.functions[current_func.name] = current_func

        # Build predecessor lists
        for func in self.current_file.functions.values():
            for block in func.blocks.values():
                for succ in block.successors:
                    if succ in func.blocks:
                        func.blocks[succ].predecessors.append(block.label)


def parse_all_files(directory: str) -> Dict[str, ParsedFile]:
    """Parse all .asm files in a directory"""
    import os

    parser = NASMParser()
    results = {}

    for filename in os.listdir(directory):
        if filename.endswith('.asm'):
            filepath = os.path.join(directory, filename)
            try:
                results[filename] = parser.parse_file(filepath)
            except Exception as e:
                print(f"Error parsing {filename}: {e}")

    return results


if __name__ == '__main__':
    import sys

    if len(sys.argv) < 2:
        print("Usage: python parser.py <file.asm or directory>")
        sys.exit(1)

    import os
    target = sys.argv[1]

    if os.path.isdir(target):
        results = parse_all_files(target)
        for filename, parsed in results.items():
            print(f"\n=== {filename} ===")
            print(f"  Functions: {list(parsed.functions.keys())}")
            for func_name, func in parsed.functions.items():
                print(f"    {func_name}: {len(func.blocks)} blocks, calls {func.calls}")
    else:
        parser = NASMParser()
        result = parser.parse_file(target)
        print(f"Globals: {result.globals}")
        print(f"Externs: {result.externs}")
        for func_name, func in result.functions.items():
            print(f"\nFunction: {func_name}")
            print(f"  Entry: {func.entry_block}")
            print(f"  Exits: {func.exit_blocks}")
            print(f"  Calls: {func.calls}")
            for block_name, block in func.blocks.items():
                print(f"  Block {block_name}:")
                print(f"    {len(block.instructions)} instructions")
                print(f"    Successors: {block.successors}")
