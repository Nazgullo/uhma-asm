#!/usr/bin/env python3
"""
UHMA Assembly to Symbolic Logic Converter

Converts x86_64 assembly to a formal logic representation for verification.
Tracks: register state, stack balance, memory access bounds, control flow.
"""

import re
import os
from dataclasses import dataclass, field
from typing import Dict, List, Set, Optional, Tuple
from enum import Enum
from pathlib import Path

class RegSize(Enum):
    R64 = 64
    R32 = 32
    R16 = 16
    R8 = 8

@dataclass
class Register:
    name: str
    size: RegSize
    base: str  # e.g., 'rax' for 'eax', 'ax', 'al'

# Register mappings
REG_64 = {'rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp', 'rsp',
          'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'r15'}
REG_32 = {'eax', 'ebx', 'ecx', 'edx', 'esi', 'edi', 'ebp', 'esp',
          'r8d', 'r9d', 'r10d', 'r11d', 'r12d', 'r13d', 'r14d', 'r15d'}
REG_16 = {'ax', 'bx', 'cx', 'dx', 'si', 'di', 'bp', 'sp'}
REG_8 = {'al', 'bl', 'cl', 'dl', 'sil', 'dil', 'bpl', 'spl',
         'r8b', 'r9b', 'r10b', 'r11b', 'r12b', 'r13b', 'r14b', 'r15b'}

CALLEE_SAVED = {'rbx', 'rbp', 'r12', 'r13', 'r14', 'r15'}
CALLER_SAVED = {'rax', 'rcx', 'rdx', 'rsi', 'rdi', 'r8', 'r9', 'r10', 'r11'}

@dataclass
class StackOp:
    """Represents a stack operation"""
    op: str  # 'push', 'pop', 'sub', 'add'
    operand: str
    size: int  # bytes
    line: int

@dataclass
class MemAccess:
    """Represents a memory access"""
    op: str  # 'read', 'write'
    base: str  # base register or symbol
    offset: str  # offset expression
    size: int
    line: int

@dataclass
class FunctionModel:
    """Symbolic model of a function"""
    name: str
    file: str
    start_line: int
    end_line: int

    # Stack analysis
    stack_ops: List[StackOp] = field(default_factory=list)
    max_stack_depth: int = 0
    stack_balanced: Optional[bool] = None

    # Register analysis
    pushed_regs: List[str] = field(default_factory=list)
    popped_regs: List[str] = field(default_factory=list)
    modified_regs: Set[str] = field(default_factory=set)
    callee_saved_violated: List[str] = field(default_factory=list)

    # Memory analysis
    mem_accesses: List[MemAccess] = field(default_factory=list)
    bounds_violations: List[str] = field(default_factory=list)

    # Control flow
    calls: List[str] = field(default_factory=list)
    indirect_calls: List[Tuple[int, str]] = field(default_factory=list)
    indirect_jumps: List[Tuple[int, str]] = field(default_factory=list)

    # Verification results
    invariants: List[str] = field(default_factory=list)
    violations: List[str] = field(default_factory=list)

@dataclass
class Proposition:
    """A logical proposition about program state"""
    name: str
    formula: str
    holds: Optional[bool] = None
    counterexample: Optional[str] = None

class SymbolicState:
    """Tracks symbolic state during analysis"""
    def __init__(self):
        self.stack_depth = 0  # relative to function entry
        self.push_seq: List[str] = []  # sequence of pushed values
        self.regs: Dict[str, str] = {}  # reg -> symbolic value
        self.mem: Dict[str, str] = {}  # addr expr -> symbolic value

class AsmParser:
    """Parse assembly files into structured representation"""

    def __init__(self):
        self.functions: Dict[str, FunctionModel] = {}
        self.constants: Dict[str, int] = {}
        self.current_file = ""

    def parse_file(self, filepath: str) -> List[FunctionModel]:
        """Parse an assembly file and extract functions"""
        self.current_file = os.path.basename(filepath)
        functions = []

        with open(filepath, 'r') as f:
            lines = f.readlines()

        # First pass: find all labels and their types
        labels = {}
        current_func = None
        func_start = 0

        for i, line in enumerate(lines):
            line = line.strip()

            # Skip empty lines and comments
            if not line or line.startswith(';'):
                continue

            # Global declaration marks function
            if line.startswith('global '):
                func_name = line.split()[1]
                labels[func_name] = ('function', i)

            # Label definition
            elif ':' in line and not line.startswith('%'):
                label = line.split(':')[0].strip()
                if label in labels and labels[label][0] == 'function':
                    # Start of a new function
                    if current_func:
                        current_func.end_line = i - 1
                        functions.append(current_func)
                    current_func = FunctionModel(
                        name=label,
                        file=self.current_file,
                        start_line=i,
                        end_line=len(lines)
                    )
                    func_start = i

        if current_func:
            functions.append(current_func)

        # Second pass: analyze each function
        for func in functions:
            self._analyze_function(func, lines)

        return functions

    def _analyze_function(self, func: FunctionModel, lines: List[str]):
        """Analyze a single function for stack balance, register usage, etc."""
        state = SymbolicState()
        in_function = False

        for i in range(func.start_line, min(func.end_line + 1, len(lines))):
            line = lines[i].strip()

            # Skip empty, comments, directives
            if not line or line.startswith(';') or line.startswith('%'):
                continue

            # Remove inline comments
            if ';' in line:
                line = line.split(';')[0].strip()

            # Check for function start
            if line == f"{func.name}:":
                in_function = True
                continue

            if not in_function:
                continue

            # Check for next label (end of function)
            if ':' in line and not line.startswith('.'):
                break

            # Parse instruction
            self._analyze_instruction(func, state, line, i + 1)

    def _analyze_instruction(self, func: FunctionModel, state: SymbolicState,
                            line: str, line_num: int):
        """Analyze a single instruction"""
        parts = line.replace(',', ' ').split()
        if not parts:
            return

        op = parts[0].lower()
        args = parts[1:] if len(parts) > 1 else []

        # Stack operations
        if op == 'push':
            reg = args[0] if args else '?'
            size = 8  # 64-bit push
            func.stack_ops.append(StackOp('push', reg, size, line_num))
            state.stack_depth += size
            state.push_seq.append(reg)
            func.pushed_regs.append(reg)
            func.max_stack_depth = max(func.max_stack_depth, state.stack_depth)

        elif op == 'pop':
            reg = args[0] if args else '?'
            size = 8
            func.stack_ops.append(StackOp('pop', reg, size, line_num))
            state.stack_depth -= size
            func.popped_regs.append(reg)

            # Check LIFO order
            if state.push_seq:
                expected = state.push_seq.pop()
                if expected != reg:
                    func.violations.append(
                        f"Line {line_num}: pop {reg} but last push was {expected} (LIFO violation)")
            else:
                func.violations.append(
                    f"Line {line_num}: pop {reg} with empty push stack")

        elif op == 'sub' and len(args) >= 2 and args[0] in ('rsp', 'esp'):
            # sub rsp, N
            try:
                size = self._parse_imm(args[1])
                func.stack_ops.append(StackOp('sub', args[1], size, line_num))
                state.stack_depth += size
                func.max_stack_depth = max(func.max_stack_depth, state.stack_depth)
            except:
                func.stack_ops.append(StackOp('sub', args[1], 0, line_num))

        elif op == 'add' and len(args) >= 2 and args[0] in ('rsp', 'esp'):
            # add rsp, N
            try:
                size = self._parse_imm(args[1])
                func.stack_ops.append(StackOp('add', args[1], size, line_num))
                state.stack_depth -= size
            except:
                func.stack_ops.append(StackOp('add', args[1], 0, line_num))

        # Calls
        elif op == 'call':
            if args:
                target = args[0]
                if target.startswith('r') or target.startswith('['):
                    # Indirect call
                    func.indirect_calls.append((line_num, target))
                else:
                    func.calls.append(target)

        # Indirect jumps
        elif op in ('jmp', 'jne', 'je', 'jg', 'jl', 'jge', 'jle', 'jz', 'jnz',
                    'ja', 'jb', 'jae', 'jbe'):
            if args and (args[0].startswith('r') or args[0].startswith('[')):
                func.indirect_jumps.append((line_num, args[0]))

        # Return - check stack balance
        elif op == 'ret':
            if state.stack_depth != 0:
                func.violations.append(
                    f"Line {line_num}: ret with stack depth {state.stack_depth} (should be 0)")
                func.stack_balanced = False
            else:
                if func.stack_balanced is None:
                    func.stack_balanced = True

            # Check callee-saved registers
            pushed_callee = [r for r in func.pushed_regs if r in CALLEE_SAVED]
            popped_callee = [r for r in func.popped_regs if r in CALLEE_SAVED]
            if pushed_callee != list(reversed(popped_callee)):
                func.violations.append(
                    f"Line {line_num}: callee-saved mismatch: pushed {pushed_callee}, "
                    f"popped {popped_callee}")

        # Memory accesses with brackets
        if '[' in line:
            self._analyze_mem_access(func, line, line_num)

        # Track register modifications
        if args and op not in ('push', 'call', 'jmp', 'je', 'jne', 'ret', 'cmp', 'test'):
            dest = args[0].lower()
            if dest in REG_64 or dest in REG_32 or dest in REG_16 or dest in REG_8:
                base = self._reg_base(dest)
                func.modified_regs.add(base)

    def _analyze_mem_access(self, func: FunctionModel, line: str, line_num: int):
        """Analyze memory access patterns"""
        # Extract [base + offset] patterns
        match = re.search(r'\[([^\]]+)\]', line)
        if match:
            addr_expr = match.group(1)

            # Determine if read or write
            parts = line.split()
            op = parts[0].lower()

            if op in ('mov', 'movsd', 'movss', 'movzx', 'movsx'):
                args = ' '.join(parts[1:]).split(',')
                if len(args) >= 2:
                    if '[' in args[0]:
                        access_type = 'write'
                    else:
                        access_type = 'read'
                else:
                    access_type = 'read'
            else:
                access_type = 'read'  # default assumption

            func.mem_accesses.append(MemAccess(
                op=access_type,
                base=addr_expr,
                offset='0',
                size=8,
                line=line_num
            ))

    def _parse_imm(self, s: str) -> int:
        """Parse an immediate value"""
        s = s.strip()
        if s.startswith('0x'):
            return int(s, 16)
        elif s.isdigit():
            return int(s)
        elif '*' in s:
            # Handle expressions like "16 * 4"
            parts = s.split('*')
            return self._parse_imm(parts[0]) * self._parse_imm(parts[1])
        else:
            # Symbol - lookup or return 0
            return self.constants.get(s, 0)

    def _reg_base(self, reg: str) -> str:
        """Get base 64-bit register name"""
        reg = reg.lower()
        if reg in REG_64:
            return reg
        mapping = {
            'eax': 'rax', 'ax': 'rax', 'al': 'rax', 'ah': 'rax',
            'ebx': 'rbx', 'bx': 'rbx', 'bl': 'rbx', 'bh': 'rbx',
            'ecx': 'rcx', 'cx': 'rcx', 'cl': 'rcx', 'ch': 'rcx',
            'edx': 'rdx', 'dx': 'rdx', 'dl': 'rdx', 'dh': 'rdx',
            'esi': 'rsi', 'si': 'rsi', 'sil': 'rsi',
            'edi': 'rdi', 'di': 'rdi', 'dil': 'rdi',
            'ebp': 'rbp', 'bp': 'rbp', 'bpl': 'rbp',
            'esp': 'rsp', 'sp': 'rsp', 'spl': 'rsp',
        }
        if reg in mapping:
            return mapping[reg]
        # r8-r15 variants
        for i in range(8, 16):
            if reg.startswith(f'r{i}'):
                return f'r{i}'
        return reg


class LogicGenerator:
    """Generate formal logic propositions from function models"""

    def __init__(self):
        self.propositions: List[Proposition] = []

    def generate_propositions(self, func: FunctionModel) -> List[Proposition]:
        """Generate verification propositions for a function"""
        props = []

        # P1: Stack Balance
        push_total = sum(op.size for op in func.stack_ops if op.op == 'push')
        pop_total = sum(op.size for op in func.stack_ops if op.op == 'pop')
        sub_total = sum(op.size for op in func.stack_ops if op.op == 'sub')
        add_total = sum(op.size for op in func.stack_ops if op.op == 'add')

        net = push_total - pop_total + sub_total - add_total
        props.append(Proposition(
            name=f"{func.name}.stack_balanced",
            formula=f"push({push_total}) + sub({sub_total}) = pop({pop_total}) + add({add_total})",
            holds=(net == 0),
            counterexample=f"net stack delta = {net}" if net != 0 else None
        ))

        # P2: Callee-saved register preservation
        pushed_callee = set(r for r in func.pushed_regs if r in CALLEE_SAVED)
        popped_callee = set(r for r in func.popped_regs if r in CALLEE_SAVED)
        modified_callee = func.modified_regs & CALLEE_SAVED
        unrestored = modified_callee - popped_callee

        props.append(Proposition(
            name=f"{func.name}.callee_saved",
            formula=f"forall r in {modified_callee}: pushed(r) and popped(r)",
            holds=(len(unrestored) == 0),
            counterexample=f"modified but not restored: {unrestored}" if unrestored else None
        ))

        # P3: LIFO stack discipline
        lifo_ok = len([v for v in func.violations if 'LIFO' in v]) == 0
        props.append(Proposition(
            name=f"{func.name}.lifo_discipline",
            formula="forall push(x), pop(y): pop order = reverse(push order)",
            holds=lifo_ok,
            counterexample=next((v for v in func.violations if 'LIFO' in v), None)
        ))

        # P4: No indirect calls to data sections
        safe_indirect = all(
            not any(danger in target for danger in ['input_buf', 'bss', 'data'])
            for _, target in func.indirect_calls
        )
        props.append(Proposition(
            name=f"{func.name}.safe_indirect_calls",
            formula="forall call(target): target in .text",
            holds=safe_indirect,
            counterexample=str(func.indirect_calls) if not safe_indirect else None
        ))

        # P5: Stack depth never negative at ret
        ret_violations = [v for v in func.violations if 'ret with stack depth' in v]
        props.append(Proposition(
            name=f"{func.name}.valid_ret",
            formula="at ret: stack_depth = 0",
            holds=(len(ret_violations) == 0),
            counterexample=ret_violations[0] if ret_violations else None
        ))

        return props


def analyze_codebase(asm_dir: str) -> Dict[str, List[Proposition]]:
    """Analyze all assembly files in directory"""
    parser = AsmParser()
    logic_gen = LogicGenerator()
    results = {}

    # First, parse constants.inc if it exists
    const_file = os.path.join(asm_dir, 'include', 'constants.inc')
    if os.path.exists(const_file):
        with open(const_file, 'r') as f:
            for line in f:
                if line.strip().startswith('%define'):
                    parts = line.strip().split()
                    if len(parts) >= 3:
                        name = parts[1]
                        try:
                            val = int(parts[2], 0)
                            parser.constants[name] = val
                        except:
                            pass

    # Analyze each .asm file
    for fname in sorted(os.listdir(asm_dir)):
        if fname.endswith('.asm'):
            filepath = os.path.join(asm_dir, fname)
            try:
                functions = parser.parse_file(filepath)
                for func in functions:
                    props = logic_gen.generate_propositions(func)
                    results[f"{fname}:{func.name}"] = props
            except Exception as e:
                print(f"Error parsing {fname}: {e}")

    return results


def print_verification_report(results: Dict[str, List[Proposition]]):
    """Print verification results"""
    total_props = 0
    passed = 0
    failed = 0

    print("=" * 70)
    print("UHMA FORMAL VERIFICATION REPORT")
    print("=" * 70)
    print()

    # Group by file
    by_file = {}
    for key, props in results.items():
        fname = key.split(':')[0]
        if fname not in by_file:
            by_file[fname] = []
        by_file[fname].append((key, props))

    for fname in sorted(by_file.keys()):
        print(f"\n{'='*70}")
        print(f"FILE: {fname}")
        print('='*70)

        for key, props in by_file[fname]:
            func_name = key.split(':')[1]
            print(f"\n  FUNCTION: {func_name}")
            print(f"  {'-'*60}")

            for prop in props:
                total_props += 1
                status = "PASS" if prop.holds else "FAIL"
                if prop.holds:
                    passed += 1
                else:
                    failed += 1

                symbol = "[+]" if prop.holds else "[!]"
                print(f"    {symbol} {prop.name}")
                print(f"        Formula: {prop.formula}")
                if not prop.holds and prop.counterexample:
                    print(f"        COUNTEREXAMPLE: {prop.counterexample}")

    # Summary
    print("\n" + "=" * 70)
    print("SUMMARY")
    print("=" * 70)
    print(f"  Total propositions: {total_props}")
    print(f"  Passed: {passed}")
    print(f"  Failed: {failed}")
    print(f"  Pass rate: {100*passed/total_props:.1f}%" if total_props > 0 else "N/A")
    print()

    return failed


if __name__ == "__main__":
    import sys
    asm_dir = sys.argv[1] if len(sys.argv) > 1 else "."
    results = analyze_codebase(asm_dir)
    failures = print_verification_report(results)
    sys.exit(1 if failures > 0 else 0)
