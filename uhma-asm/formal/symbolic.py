"""
symbolic.py — Symbolic Execution Engine for x86-64 assembly

@entry SymbolicExecutor.execute(func) -> FunctionAnalysis
@entry analyze_file(path) -> list[FunctionAnalysis]

@calls parser.py:ParsedFile, Function, BasicBlock, Instruction
@calledby analyze.py

TRACKS:
  - Stack depth (abstract integer, detects imbalance)
  - Register state: SAVED/RESTORED/MODIFIED/ARGUMENT
  - Callee-saved violations (rbx, rbp, r12-r15)

GOTCHAS:
  - Explores ALL paths (exponential in conditional branches)
  - CALLEE_SAVED = {rbx, rbp, r12, r13, r14, r15}
  - Reports path to first violation for debugging
"""

from dataclasses import dataclass, field
from typing import Dict, List, Set, Optional, Tuple, Any
from enum import Enum, auto
from copy import deepcopy
from parser import (
    ParsedFile, Function, BasicBlock, Instruction,
    InstrType, Operand, NASMParser
)


class RegState(Enum):
    """State of a register"""
    UNKNOWN = auto()      # Unknown value
    SAVED = auto()        # Saved to stack, not yet restored
    RESTORED = auto()     # Restored from stack
    MODIFIED = auto()     # Modified without save
    ARGUMENT = auto()     # Contains function argument
    RETURN_VAL = auto()   # Contains return value


@dataclass
class StackEntry:
    """An entry on the symbolic stack"""
    source: str           # What was pushed (register name or 'unknown')
    line_num: int         # Where it was pushed
    block: str            # Which block


@dataclass
class SymbolicState:
    """Complete symbolic state at a program point"""
    stack_depth: int = 0
    stack: List[StackEntry] = field(default_factory=list)
    registers: Dict[str, RegState] = field(default_factory=dict)
    saved_registers: Dict[str, int] = field(default_factory=dict)  # reg -> stack position when saved
    path: List[str] = field(default_factory=list)  # Sequence of blocks visited
    issues: List[str] = field(default_factory=list)  # Issues found on this path

    def clone(self) -> 'SymbolicState':
        """Deep copy the state"""
        return SymbolicState(
            stack_depth=self.stack_depth,
            stack=list(self.stack),
            registers=dict(self.registers),
            saved_registers=dict(self.saved_registers),
            path=list(self.path),
            issues=list(self.issues)
        )


@dataclass
class PathResult:
    """Result of analyzing one path through a function"""
    path: List[str]
    final_state: SymbolicState
    issues: List[str]
    is_valid: bool


@dataclass
class FunctionAnalysis:
    """Complete analysis of a function"""
    name: str
    paths: List[PathResult]
    all_issues: List[str]
    stack_balanced: bool
    registers_preserved: bool
    indirect_calls: List[Tuple[int, str]]  # (line, target register)


# Callee-saved registers that must be preserved
CALLEE_SAVED = {'rbx', 'rbp', 'r12', 'r13', 'r14', 'r15'}

# Register mappings (sub-registers to full register)
REG_TO_FULL = {
    'eax': 'rax', 'ax': 'rax', 'al': 'rax', 'ah': 'rax',
    'ebx': 'rax', 'bx': 'rbx', 'bl': 'rbx', 'bh': 'rbx',
    'ecx': 'rcx', 'cx': 'rcx', 'cl': 'rcx', 'ch': 'rcx',
    'edx': 'rdx', 'dx': 'rdx', 'dl': 'rdx', 'dh': 'rdx',
    'esi': 'rsi', 'si': 'rsi', 'sil': 'rsi',
    'edi': 'rdi', 'di': 'rdi', 'dil': 'rdi',
    'ebp': 'rbp', 'bp': 'rbp', 'bpl': 'rbp',
    'esp': 'rsp', 'sp': 'rsp', 'spl': 'rsp',
    'r8d': 'r8', 'r8w': 'r8', 'r8b': 'r8',
    'r9d': 'r9', 'r9w': 'r9', 'r9b': 'r9',
    'r10d': 'r10', 'r10w': 'r10', 'r10b': 'r10',
    'r11d': 'r11', 'r11w': 'r11', 'r11b': 'r11',
    'r12d': 'r12', 'r12w': 'r12', 'r12b': 'r12',
    'r13d': 'r13', 'r13w': 'r13', 'r13b': 'r13',
    'r14d': 'r14', 'r14w': 'r14', 'r14b': 'r14',
    'r15d': 'r15', 'r15w': 'r15', 'r15b': 'r15',
}


def get_full_reg(reg: str) -> str:
    """Get the full 64-bit register name"""
    reg = reg.lower()
    return REG_TO_FULL.get(reg, reg)


class SymbolicExecutor:
    """Symbolically execute functions to check invariants"""

    def __init__(self, max_path_depth: int = 50, max_loop_unroll: int = 2,
                 max_paths_per_function: int = 500):
        self.max_path_depth = max_path_depth
        self.max_loop_unroll = max_loop_unroll
        self.max_paths_per_function = max_paths_per_function
        self.paths_explored = 0  # Track during analysis

    def analyze_function(self, func: Function) -> FunctionAnalysis:
        """Analyze all paths through a function"""
        paths = []
        all_issues = []
        indirect_calls = []
        self.paths_explored = 0  # Reset for this function
        self.path_limit_hit = False

        if not func.entry_block or func.entry_block not in func.blocks:
            return FunctionAnalysis(
                name=func.name,
                paths=[],
                all_issues=[f"No entry block found"],
                stack_balanced=False,
                registers_preserved=False,
                indirect_calls=[]
            )

        # Start symbolic execution from entry
        initial_state = SymbolicState()
        initial_state.path = [func.entry_block]

        # Mark callee-saved registers as needing preservation
        for reg in CALLEE_SAVED:
            initial_state.registers[reg] = RegState.UNKNOWN

        # Explore all paths
        self._explore_paths(func, func.entry_block, initial_state, paths, set())

        if self.path_limit_hit:
            all_issues.append(f"Path limit ({self.max_paths_per_function}) reached - analysis may be incomplete")

        # Collect results
        stack_balanced = True
        registers_preserved = True

        for path_result in paths:
            all_issues.extend(path_result.issues)
            if not path_result.is_valid:
                if 'stack' in ' '.join(path_result.issues).lower():
                    stack_balanced = False
                if 'register' in ' '.join(path_result.issues).lower():
                    registers_preserved = False

        # Find indirect calls
        for block in func.blocks.values():
            for instr in block.instructions:
                if instr.instr_type == InstrType.CALL:
                    if instr.jump_target and instr.jump_target.startswith('indirect:'):
                        reg = instr.jump_target.split(':')[1]
                        indirect_calls.append((instr.line_num, reg))

        return FunctionAnalysis(
            name=func.name,
            paths=paths,
            all_issues=list(set(all_issues)),  # Dedupe
            stack_balanced=stack_balanced,
            registers_preserved=registers_preserved,
            indirect_calls=indirect_calls
        )

    def _explore_paths(self, func: Function, block_label: str,
                       state: SymbolicState, results: List[PathResult],
                       visited_in_path: Set[str]):
        """Recursively explore all paths through the function"""

        # Check path limit
        if self.paths_explored >= self.max_paths_per_function:
            self.path_limit_hit = True
            return

        # Check for infinite loops
        if len(state.path) > self.max_path_depth:
            state.issues.append(f"Path too deep (>{self.max_path_depth}), possible infinite loop")
            results.append(PathResult(
                path=list(state.path),
                final_state=state,
                issues=list(state.issues),
                is_valid=False
            ))
            self.paths_explored += 1
            return

        # Check for revisiting block (loop detection)
        visit_count = state.path.count(block_label)
        if visit_count > self.max_loop_unroll:
            # Assume loop is fine, don't explore further
            return

        # Get the block
        if block_label not in func.blocks:
            # Jumping to external label or unknown block
            if block_label.startswith('.'):
                state.issues.append(f"Jump to unknown local label: {block_label}")
            return

        block = func.blocks[block_label]

        # Execute each instruction in the block
        for instr in block.instructions:
            self._execute_instruction(instr, state, block_label)

        # Handle block termination
        last_instr = block.instructions[-1] if block.instructions else None

        if last_instr and last_instr.instr_type == InstrType.RET:
            # Function exit - check invariants
            self._check_exit_invariants(state, func.name)
            results.append(PathResult(
                path=list(state.path),
                final_state=state.clone(),
                issues=list(state.issues),
                is_valid=len(state.issues) == 0
            ))
            self.paths_explored += 1
            return

        if last_instr and last_instr.instr_type == InstrType.JUMP:
            # Unconditional jump
            if last_instr.jump_target:
                new_state = state.clone()
                new_state.path.append(last_instr.jump_target)
                self._explore_paths(func, last_instr.jump_target, new_state,
                                  results, visited_in_path | {block_label})
            return

        if last_instr and last_instr.instr_type == InstrType.COND_JUMP:
            # Conditional jump - explore both paths
            if last_instr.jump_target:
                # Taken branch
                taken_state = state.clone()
                taken_state.path.append(last_instr.jump_target)
                self._explore_paths(func, last_instr.jump_target, taken_state,
                                  results, visited_in_path | {block_label})

            # Fall-through branch - find next block
            for succ in block.successors:
                if succ != last_instr.jump_target:
                    fall_state = state.clone()
                    fall_state.path.append(succ)
                    self._explore_paths(func, succ, fall_state,
                                      results, visited_in_path | {block_label})
            return

        # Fall through to successors
        for succ in block.successors:
            succ_state = state.clone()
            succ_state.path.append(succ)
            self._explore_paths(func, succ, succ_state,
                              results, visited_in_path | {block_label})

    def _execute_instruction(self, instr: Instruction, state: SymbolicState, block: str):
        """Symbolically execute one instruction"""

        if instr.instr_type == InstrType.PUSH:
            # Push increases stack depth
            source = 'unknown'
            if instr.operands and instr.operands[0].is_register:
                source = get_full_reg(instr.operands[0].register)
                # Track that this register is saved
                if source in CALLEE_SAVED:
                    state.saved_registers[source] = state.stack_depth
                    state.registers[source] = RegState.SAVED

            state.stack.append(StackEntry(
                source=source,
                line_num=instr.line_num,
                block=block
            ))
            state.stack_depth += 1

        elif instr.instr_type == InstrType.POP:
            # Pop decreases stack depth
            if state.stack_depth <= 0:
                state.issues.append(
                    f"Line {instr.line_num}: Stack underflow (pop with empty stack)"
                )
            else:
                state.stack_depth -= 1
                if state.stack:
                    popped = state.stack.pop()
                    # Check if we're restoring to the right register
                    if instr.operands and instr.operands[0].is_register:
                        dest_reg = get_full_reg(instr.operands[0].register)
                        if popped.source in CALLEE_SAVED:
                            if dest_reg == popped.source:
                                state.registers[dest_reg] = RegState.RESTORED
                                if dest_reg in state.saved_registers:
                                    del state.saved_registers[dest_reg]
                            else:
                                # Popping saved register into different register
                                # This is the bug we found in dreams.asm!
                                state.issues.append(
                                    f"Line {instr.line_num}: LIFO violation - "
                                    f"pushed {popped.source} at line {popped.line_num}, "
                                    f"popping into {dest_reg}"
                                )

        elif instr.instr_type == InstrType.MOV:
            # Track register modifications
            if instr.operands and len(instr.operands) >= 1:
                if instr.operands[0].is_register:
                    dest_reg = get_full_reg(instr.operands[0].register)
                    if dest_reg in CALLEE_SAVED:
                        if state.registers.get(dest_reg) != RegState.SAVED:
                            state.registers[dest_reg] = RegState.MODIFIED

        elif instr.instr_type == InstrType.CALL:
            # Calls may clobber caller-saved registers
            # For now, we don't track this deeply
            pass

        elif instr.mnemonic and instr.mnemonic.startswith('sub') and instr.operands:
            # sub rsp, N - allocates stack space
            if (instr.operands[0].is_register and
                get_full_reg(instr.operands[0].register) == 'rsp'):
                if instr.operands[1].is_immediate:
                    # Track stack frame allocation
                    alloc = instr.operands[1].immediate_value
                    if alloc:
                        state.stack_depth += alloc // 8  # Approximate

        elif instr.mnemonic and instr.mnemonic.startswith('add') and instr.operands:
            # add rsp, N - deallocates stack space
            if (instr.operands[0].is_register and
                get_full_reg(instr.operands[0].register) == 'rsp'):
                if instr.operands[1].is_immediate:
                    dealloc = instr.operands[1].immediate_value
                    if dealloc:
                        state.stack_depth -= dealloc // 8  # Approximate

    def _check_exit_invariants(self, state: SymbolicState, func_name: str):
        """Check invariants at function exit"""

        # Check stack balance
        if state.stack_depth != 0:
            state.issues.append(
                f"Stack imbalance at return: depth={state.stack_depth} "
                f"(expected 0). Path: {' -> '.join(state.path[-5:])}"
            )

        # Check callee-saved registers
        for reg in CALLEE_SAVED:
            reg_state = state.registers.get(reg, RegState.UNKNOWN)
            if reg_state == RegState.SAVED:
                state.issues.append(
                    f"Register {reg} saved but not restored at return. "
                    f"Path: {' -> '.join(state.path[-5:])}"
                )
            elif reg_state == RegState.MODIFIED:
                state.issues.append(
                    f"Callee-saved register {reg} modified without save/restore. "
                    f"Path: {' -> '.join(state.path[-5:])}"
                )


def analyze_file(parsed: ParsedFile) -> Dict[str, FunctionAnalysis]:
    """Analyze all functions in a parsed file"""
    executor = SymbolicExecutor()
    results = {}

    for func_name, func in parsed.functions.items():
        results[func_name] = executor.analyze_function(func)

    return results


if __name__ == '__main__':
    import sys

    if len(sys.argv) < 2:
        print("Usage: python symbolic.py <file.asm>")
        sys.exit(1)

    parser = NASMParser()
    parsed = parser.parse_file(sys.argv[1])

    results = analyze_file(parsed)

    for func_name, analysis in results.items():
        print(f"\n{'='*60}")
        print(f"Function: {func_name}")
        print(f"  Paths analyzed: {len(analysis.paths)}")
        print(f"  Stack balanced: {analysis.stack_balanced}")
        print(f"  Registers preserved: {analysis.registers_preserved}")

        if analysis.indirect_calls:
            print(f"  Indirect calls:")
            for line, reg in analysis.indirect_calls:
                print(f"    Line {line}: call {reg}")

        if analysis.all_issues:
            print(f"  ISSUES:")
            for issue in analysis.all_issues:
                print(f"    - {issue}")
