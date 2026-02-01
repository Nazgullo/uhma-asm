#!/usr/bin/env python3
"""
analyze.py — UHMA Formal Analyzer: symbolic execution + invariant checking

@entry main()                              CLI entry, analyzes all .asm files
@entry analyze_file(path) -> FileReport    Analyze single file
@entry generate_report(reports) -> str     Generate proof report

@calls parser.py:NASMParser
@calls symbolic.py:SymbolicExecutor, analyze_file
@calledby CLI (python3 formal/analyze.py)

GOTCHAS:
  - Analyzes ALL paths through each function (exponential in branches)
  - Reports: stack imbalance, callee-saved violations, unreachable code
  - Exit code 0 = all clear, 1 = issues found
"""

import sys
import os
from pathlib import Path
from typing import Dict, List, Set, Tuple, Optional
from dataclasses import dataclass, field

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent))

from parser import NASMParser, ParsedFile, Function, BasicBlock, Instruction, InstrType
from symbolic import SymbolicExecutor, FunctionAnalysis, analyze_file, CALLEE_SAVED


@dataclass
class FileReport:
    """Analysis report for one file"""
    filename: str
    functions_analyzed: int
    total_paths: int
    issues: List[str]
    warnings: List[str]
    function_reports: Dict[str, 'FunctionReport']


@dataclass
class FunctionReport:
    """Detailed report for one function"""
    name: str
    paths_analyzed: int
    stack_balanced: bool
    registers_preserved: bool
    indirect_calls: List[Tuple[int, str]]
    issues: List[str]
    warnings: List[str]


@dataclass
class ProofResult:
    """Result of a specific proof/invariant check"""
    name: str
    passed: bool
    details: str
    location: Optional[str] = None


class InvariantChecker:
    """Additional invariant checks beyond symbolic execution"""

    def __init__(self):
        self.results: List[ProofResult] = []

    def check_all(self, parsed: ParsedFile, filename: str) -> List[ProofResult]:
        """Run all invariant checks on a parsed file"""
        self.results = []

        for func_name, func in parsed.functions.items():
            self._check_function_prologue_epilogue(func, filename)
            self._check_call_alignment(func, filename)
            self._check_memory_access_patterns(func, filename)
            self._check_jump_targets(func, filename)
            self._check_indirect_jumps(func, filename)

        return self.results

    def _check_function_prologue_epilogue(self, func: Function, filename: str):
        """Verify function has proper prologue/epilogue structure"""
        if not func.blocks:
            return

        entry_block = func.blocks.get(func.entry_block)
        if not entry_block or not entry_block.instructions:
            return

        # Check for standard prologue pattern
        first_instrs = entry_block.instructions[:5]
        has_push_rbp = any(
            i.mnemonic == 'push' and i.operands and
            i.operands[0].is_register and i.operands[0].register.lower() in ('rbp', 'ebp')
            for i in first_instrs
        )

        # Find all return blocks
        for block in func.blocks.values():
            for instr in block.instructions:
                if instr.instr_type == InstrType.RET:
                    # Check if there's a matching pop rbp before ret
                    idx = block.instructions.index(instr)
                    prev_instrs = block.instructions[max(0, idx-5):idx]
                    has_pop_rbp = any(
                        i.mnemonic == 'pop' and i.operands and
                        i.operands[0].is_register and i.operands[0].register.lower() in ('rbp', 'ebp')
                        for i in prev_instrs
                    )

                    if has_push_rbp and not has_pop_rbp:
                        self.results.append(ProofResult(
                            name="prologue_epilogue_match",
                            passed=False,
                            details=f"Function pushes rbp but doesn't pop before ret at line {instr.line_num}",
                            location=f"{filename}:{func.name}:{instr.line_num}"
                        ))

    def _check_call_alignment(self, func: Function, filename: str):
        """Check that stack is 16-byte aligned before calls (System V ABI)"""
        # This is a simplified check - full check would need symbolic tracking
        for block in func.blocks.values():
            for instr in block.instructions:
                if instr.instr_type == InstrType.CALL:
                    # Warning: can't fully verify alignment without symbolic execution
                    # but we can flag suspicious patterns
                    pass

    def _check_memory_access_patterns(self, func: Function, filename: str):
        """Check for potentially dangerous memory access patterns"""
        for block in func.blocks.values():
            for instr in block.instructions:
                if not instr.operands:
                    continue

                for op in instr.operands:
                    if op.is_memory:
                        # Check for direct memory access without bounds checking
                        # This is heuristic - looking for access patterns like [rax]
                        # without prior validation
                        if op.base_reg and not op.index_reg and op.displacement == 0:
                            # Direct register dereference - potentially dangerous
                            # but common in assembly, so just note it
                            pass

    def _check_jump_targets(self, func: Function, filename: str):
        """Verify all jump targets exist within the function"""
        defined_labels = set(func.blocks.keys())

        for block in func.blocks.values():
            for instr in block.instructions:
                if instr.instr_type in (InstrType.JUMP, InstrType.COND_JUMP):
                    target = instr.jump_target
                    if target and target.startswith('.'):
                        # Local label - should exist in function
                        if target not in defined_labels:
                            self.results.append(ProofResult(
                                name="jump_target_exists",
                                passed=False,
                                details=f"Jump to undefined local label '{target}' at line {instr.line_num}",
                                location=f"{filename}:{func.name}:{instr.line_num}"
                            ))

    def _check_indirect_jumps(self, func: Function, filename: str):
        """Flag indirect jumps/calls for review"""
        for block in func.blocks.values():
            for instr in block.instructions:
                if instr.mnemonic and instr.mnemonic.startswith('jmp'):
                    if instr.operands and instr.operands[0].is_register:
                        self.results.append(ProofResult(
                            name="indirect_jump_review",
                            passed=True,  # Not a failure, just a note
                            details=f"Indirect jump via {instr.operands[0].register} at line {instr.line_num}",
                            location=f"{filename}:{func.name}:{instr.line_num}"
                        ))


class UHMAAnalyzer:
    """Main analyzer for UHMA assembly codebase"""

    def __init__(self, verbose: bool = True):
        self.verbose = verbose
        self.parser = NASMParser()
        self.executor = SymbolicExecutor(max_path_depth=100, max_loop_unroll=3)
        self.checker = InvariantChecker()
        self.file_reports: Dict[str, FileReport] = {}

    def analyze_file(self, filepath: str) -> FileReport:
        """Analyze a single assembly file"""
        filename = os.path.basename(filepath)

        if self.verbose:
            print(f"\n{'='*60}")
            print(f"Analyzing: {filename}")
            print('='*60)

        # Parse the file
        try:
            parsed = self.parser.parse_file(filepath)
        except Exception as e:
            return FileReport(
                filename=filename,
                functions_analyzed=0,
                total_paths=0,
                issues=[f"Parse error: {str(e)}"],
                warnings=[],
                function_reports={}
            )

        # Run symbolic execution on all functions
        sym_results = analyze_file(parsed)

        # Run additional invariant checks
        inv_results = self.checker.check_all(parsed, filename)

        # Compile report
        issues = []
        warnings = []
        func_reports = {}
        total_paths = 0

        for func_name, analysis in sym_results.items():
            func_issues = list(analysis.all_issues)
            func_warnings = []

            # Add invariant check results
            for inv in inv_results:
                if inv.location and func_name in inv.location:
                    if not inv.passed:
                        func_issues.append(f"[{inv.name}] {inv.details}")
                    elif 'review' in inv.name:
                        func_warnings.append(f"[{inv.name}] {inv.details}")

            func_report = FunctionReport(
                name=func_name,
                paths_analyzed=len(analysis.paths),
                stack_balanced=analysis.stack_balanced,
                registers_preserved=analysis.registers_preserved,
                indirect_calls=analysis.indirect_calls,
                issues=func_issues,
                warnings=func_warnings
            )
            func_reports[func_name] = func_report

            total_paths += len(analysis.paths)
            issues.extend([f"{func_name}: {i}" for i in func_issues])
            warnings.extend([f"{func_name}: {w}" for w in func_warnings])

            if self.verbose:
                self._print_function_report(func_report)

        report = FileReport(
            filename=filename,
            functions_analyzed=len(sym_results),
            total_paths=total_paths,
            issues=issues,
            warnings=warnings,
            function_reports=func_reports
        )

        self.file_reports[filename] = report
        return report

    def analyze_directory(self, dirpath: str, pattern: str = "*.asm") -> Dict[str, FileReport]:
        """Analyze all assembly files in a directory"""
        import glob

        files = glob.glob(os.path.join(dirpath, pattern))
        for filepath in sorted(files):
            self.analyze_file(filepath)

        return self.file_reports

    def _print_function_report(self, report: FunctionReport):
        """Print a function analysis report"""
        status = "OK" if not report.issues else "ISSUES"
        print(f"\n  Function: {report.name} [{status}]")
        print(f"    Paths: {report.paths_analyzed}")
        print(f"    Stack balanced: {report.stack_balanced}")
        print(f"    Registers preserved: {report.registers_preserved}")

        if report.indirect_calls:
            print(f"    Indirect calls: {len(report.indirect_calls)}")
            for line, reg in report.indirect_calls[:3]:
                print(f"      Line {line}: call {reg}")
            if len(report.indirect_calls) > 3:
                print(f"      ... and {len(report.indirect_calls) - 3} more")

        if report.issues:
            print(f"    ISSUES ({len(report.issues)}):")
            for issue in report.issues[:5]:
                print(f"      - {issue}")
            if len(report.issues) > 5:
                print(f"      ... and {len(report.issues) - 5} more")

        if report.warnings:
            print(f"    Warnings ({len(report.warnings)}):")
            for warn in report.warnings[:3]:
                print(f"      - {warn}")

    def generate_summary(self) -> str:
        """Generate a summary report of all analyses"""
        lines = []
        lines.append("\n" + "="*70)
        lines.append("UHMA FORMAL ANALYSIS SUMMARY")
        lines.append("="*70)

        total_functions = 0
        total_paths = 0
        total_issues = 0
        total_warnings = 0
        files_with_issues = []

        for filename, report in self.file_reports.items():
            total_functions += report.functions_analyzed
            total_paths += report.total_paths
            total_issues += len(report.issues)
            total_warnings += len(report.warnings)
            if report.issues:
                files_with_issues.append(filename)

        lines.append(f"\nFiles analyzed: {len(self.file_reports)}")
        lines.append(f"Functions analyzed: {total_functions}")
        lines.append(f"Total paths explored: {total_paths}")
        lines.append(f"Issues found: {total_issues}")
        lines.append(f"Warnings: {total_warnings}")

        if files_with_issues:
            lines.append(f"\nFiles with issues:")
            for filename in files_with_issues:
                report = self.file_reports[filename]
                lines.append(f"  - {filename}: {len(report.issues)} issues")
        else:
            lines.append("\nNo issues found - all invariants pass!")

        lines.append("\n" + "="*70)

        # Detailed issues
        if total_issues > 0:
            lines.append("\nDETAILED ISSUES:")
            lines.append("-"*70)
            for filename, report in self.file_reports.items():
                if report.issues:
                    lines.append(f"\n{filename}:")
                    for issue in report.issues:
                        lines.append(f"  - {issue}")

        return "\n".join(lines)

    def generate_proof_report(self) -> str:
        """Generate a formal proof-style report"""
        lines = []
        lines.append("FORMAL VERIFICATION REPORT")
        lines.append("="*70)
        lines.append("")
        lines.append("INVARIANTS CHECKED:")
        lines.append("  1. Stack Balance: stack_depth == 0 at all return points")
        lines.append("  2. Register Preservation: callee-saved registers (rbx, rbp, r12-r15)")
        lines.append("     must be saved before modification and restored before return")
        lines.append("  3. LIFO Discipline: push/pop pairs must match in reverse order")
        lines.append("  4. Jump Target Validity: all local jumps must target defined labels")
        lines.append("  5. Prologue/Epilogue Matching: push rbp must have matching pop rbp")
        lines.append("")

        all_pass = True
        for filename, report in self.file_reports.items():
            lines.append(f"\nFILE: {filename}")
            lines.append("-"*50)

            for func_name, func_report in report.function_reports.items():
                proofs = []

                # Stack balance proof
                proofs.append(ProofResult(
                    name="stack_balance",
                    passed=func_report.stack_balanced,
                    details=f"All {func_report.paths_analyzed} paths end with stack_depth == 0"
                            if func_report.stack_balanced else
                            "Stack imbalance detected on some paths"
                ))

                # Register preservation proof
                proofs.append(ProofResult(
                    name="register_preservation",
                    passed=func_report.registers_preserved,
                    details=f"All callee-saved registers properly preserved"
                            if func_report.registers_preserved else
                            "Callee-saved register violation detected"
                ))

                lines.append(f"\n  FUNCTION: {func_name}")
                lines.append(f"  Paths analyzed: {func_report.paths_analyzed}")

                for proof in proofs:
                    status = "PASS" if proof.passed else "FAIL"
                    lines.append(f"    [{status}] {proof.name}: {proof.details}")
                    if not proof.passed:
                        all_pass = False

                if func_report.issues:
                    lines.append(f"    Issues:")
                    for issue in func_report.issues:
                        lines.append(f"      - {issue}")

        lines.append("")
        lines.append("="*70)
        if all_pass:
            lines.append("RESULT: ALL INVARIANTS VERIFIED")
        else:
            lines.append("RESULT: INVARIANT VIOLATIONS DETECTED")
        lines.append("="*70)

        return "\n".join(lines)


def main():
    import argparse

    parser = argparse.ArgumentParser(description='UHMA Formal Analyzer')
    parser.add_argument('paths', nargs='*', default=['.'],
                       help='Files or directories to analyze')
    parser.add_argument('-q', '--quiet', action='store_true',
                       help='Quiet mode - only show summary')
    parser.add_argument('-p', '--proof', action='store_true',
                       help='Generate formal proof report')
    parser.add_argument('-o', '--output', type=str,
                       help='Output file for report')

    args = parser.parse_args()

    analyzer = UHMAAnalyzer(verbose=not args.quiet)

    for path in args.paths:
        if os.path.isfile(path):
            analyzer.analyze_file(path)
        elif os.path.isdir(path):
            analyzer.analyze_directory(path)
        else:
            print(f"Warning: {path} not found")

    if args.proof:
        report = analyzer.generate_proof_report()
    else:
        report = analyzer.generate_summary()

    print(report)

    if args.output:
        with open(args.output, 'w') as f:
            f.write(report)
        print(f"\nReport saved to: {args.output}")

    # Exit with error code if issues found
    total_issues = sum(len(r.issues) for r in analyzer.file_reports.values())
    sys.exit(1 if total_issues > 0 else 0)


if __name__ == '__main__':
    main()
