# UHMA Quick Usage

## Requirements
- SBCL (Steel Bank Common Lisp)
- 8GB RAM recommended

## Load System
```bash
sbcl --dynamic-space-size 8192 --load uhma-load-only.lisp
```

## Run Full Test (66 Claims + Live Mode)
```bash
sbcl --dynamic-space-size 8192 --noinform --disable-debugger --load test-66-claims.lisp
```

This will:
1. Feed 32 UHMA source files (3 passes)
2. Run live mode for 3 hours
3. Feed all HOMOICONIC files
4. Run indefinitely with periodic saves

## Interactive Use
After loading:
```lisp
(in-package :uhma)
(reset!)                          ; Initialize
(process-chunk! "your text here") ; Feed text
(live!)                           ; Enter live mode (Ctrl+C to stop)
```

## Save/Restore State
```lisp
(save-system-state! "my-state")   ; Save
(load-system-state! "my-state")   ; Restore
(checkpoint-session!)             ; Quick checkpoint
(restore-session!)                ; Restore checkpoint
```

## Key Files
- `uhma-load-only.lisp` - Loads all 32 modules
- `test-66-claims.lisp` - Comprehensive test
- `UHMA-DESIGN-SPEC.md` - Design goals and claims
- `uhma-continuous.lisp` - Live mode
- `uhma-persistence.lisp` - State save/restore
