# UHMA Project Guidelines

## What This Is
UHMA (Unified Holographic Memory Architecture) - a self-modifying x86-64 assembly system that learns patterns through holographic/VSA encoding. It's meant to abstract, generalize, and recognize patterns like a cognitive system.

## Problem-Solving Methodology

**USE THE SYSTEM'S OWN PARADIGM FIRST.**

When you see a problem in UHMA, the solution should come from holographic/VSA thinking, not generic CS patterns:

| Problem | Wrong instinct | Right instinct |
|---------|----------------|----------------|
| O(n²) comparison | Hash table, indexes | Holographic trace + resonance query |
| Find similar items | Loop and compare | Encode + dot product |
| Group by property | Buckets, maps | Superpose into trace, query by similarity |
| Remember history | Ring buffer, list | Holographic superposition |

**The pattern:**
1. Encode the thing as a VSA vector
2. Query existing trace by dot product (similarity)
3. Superpose into trace for future queries

This is O(1) per item. The holographic memory IS the index. Don't build separate data structures when the surface already provides similarity search.

**Ask yourself:** "How would this work if the system only had holographic memory?" - then implement that.

## Hard-Learned Lessons

### Testing
- **ALWAYS stream logs** - use `tee`, never run blind waiting for completion
- **Short test FIRST** - verify with tiny_test.txt (8 tokens), THEN scale up
- **Check ALL code paths** - digest_file and process_input had different behavior (abstraction was missing in digest)

### Architecture
- **Mood/somatic binding = metadata, NOT pattern identity** - a bike is a bike whether happy or angry. Mood affects behavior, not what patterns ARE
- **Fault handler longjmps to REPL** - save/restore fault_safe_rsp/rip around calls that might fault (digest loop)
- **Context = hash(prev_token) only** - no somatic XOR, that was breaking pattern recognition
- **rcx is caller-saved** - don't use ecx/rcx in digest loop, it gets clobbered; use r10-r15 instead
- **Restore r14 after fault recovery** - process_token may clobber r14, must reload SURFACE_BASE after .continue_loop

### Emitted Patterns
- Emitted patterns work for text processing - they're valid x86 (cmp/jne/mov/ret stubs)
- The emitted code compares context hash and returns predicted token or 0

### Performance
- FACTOR now uses O(n) hash-based grouping instead of O(n²) byte comparison
- Large files still take time due to volume (11K+ tokens = 11K+ learning cycles)
- Each token goes through: hash → SEARCH → LEARN → VERIFY → EMIT
- For quick testing, use small files (< 100 tokens)

### Token Abstraction
- Hex literals (0x...) → TOKEN_HEX (0x48455821)
- Numbers (digits only) → TOKEN_NUM (0x4e554d21)
- This abstraction MUST happen in both process_input AND digest_file

### Schema System
- Schemas generalize CONTEXT (same token, similar contexts)
- They mask low bits of context hash to match broader patterns
- Dreams extract schemas from miss buffer

## Key Files
- `dispatch.asm` - token processing, prediction, hit/miss logic
- `io.asm` - digest_file for eating files
- `dreams.asm` - consolidation, schema extraction
- `receipt.asm` - event logging system
- `signal.asm` - fault handling, recovery to REPL
- `learn.asm` - pattern learning
- `vsa.asm` - holographic vector operations

## Common Commands
```bash
# Quick test
rm -f uhma.surface && ./uhma < /tmp/tiny_cmd.txt 2>&1 | grep -E "HIT|NEW"

# Streaming test
./uhma < commands.txt 2>&1 | tee /tmp/test.log

# Check HIT ratio
grep -c "HIT" /tmp/test.log && grep -c "NEW" /tmp/test.log
```
