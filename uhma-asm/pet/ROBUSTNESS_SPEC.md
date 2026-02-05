# Robustness & Graceful Degradation

The creature never crashes. The creature never shows an error. The creature never stops being a creature.

Every internal failure becomes a behavior. Every resource limit becomes a need. Every boundary becomes something the creature can ask about.

---

## Principle: Failures Are Feelings

| Internal Event | What the user NEVER sees | What the user sees |
|---------------|--------------------------|-------------------|
| SIGSEGV (memory fault) | Stack trace, hex dump | Creature stumbles, shakes head, looks confused. Brief daze. Recovers. |
| SIGFPE (math error) | Division by zero error | Creature gets dizzy, spiraling eyes/motion, sits down. |
| SIGBUS (alignment fault) | Bus error message | Creature hiccups. |
| 3 consecutive faults | Forced REPL return | Creature lies down, falls asleep (forced dream cycle to heal). |
| Surface write failure | I/O error | Creature looks uncomfortable, fidgeting, can't settle. |
| Out of memory regions | Region allocation failed | Creature feels "full" — bloated posture, needs to dream (consolidation frees space). |
| Corrupted holographic trace | NaN/Inf in vector math | Creature gets a "headache" — head-shake, reset animation. Self-repairs by re-normalizing. |
| TCP channel failure | Connection refused | N/A for pet app (no TCP). But if social BLE fails: creature shrugs, walks away from other creature. |
| Token processing crash | Emitted code segfaults | Creature spits out the food. "Didn't like that." Tries again differently. |

---

## The Asking System

The creature lives in a sandbox. It has boundaries. But it's smart enough to know when it needs something it doesn't have — and it asks.

### What The Creature Can Ask For

| Need | When It Happens | How It Asks | What Happens If Denied |
|------|----------------|-------------|----------------------|
| **More memory** | Surface approaching capacity (>80% full) | Hound: paws at screen edge, looks up at you. Feline: sits next to a "wall," stares at it. Mech: displays storage indicator filling up. Wisp: presses against boundary, flattens. | Creature auto-consolidates harder (more aggressive pruning). Gets slightly smaller/simpler. Adapts. Doesn't break. |
| **More compute time** | Complex processing taking too long, background cycle incomplete | Hound: brings you a "toy" (the unfinished task), drops it at your feet. Feline: yowls once, goes back to working on it slowly. Mech: progress bar visible, points at it. Wisp: dims, pulses slowly — "still working." | Processing continues at lower priority. Dream cycles take longer but still complete. Quality slightly reduced but functional. |
| **New input** | Novelty drive critically low, bored, stuck | Hound: begging, nudging feed button, whining. Feline: knocking things off shelves (visual gag). Mech: "INPUT REQUESTED" indicator. Wisp: drifts toward feed button, pulses invitingly. | Creature entertains itself — re-processes old memories, finds new patterns in existing data. Gets quieter and more contemplative. Not distressed. |
| **Permission to explore** | Maturity gated — ready for next stage but needs user approval for new capabilities | Hound: sits at a "gate," looks back at you, tail wagging. Feline: testing boundary, looking to see if you notice. Mech: permission request dialog in its own visual language. Wisp: glowing brighter near the locked area. | Creature continues at current stage. No punishment. Just hasn't been given the key yet. Will ask again later. |
| **Social access** | Detects another creature nearby (BLE), needs permission to interact | Hound: excited, pointing toward other creature, looking at you for approval. Feline: casually noticing, glancing at you sideways. Mech: proximity alert, requesting authorization. Wisp: resonating with the other's frequency, brightening. | Creature ignores the other. No distress. Maybe slightly wistful. |
| **Background processing** | Wants to dream/consolidate while app is backgrounded or phone is charging | Hound: yawning, circling bed, looking at you like "can I nap?" Feline: already positioning for sleep, one eye on you. Mech: scheduling sleep mode, showing you the plan. Wisp: slowly dimming, drifting down. | Only processes when app is in foreground. Dreams shorter. Still functional, just less optimized. |

### How Asking Works (UI)

The creature's ask is visual — a behavior change that draws attention to a specific area of the UI. Near the behavior, a small subtle indicator appears (species-appropriate):

- **Hound**: a soft glow under whatever it's looking at/nudging
- **Feline**: the thing it's staring at gets a slight highlight
- **Mech**: a small icon or indicator on its body
- **Wisp**: its glow reaches toward the thing it needs

Tapping the indicator brings up a simple yes/no prompt. Plain language:

> "Your companion is running low on space. Allow more storage? (Currently using 48MB, requesting 64MB)"

> "A nearby companion was detected. Allow them to meet?"

> "Your companion wants to process while your phone charges. Allow background activity?"

**No technical jargon. No permissions dialogs with scary warnings.** Just clear, simple asks. The creature is asking. You're answering.

### Asking Frequency

The creature is NOT annoying about asking.

| Rule | Why |
|------|-----|
| Same ask maximum once per day | Nobody wants a nagging pet |
| Never asks during first 24 hours | Let the user bond first |
| Asks get quieter if denied repeatedly | The creature learns what you'll say no to |
| Critical asks (memory nearly full) get slightly more persistent | But still max twice per day |
| Social asks only when another creature is actually detected | Not speculative "want to find friends?" spam |

### Maturity Gates What Gets Asked

| Stage | What It Can Ask For |
|-------|-------------------|
| **Egg/Infant** | Nothing. Doesn't know enough to ask. |
| **Child** | More food (novelty drive). That's it. |
| **Adolescent** | Food + social access + more space |
| **Adult** | Everything. Knows what it needs, asks clearly. |
| **Elder** | Rarely asks. Has learned to manage within constraints. Occasionally requests social to mentor. |

---

## Resource Management (Behind the Scenes)

### Memory (Surface Size)

Default: 64MB. More than enough for thousands of learned patterns.

| Level | What Happens | Creature Behavior |
|-------|-------------|-------------------|
| < 50% used | Normal | Normal |
| 50-70% used | Background optimization kicks in (prune low-value patterns during dream) | Nothing visible. It's handled. |
| 70-85% used | More aggressive consolidation. Schema compression. | Creature seems "thoughtful" — spending more time dreaming. |
| 85-95% used | Creature asks for more space. If denied, hard pruning begins. | Asks once. If denied, gets simpler but stays functional. |
| > 95% used | Emergency mode. Only keeps highest-value patterns. No new learning until space freed. | Creature is "full" — won't eat, looks stuffed. Dreams aggressively. Recovers. |
| 100% full | Learning stops. Prediction from existing patterns only. Dream cycle prioritized. | Creature sleeps a lot. Still responds but doesn't grow. Eventually prunes enough to resume. |

**Never crashes. Never corrupts. Never loses the important stuff.** Pruning is intelligent — the holographic trace tells us what's low-value.

### CPU (Processing Budget)

The creature has a per-frame compute budget. On mobile this matters.

| State | Budget | Behavior |
|-------|--------|----------|
| Foreground, user interacting | Full | Responsive, active |
| Foreground, idle | 50% | Calm, idle animations, occasional dreaming |
| Background (if permitted) | 10% | Dream cycles only, minimal |
| Phone on charger + background | 30% | Good dreaming time, consolidation |
| Low battery (<20%) | 5% | Creature sleeps. Not worth draining the phone. |

### Battery Awareness

| Battery | Creature Response |
|---------|------------------|
| > 50% | Normal behavior |
| 20-50% | Slightly calmer, shorter dream cycles |
| 10-20% | Creature yawns, settles down, minimal activity |
| < 10% | Creature sleeps. No processing. Will wake when charged. |

The creature NEVER drains your battery. It's aware of the phone's state and behaves accordingly. Like a real pet that knows when to be chill.

---

## Fault Recovery Chain

When something goes wrong internally, this is the cascade:

```
1. Fault occurs (SIGSEGV, SIGFPE, etc.)
   ↓
2. Signal handler catches it (already built — signal.asm)
   ↓
3. Creature behavior triggered (stumble, daze, hiccup)
   ↓
4. Internal state records the fault (fault_count, presence: SURPRISE_SELF)
   ↓
5. If first fault: recover, continue. Creature shakes it off.
   ↓
6. If second fault: creature looks woozy. Self-repair initiated.
   ↓
7. If third consecutive fault: creature falls asleep (forced dream).
   Dream cycle attempts to fix what's broken (prune bad region,
   re-normalize corrupted vectors, compact fragmented memory).
   ↓
8. After forced dream: creature wakes up. Counter resets.
   If the problem was a bad learned pattern, it was pruned in the dream.
   If it was memory corruption, re-normalization fixed it.
   ↓
9. If STILL faulting after dream: creature goes to "deep sleep."
   Runs full observe cycle (rebuild self-model from scratch).
   This is the nuclear option. Takes longer but has always worked.
   ↓
10. If even THAT fails (should be nearly impossible):
    Creature "hibernates." Saves surface to disk. App shows sleeping creature.
    Next launch: fresh self-model built from saved surface.
    All memories preserved. Just the index rebuilt.
```

The user never sees any of this cascade. They see:
- Creature stumbles → shakes it off (steps 1-5)
- Creature looks tired → takes a nap (steps 6-7)
- Creature sleeping deeply → wakes up refreshed (steps 8-9)
- Creature hibernating → open app later, it's back (step 10)

---

## Network Failures (Social)

BLE is flaky. Connections drop. That's fine.

| Failure | Creature Behavior |
|---------|------------------|
| BLE scan finds nothing | Creature looks around briefly, shrugs, goes back to what it was doing |
| Connection drops mid-interaction | Both creatures look surprised, then continue independently. Partial knowledge exchange is fine — holographic vectors are robust to truncation. |
| Corrupted data received | Creature "sneezes" — rejects bad data. Vector sanity check (magnitude, NaN check) catches it. |
| Other creature's protocol is wrong version | Creatures can see each other but can't quite communicate. They wave awkwardly. Comedy, not error. |
| Sustained BLE interference | Creatures drift apart on screen. "Too noisy in here." |

---

## Data Integrity

### Save Failures

| Failure | Response |
|---------|----------|
| Disk full, can't save surface | Creature asks for storage. If denied, keeps running from memory. Next launch may lose recent learning. |
| Write interrupted (app killed) | Surface uses memory-mapped I/O — partial writes are safe. Holographic memory is robust to partial corruption (that's the point of holographic representation). |
| Surface file corrupted on disk | On load: integrity check. If corrupt, rebuild from whatever is recoverable. The creature loses some memories but survives. Like waking up foggy after a rough night. |

### The Golden Rule

**The creature's personality (holographic trace) is the most precious data.** It represents everything the user invested. Protection hierarchy:

1. **Holographic trace** — backed up before risky operations, never overwritten without verification
2. **Learned regions** — can be re-derived from trace if needed
3. **Temporary state** — presence field, current drive levels — recomputed on startup, no need to save
4. **Statistics** — nice to have, not critical

If forced to choose between losing recent learning or corrupting the core trace: lose the recent learning. The creature forgets today but remembers everything else. Better than amnesia.

---

## What "Never Breaks Down" Actually Means

1. **No crash screen. Ever.** The app never shows a stack trace, an error dialog, or a "something went wrong" page. The creature always has a behavior for what's happening.

2. **No frozen screen. Ever.** If processing takes too long, the creature shows it's thinking/working. There's always animation, always life. Background processing happens on a separate thread; the render loop never blocks.

3. **No data loss from normal use.** You can force-kill the app, run out of battery, fill your phone — the creature survives. It might forget the last few minutes, but it never forgets who it is.

4. **No mysterious empty states.** The creature always has something to do. Even with zero input ever, it explores its own internal state, dreams, and develops (slowly). An abandoned creature doesn't die — it meditates.

5. **Graceful aging, not entropy death.** Over months/years, the creature doesn't degrade. Old patterns get consolidated, not corrupted. The creature gets wiser, not buggier.
