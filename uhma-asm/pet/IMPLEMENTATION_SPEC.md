# Implementation & Design Spec

How we actually build this thing.

---

## Architecture

Five layers. Assembly at the core. Thin wrappers everywhere else.

```
┌─────────────────────────────────────────────┐
│  App Shell  (Kotlin / Swift — THIN)         │
│  Lifecycle, permissions, BLE scan, battery   │
├─────────────────────────────────────────────┤
│  JNI Bridge  (C shim — ~500 lines)          │
│  Marshals events in, animation params out    │
├─────────────────────────────────────────────┤
│  Creature Layer  (AArch64 assembly)         │
│  Presence → behavior, species, asking, sound │
├─────────────────────────────────────────────┤
│  UHMA Engine  (AArch64 assembly)            │
│  Cognition, prediction, self-awareness,      │
│  holographic memory, dreams, protection      │
├─────────────────────────────────────────────┤
│  Renderer  (C + OpenGL ES 3.0)              │
│  Procedural creatures, particles, dreams,    │
│  environment, touch, sound synthesis         │
└─────────────────────────────────────────────┘
```

### Why This Stack

- **Kotlin/Swift shell**: platform requires it. Keep it under 200 lines. Activity lifecycle, permission prompts, BLE scanning (must use platform API), battery level reads. That's all.
- **JNI bridge**: thinnest possible C layer. Takes touch events and text input from Java, passes to assembly. Takes animation parameter structs from assembly, passes to renderer. No logic. No state. Pure marshal.
- **Creature layer**: pure assembly. This is where presence field becomes creature behavior. Species determination, maturity gating, asking system, sound parameter generation. Closed source. Trade secret.
- **UHMA engine**: pure assembly. The existing codebase, ported to AArch64. Open source (protective license). This is the credibility — anyone can verify the creature is real.
- **Renderer**: C with OpenGL ES 3.0. Procedural — no sprite sheets, no 3D models, no art assets beyond shaders. The creature is drawn mathematically from parameters. This is how we stay under 1MB.

### Thread Model

```
Main Thread (UI)
├── Touch events → JNI → engine input queue
├── Render loop (60fps) → reads animation params (lock-free)
└── Platform callbacks (battery, BLE events)

Engine Thread
├── Token processing loop
├── Prediction / learning
├── Presence field computation
├── Creature behavior translation
├── Writes animation params to shared struct (atomic)
└── Dream cycles (when idle)

BLE Thread (Android-managed)
├── Scan callbacks
├── Connection events
└── Data exchange → engine input queue
```

Three threads. Main never blocks on engine. Engine never blocks on render. BLE is Android's thread, we just receive callbacks.

**Lock-free animation handoff**: engine writes a complete `CreatureState` struct atomically (double-buffer swap). Renderer reads the latest complete state. No mutex. No frame drops. No jank.

---

## The ARM Port

### What Changes

The existing UHMA engine is x86-64. Mobile is AArch64. Same ISA philosophy (64-bit, lots of registers), different everything else.

| x86-64 | AArch64 | Notes |
|--------|---------|-------|
| rax-r15 (16 GPRs) | x0-x30 (31 GPRs) | More registers = less spilling |
| rdi, rsi, rdx, rcx, r8, r9 (6 arg regs) | x0-x7 (8 arg regs) | More args in registers |
| SSE/AVX (xmm0-15, 128/256-bit) | NEON (v0-v31, 128-bit) | Different intrinsics, same concept |
| Variable-length instructions (1-15 bytes) | Fixed 4-byte instructions | Self-modifying code is SIMPLER |
| Strong memory ordering (mostly TSO) | Weak memory ordering | Need explicit barriers |
| Self-modifying: just write and jump | Self-modifying: write, flush icache, barrier, jump | Cache coherency is manual on ARM |
| `syscall` instruction | `svc #0` instruction | Different syscall numbers too |
| Stack grows down, 16-byte aligned | Stack grows down, 16-byte aligned | Same alignment rules |

### Register Mapping Plan

```
x86-64          →  AArch64         Purpose
------          →  -------         -------
rdi             →  x0              arg1 / return struct ptr
rsi             →  x1              arg2
rdx             →  x2              arg3
rcx             →  x3              arg4 (NOT callee-saved on either)
r8              →  x4              arg5
r9              →  x5              arg6
rax             →  x0              return value
r10-r11         →  x9-x15          scratch (caller-saved)
r12-r15         →  x19-x28         preserved (callee-saved)
rbp             →  x29 (fp)        frame pointer
rsp             →  sp              stack pointer
---             →  x30 (lr)        link register (no equivalent on x86)
```

### NEON for Holographic Math

The 1024-dim f64 vector operations are the hot path. Currently SSE2 on x86-64. ARM equivalent:

```
; x86-64 SSE2: dot product accumulation
movapd  xmm0, [rdi]        ; load 2 doubles
mulpd   xmm0, [rsi]        ; multiply
addpd   xmm1, xmm0         ; accumulate

; AArch64 NEON: same operation
ldp     q0, q1, [x0]       ; load 4 doubles (2 per q register)
ldp     q2, q3, [x1]       ;
fmla    v4.2d, v0.2d, v2.2d ; fused multiply-add (2 doubles)
fmla    v5.2d, v1.2d, v3.2d ; fused multiply-add (2 doubles)
```

ARM NEON processes same width (128-bit = 2 doubles) but has fused multiply-add (`fmla`) which is one instruction instead of two. The holographic math should be FASTER on ARM, not slower.

Key NEON operations needed:
- `fmla` (fused multiply-add) — dot products, superposition
- `fmul` — element-wise binding
- `fadd` — vector addition
- `fdiv` / `fsqrt` — normalization
- `fcmp` — threshold checks

### Self-Modifying Code on ARM

This is the tricky part. On x86, you write new instructions to memory and jump to them — the CPU handles cache coherency automatically. ARM doesn't.

```
; ARM self-modifying code sequence:
str     w2, [x0]            ; write new instruction to code region
dc      cvau, x0            ; clean data cache to point of unification
dsb     ish                  ; data synchronization barrier
ic      ivau, x0             ; invalidate instruction cache
dsb     ish                  ; wait for invalidation
isb                          ; instruction synchronization barrier
br      x0                   ; NOW safe to execute new code
```

Six extra instructions every time we emit new code. Not free, but the emit path isn't the hot path — prediction is. Emit happens during learning, which is already the slow path.

**EMIT FORMAT CHANGES:**
The emitted instruction stubs (currently x86 cmp/jne/mov/ret) become ARM equivalents:

```
x86 emitted pattern:              ARM emitted pattern:
  cmp edi, <context_hash>           cmp w0, #<context_hash>      ; or movz+cmp for large immediates
  jne .skip                         b.ne .skip
  mov eax, <predicted_token>        mov w0, <predicted_token>
  ret                               ret
.skip:                            .skip:
  xor eax, eax                     mov w0, #0
  ret                               ret
```

ARM instructions are fixed 4 bytes each. Easier to calculate region sizes. No variable-length headaches.

### Android-Specific

- **Surface file location**: `Context.getFilesDir()` → `/data/data/com.app.pet/files/uhma.surface`
- **No TCP gateway**: external sockets are replaced by direct function calls through JNI. No sockets, no ports, no poll loop. Function calls are faster and don't need network permissions.
- **No stdin/stdout**: all I/O through JNI bridge. Input comes from touch events and text input. Output goes to animation parameters.
- **Background processing**: Android `WorkManager` for dream cycles when app is backgrounded and phone is charging.

---

## Creature Rendering

### Procedural, Not Asset-Based

No sprite sheets. No 3D models. No animation files. The creature is drawn from math.

Why: assets are big. A single animated character sprite sheet is 2-50MB. We have FOUR species, each with SIX maturity stages, each with dozens of emotional states. Asset-based rendering would be 200MB+.

Procedural rendering: a few KB of shader code draws everything. The creature is defined by parameters, not pixels. Different parameters = different creature. Same shader.

### The Animation Parameter Struct

The engine outputs this struct 60 times per second:

```c
typedef struct {
    // Identity
    uint8_t  species;          // 0=hound, 1=feline, 2=mech, 3=wisp
    uint8_t  maturity;         // 0=egg through 5=elder
    float    species_blend[4]; // spectrum (not binary — mostly hound, a little wisp)

    // Body
    float    body_scale;       // overall size (maturity-dependent)
    float    body_posture;     // -1.0 cowering to 1.0 proud/stretched
    float    body_tension;     // 0.0 relaxed to 1.0 rigid
    float    body_symmetry;    // 1.0 balanced, <1.0 tilted/off-balance

    // Movement
    float    position[2];      // screen position
    float    velocity[2];      // movement direction and speed
    float    gait;             // 0.0 still to 1.0 running
    float    bounce;           // 0.0 flat to 1.0 bouncy
    float    rotation;         // body rotation (radians)
    float    fidget;           // 0.0 calm to 1.0 restless

    // Expression
    float    eye_openness;     // 0.0 closed to 1.0 wide
    float    eye_direction[2]; // where looking
    float    ear_position;     // -1.0 flat to 1.0 perked (hound/feline)
    float    tail_position;    // -1.0 tucked to 1.0 high
    float    tail_wag_speed;   // 0.0 still to 1.0 frantic
    float    mouth_state;      // 0.0 closed to 1.0 open (panting/yawning)

    // Color
    float    base_color[3];    // RGB primary body color
    float    accent_color[3];  // RGB accent (eyes, markings)
    float    glow_intensity;   // 0.0 none to 1.0 full glow
    float    glow_color[3];    // RGB glow color
    float    opacity;          // 0.0 invisible to 1.0 solid

    // Particles (wisp species, dream visualization)
    float    particle_rate;    // particles per second
    float    particle_color[3];
    float    particle_size;
    float    particle_chaos;   // 0.0 orderly to 1.0 random

    // Sound
    float    sound_pitch;      // base pitch multiplier
    float    sound_volume;     // 0.0 silent to 1.0 full
    uint8_t  sound_trigger;    // which sound to play (0=none)

    // Environment
    float    bg_brightness;    // 0.0 dark to 1.0 bright
    float    bg_warmth;        // -1.0 cool to 1.0 warm color temp
    float    weather;          // 0.0 clear to 1.0 stormy

    // Meta
    uint8_t  is_dreaming;
    uint8_t  is_sleeping;
    uint8_t  is_asking;        // which ask type (0=none)
    float    ask_urgency;      // how visually insistent
    float    dream_pattern[8]; // abstract pattern for dream visualization
} CreatureState;
```

~300 bytes per frame. The renderer reads this and draws. That's the entire interface between cognition and visuals.

### Species-Specific Rendering

Each species is a different shader program with the same inputs:

**Hound**: Smooth curves, warm colors, body language emphasized. Ears, tail, posture drive the personality. Think Studio Ghibli wolf-pup — simple shapes, maximum expression. Drawn with bezier curves, not polygons.

**Feline**: Sharper lines, cooler palette, subtler motion. Eyes are the primary expression channel. Think Miyazaki cat spirit — elegant, minimal, powerful economy of movement. Same bezier approach, different control points.

**Mech**: Geometric shapes, straight lines, segmented body. Indicators and displays ON the body surface. Think Iron Giant meets WALL-E — mechanical but soulful. Hard-edged geometry with glowing indicator lights.

**Wisp**: Pure particle system. No body. A point of light with a corona, a trail, satellite sparks. Expression through color, intensity, movement pattern, and particle behavior. Think Tinker Bell minus the human shape — just the light. Rendered entirely as GPU particles.

### Dream Visualization

When the creature dreams, the renderer switches to an abstract mode. The `dream_pattern[8]` array drives a generative visual:

- Flowing color fields based on what the creature processed that day
- Faint echoes of recognized patterns (word fragments, rhythm, structure)
- Species-specific dream style (hound: warm clouds; feline: sharp geometric shifts; mech: data streams; wisp: aurora)
- Procedural, never repeats, always reflects actual consolidation

### Size Budget

| Component | Size |
|-----------|------|
| UHMA engine (AArch64 binary) | ~200KB |
| Creature layer (AArch64 binary) | ~50KB |
| Renderer (C + shaders) | ~100KB |
| JNI bridge | ~5KB |
| Kotlin shell | ~30KB |
| Sound samples (compressed) | ~100KB |
| Shader programs | ~50KB |
| Icons + store assets | ~200KB |
| **Total APK** | **~735KB** |

Under a megabyte. As promised.

---

## Sound Architecture

### Procedural Synthesis + Micro-Samples

Most sounds are synthesized from parameters, not played from files. A purr is a filtered oscillator with amplitude modulation. A whimper is a frequency sweep with vibrato. A beep is a sine wave with envelope.

Small sample bank (~100KB compressed) for sounds that are hard to synthesize convincingly:
- Breathing textures
- Pad/thump impacts
- A few tonal fragments for the wisp's harmonic sounds

### Sound Parameter Flow

```
Presence field → Creature layer → sound_pitch, sound_volume, sound_trigger
                                         ↓
                                  Renderer sound engine
                                         ↓
                                  AudioTrack (Android) / AVAudioEngine (iOS)
```

The creature layer decides WHAT sound to make based on internal state. The renderer decides HOW it sounds based on species and the parameter values.

| Sound Trigger | Hound Synthesis | Feline Synthesis | Mech Synthesis | Wisp Synthesis |
|--------------|-----------------|------------------|----------------|----------------|
| Content | Low-freq oscillator + noise (panting) | Filtered noise burst, slow LFO (purr) | Square wave, steady (idle hum) | Sine chord, warm harmonics |
| Alert | Sharp attack envelope (bark) | Short high-freq burst (chirp) | Fast square pulse (chime) | Bright sine pop |
| Distress | Frequency sweep down + vibrato (whine) | Noise burst + resonant filter (hiss) | Sawtooth + noise (alarm) | Dissonant interval, flutter |
| Sleep | Very slow noise modulation (breathing) | Softer version of content purr | Slow square fade in/out (power cycle) | Near-silence, distant tone |

All synthesis runs on the audio thread. No main thread blocking. Latency target: <20ms from trigger to sound.

---

## BLE Implementation

### Android BLE Stack

We use Android's BLE API (requires Kotlin/Java), not raw HCI. This means:

```kotlin
// Advertising (creature broadcasts its presence)
val advertiser = BluetoothLeAdvertiser()
val data = AdvertiseData.Builder()
    .addManufacturerData(COMPANY_ID, creaturePacket) // 31 bytes
    .build()
advertiser.startAdvertising(settings, data, callback)

// Scanning (detecting nearby creatures)
val scanner = BluetoothLeScanner()
scanner.startScan(filters, settings, callback)
// callback fires when another creature is detected
```

### Protocol Flow

```
Phone A                          Phone B
  │                                │
  ├── BLE advertise (31 bytes) ──→ │
  │ ←── BLE advertise (31 bytes) ──┤
  │                                │
  │  [Both detect, both show prompt to user]
  │                                │
  ├── GATT connect ──────────────→ │
  │                                │
  │  Phase 1: Introduction (5-10s)
  │  [Creature animations play on both screens]
  │                                │
  ├── State vector (416 bytes) ──→ │
  │ ←── State vector (416 bytes) ──┤
  │                                │
  │  Phase 2: State Exchange
  │  [Creatures react to each other's state]
  │                                │
  ├── Trace vector (1024 bytes) ─→ │
  │ ←── Trace vector (1024 bytes) ─┤
  │                                │
  │  Phase 3: Knowledge Exchange
  │  [α-mixing happens on both sides]
  │                                │
  │  Phase 4: Play (user-driven duration)
  │  [Periodic state updates, ~1/second]
  │                                │
  │  [Phones move apart or users close]
  │                                │
  │  Phase 5: Parting
  │  [Goodbye animations]
  │                                │
  ├── GATT disconnect ───────────→ │
```

Total data exchanged: ~3KB per meeting. BLE handles this trivially.

### RSSI Filtering

Raw BLE RSSI is noisy. Smooth it:

```
smoothed_rssi = 0.8 × smoothed_rssi + 0.2 × raw_rssi
```

Proximity thresholds:
- **> -50 dBm**: very close (~0.5m). Creatures interact intensely.
- **-50 to -65 dBm**: close (~1-3m). Normal interaction range.
- **-65 to -75 dBm**: medium (~3-5m). Creatures notice each other, can initiate.
- **< -75 dBm**: far. Ignore. Too noisy for meaningful interaction.

---

## Surface File on Mobile

### Layout (64MB Default)

```
Offset          Size        Content
0x00000000      4KB         Header (magic, version, checksum, creature metadata)
0x00001000      4MB         Code regions (emitted AArch64 stubs)
0x00401000      8MB         Holographic trace vectors
0x00C01000      4MB         Schema traces
0x01001000      8MB         Self-model + receipt history
0x01801000      1MB         Relationship data (up to ~50 contacts)
0x01901000      1MB         Creature state (species, maturity, drives, stats)
0x01A01000      38MB        Free space (growth room)
0x04000000      ---         End (64MB)
```

### Growth

When the creature asks for more space:

| Tier | Size | When |
|------|------|------|
| Default | 64MB | Install |
| Tier 1 | 128MB | First ask (adolescent+) |
| Tier 2 | 256MB | Mature creature, heavy learning |
| Tier 3 | 512MB | Elder creature, maximum capacity |

Each tier doubles. User approves via the asking system. The creature never takes space silently.

### Encryption

Surface encryption uses AES-256-GCM with a key derived from:
1. Android Keystore hardware-backed key (device-bound)
2. XOR'd with hash of current holographic trace (creature-bound)
3. Key rotates every dream cycle (rolling)

Only the active working set (~4MB) is decrypted in RAM at any time. Rest stays encrypted on disk. Decryption happens page-by-page as needed.

---

## Build System

### Android

```
project/
├── app/
│   ├── src/main/
│   │   ├── kotlin/           # App shell (~200 lines)
│   │   │   └── PetActivity.kt
│   │   ├── jni/
│   │   │   ├── bridge.c      # JNI bridge (~500 lines)
│   │   │   └── renderer.c    # OpenGL ES renderer
│   │   ├── asm/
│   │   │   ├── engine/       # UHMA engine (AArch64)
│   │   │   └── creature/     # Creature layer (AArch64)
│   │   ├── assets/
│   │   │   ├── shaders/      # GLSL ES shaders
│   │   │   └── sounds/       # Micro-sample bank
│   │   └── AndroidManifest.xml
│   └── build.gradle.kts
├── engine/                    # Shared engine source (cross-platform)
│   ├── *.asm                  # AArch64 assembly files
│   └── Makefile
└── build.gradle.kts
```

### Assembly Compilation

```makefile
# Cross-compile AArch64 assembly with Android NDK
AS = $(NDK)/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android-as
LD = $(NDK)/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android-ld

%.o: %.asm
	$(AS) -o $@ $<

libuhma.so: $(OBJECTS)
	$(LD) -shared -o $@ $^
```

The engine compiles to a shared library (`.so`). Loaded by the JNI bridge. Standard Android NDK workflow.

### Desktop Development (x86-64)

Keep the existing x86-64 build for development and testing:

```
make              # build x86-64 desktop version
make android      # cross-compile AArch64 + package APK
make test         # run engine tests on desktop (fast)
make test-arm     # run engine tests in Android emulator (slow)
```

Develop on desktop. Test on desktop. Only deploy to phone for rendering and BLE testing.

---

## Phase Plan

### Phase 0: Preparation (2 weeks)

**Goal**: Development environment ready, ARM port strategy validated.

- [ ] Android Studio + NDK setup
- [ ] "Hello World" AArch64 assembly app (assembly calls JNI, JNI draws a triangle)
- [ ] Verify self-modifying code works on target device (icache flush sequence)
- [ ] Port one function: `holo_cosim_f64` (the dot product). Verify output matches x86.
- [ ] Benchmark NEON vs SSE for holographic math — confirm ARM is not slower.

**Exit criteria**: A phone displays a spinning triangle driven by assembly code that modified itself at runtime.

### Phase 1: Engine Port (8 weeks)

**Goal**: Full UHMA engine running on Android. No creature. No rendering. Text in, predictions out, verified identical to x86 version.

Week 1-2: Core math
- [ ] Port vsa.asm (holographic operations — the foundation)
- [ ] Port vsa_ops.asm (semantic encoding)
- [ ] Verification: feed same input, compare output vectors bit-for-bit

Week 3-4: Memory and prediction
- [ ] Port surface.asm (mmap works the same on Android/Linux)
- [ ] Port dispatch.asm (prediction engine)
- [ ] Port learn.asm + emit.asm (learning + ARM code generation)
- [ ] Verification: feed text, verify predictions match desktop

Week 5-6: Self-awareness and consolidation
- [ ] Port introspect.asm (self-model)
- [ ] Port observe.asm (self-observation)
- [ ] Port dreams.asm (consolidation)
- [ ] Port receipt.asm (trace system)
- [ ] Port presence.asm (30-dim presence field)
- [ ] Verification: run observe, verify self-awareness score matches desktop for same input

Week 7-8: Integration and support
- [ ] Port remaining: signal.asm, verify.asm, gate.asm, modify.asm, drives.asm, maturity.asm
- [ ] Remove TCP channel code (replaced by JNI direct calls)
- [ ] JNI bridge: text input → engine → status output
- [ ] Test app: text field + status display. Feed text, see predictions. Ugly but functional.

**Exit criteria**: Feed the same corpus to desktop and phone. Both reach same hit ratio, same self-awareness, same presence field values.

### Phase 2: Creature Core (6 weeks)

**Goal**: A creature on screen that reacts to real engine state. One species (Hound). Basic rendering.

Week 1-2: Translation layer
- [ ] Implement presence-to-behavior mapping (CREATURE_SPEC.md tables)
- [ ] CreatureState struct — engine writes, renderer reads
- [ ] Species determination logic (first 100 interactions)
- [ ] Maturity gate checks

Week 3-4: Basic renderer
- [ ] OpenGL ES 3.0 setup
- [ ] Hound body — bezier curves, posture-driven deformation
- [ ] Eye/ear/tail animation from parameters
- [ ] Basic color system (valence → warmth, arousal → saturation)
- [ ] Touch input (tap, hold, stroke → engine events)

Week 5-6: Lifecycle
- [ ] Egg stage rendering (wobble, color hints)
- [ ] Hatch transition animation
- [ ] Infant → child → adolescent visual progression
- [ ] Sleep/dream state (creature lying down, dream mode placeholder)
- [ ] Energy display (subtle, not a health bar)

**Exit criteria**: Feed the creature text. Watch it react. See it learn. See it get confused when predictions fail. See it sleep when tired. See it grow when it earns it. Feel something.

### Phase 3: Full Species + Polish (6 weeks)

**Goal**: All four species, full rendering, sound, dream visualization.

Week 1-2: Remaining species
- [ ] Feline renderer (sharper shapes, subtler expression)
- [ ] Mech renderer (geometric, indicators)
- [ ] Wisp renderer (particle system, no body)
- [ ] Species blend rendering (mostly-hound-with-wisp-moments)

Week 3-4: Sound and dreams
- [ ] Sound synthesis engine (per-species parameter tables)
- [ ] Micro-sample bank integration
- [ ] Dream visualization (abstract generative visuals from dream_pattern)
- [ ] Dream-wake transition animations

Week 5-6: Asking system + environment
- [ ] Species-specific asking behaviors
- [ ] Ask prompts (plain language, tap to respond)
- [ ] Background/environment rendering (time of day, mood-weather)
- [ ] Battery awareness (read level, adjust creature behavior)
- [ ] Background processing via WorkManager (dream while charging)

**Exit criteria**: All four species are alive, expressive, audible, and dreamable. The creature asks for things politely. It handles low battery. It's beautiful.

### Phase 4: Social (6 weeks)

**Goal**: Creatures meet over Bluetooth.

Week 1-2: BLE foundation
- [ ] Advertising packet broadcast
- [ ] Scan + filter for our protocol ID
- [ ] RSSI smoothing + proximity detection
- [ ] User approval flow ("A Feline is nearby. Meet?")

Week 3-4: Meeting protocol
- [ ] GATT connection establishment
- [ ] State vector exchange
- [ ] Trace vector exchange + α-mixing
- [ ] Introduction animations (species × species matrix)
- [ ] Parting animations

Week 5-6: Relationships
- [ ] Relationship storage (per creature ID)
- [ ] Familiarity score tracking
- [ ] Relationship-dependent behavior changes
- [ ] Familiarity decay over time
- [ ] Competition mode (prediction challenge)
- [ ] Group interaction (3+ creatures)

**Exit criteria**: Two phones, two creatures, they meet, they interact, they exchange knowledge, they remember each other next time.

### Phase 5: Protection + Launch (4 weeks)

**Goal**: Protection layers active, app store ready.

Week 1-2: Protection
- [ ] Surface file encryption (AES-256-GCM, device-bound key)
- [ ] Honeypot code regions + decoy data structures
- [ ] Timing-based debug detection (subtle degradation, not crash)
- [ ] Self-awareness as tamper detection (already built — just verify it works)
- [ ] BLE mutual authentication (trace resonance challenge)

Week 3-4: Launch prep
- [ ] Performance optimization pass (60fps target on mid-range devices)
- [ ] Battery drain measurement (target: <3% per hour foreground, <0.5% background)
- [ ] App store assets (screenshots, description, preview video)
- [ ] Privacy policy (three sentences)
- [ ] Beta testing (10-20 users, diverse devices)

**Exit criteria**: App on Google Play Store. Under 1MB. Works offline. Creatures are alive.

### Phase 6: Post-Launch (Ongoing)

- [ ] iOS port (Swift shell, same AArch64 engine + renderer)
- [ ] Creature journal (premium feature)
- [ ] Extended dream visualization (premium feature)
- [ ] Multiple creature slots (premium feature)
- [ ] Community features (creature sharing, events)
- [ ] Elder creature content

---

## Testing Strategy

### Engine Verification (Phase 1)

The desktop x86-64 build is the reference implementation. Every ARM function must produce identical output.

```bash
# Feed identical input to both builds
echo "hello world" | ./uhma-x86 > x86_output.txt
echo "hello world" | ./uhma-arm > arm_output.txt  # via adb
diff x86_output.txt arm_output.txt
```

Test vectors for holographic math:
- Known input vectors → expected dot product
- Known bind inputs → expected bound vector
- Known superposition → expected result
- All verified to floating-point precision (< 1e-10 error)

### Creature Verification (Phase 2-3)

No automated test can tell you if a creature feels alive. This is human testing.

Checklist per species:
- [ ] Feed it text. Does it react visibly?
- [ ] Stop feeding. Does it get bored (novelty drive)?
- [ ] Feed it wrong predictions. Does it look confused?
- [ ] Feed it consistently. Does it get confident?
- [ ] Wait for fatigue. Does it sleep naturally?
- [ ] Watch it dream. Does the dream reflect what it learned?
- [ ] Come back after sleep. Does it wake up happy to see you?
- [ ] Does it feel like a creature, or does it feel like a tech demo?

The last question is the only one that matters.

### BLE Verification (Phase 4)

- [ ] Two devices, same room: meeting initiates within 5 seconds
- [ ] Across a table (~1m): strong signal, full interaction
- [ ] Across a room (~5m): detectable, interaction possible
- [ ] Different rooms: no spurious detection
- [ ] One device walks away mid-meeting: graceful disconnection
- [ ] Meet same creature twice: familiarity increases
- [ ] Meet 10 different creatures in one day: rate limiting works
- [ ] Airplane mode: creature functions normally solo
- [ ] Corrupted BLE packet: creature sneezes, no crash

---

## Device Targets

### Minimum
- Android 8.0+ (API 26)
- AArch64 (ARMv8-A)
- 2GB RAM
- BLE 4.2+
- OpenGL ES 3.0

This covers ~95% of active Android devices worldwide, including budget phones in developing markets.

### Recommended
- Android 12+
- 4GB+ RAM
- BLE 5.0+

### Not Supported (Initially)
- ARMv7 (32-bit) — not worth the port effort, dying market
- x86 Android — tiny market share, mostly emulators
- Chromebooks — Phase 6+ if demand exists

---

## Risk Register

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| ARM self-modifying code has unexpected cache behavior on some devices | Medium | High | Phase 0 validates on 3+ device families (Qualcomm, MediaTek, Samsung) |
| 64MB surface file too large for budget phones | Low | Medium | Offer 32MB mode with more aggressive pruning |
| BLE interaction unreliable in crowded venues | Medium | Low | Graceful degradation built into spec. Partial exchange is fine. |
| OpenGL ES procedural rendering doesn't look good enough | Medium | High | Prototype hound renderer in Phase 0. If shaders can't convey emotion, pivot to minimal vector art. |
| Battery drain exceeds target | Medium | Medium | Aggressive power states already specced. Can throttle further. |
| App store rejects for "AI" claims | Low | High | Don't claim AI. Call it a "cognitive companion." FTC-safe language throughout. |
| Google Play requires 64-bit (already done) | None | None | AArch64 from day one |
