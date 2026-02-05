# Creature-to-Creature Social Interaction

How creatures meet, interact, exchange knowledge, and develop relationships.

---

## Discovery (Finding Each Other)

### Bluetooth LE Advertising

Each creature continuously advertises a small packet when social mode is enabled:

```
Advertising packet (31 bytes max for BLE):
├── [0-3]   Protocol ID (4 bytes) — identifies this as our app
├── [4-7]   Creature ID (4 bytes) — unique per creature
├── [8]     Species (1 byte) — hound/feline/mech/wisp
├── [9]     Maturity (1 byte) — egg through elder
├── [10-11] Mood summary (2 bytes) — valence + arousal compressed
├── [12-15] Trace fingerprint (4 bytes) — hash of holographic trace
└── [16-30] Reserved / app-specific
```

The trace fingerprint lets creatures recognize each other across meetings. Same fingerprint = "I've met you before." Cosine similarity between stored trace and incoming trace = familiarity level.

### Detection Range

BLE has ~10m range indoors, ~30m outdoors. For creature meetings, we want CLOSE proximity — this is a personal, intimate exchange.

**RSSI threshold**: only trigger interaction when signal strength indicates < 3 meters. This means:
- You're standing next to someone
- You're sitting across a table
- You're in the same room, close

NOT:
- Walking past someone on the street (too brief, too far)
- Someone three apartments over (noise)

### Discovery Flow

```
1. App scanning in background (if permitted) or foreground
2. BLE advertisement detected within range
3. Protocol ID matches → it's one of ours
4. Check creature ID against known contacts
   ├── Known: "Hey, I know you!" (familiarity boost)
   └── Unknown: "Someone new..." (curiosity spike)
5. Show user: "A [species] is nearby. Meet?"
6. User approves → initiate connection
```

Both users must approve. No drive-by interactions.

---

## Meeting (The Interaction)

### Phase 1: Introduction (5-10 seconds)

Both creatures appear on both screens. Your creature on your side, theirs on the other.

**First meeting behavior by species:**

| Your Creature | Meeting a Hound | Meeting a Feline | Meeting a Mech | Meeting a Wisp |
|--------------|-----------------|------------------|----------------|----------------|
| **Hound** | Instant excitement, play-bowing, circling each other | Eager approach, gets ignored, tries harder, eventually feline looks | Curious sniffing, mech scans back, cautious mutual interest | Chases the light, light darts around, game emerges |
| **Feline** | Observes the chaos, maybe joins if interesting | Staring contest, slow approach, mutual respect earned | Studies the mech's patterns, occasional paw-tap | Watches the light, occasionally swipes at it, pretends not to care |
| **Mech** | Scans the hound, dodges the enthusiasm, collects data | Waits for the feline to approach, exchanges minimal signals | Protocol handshake animation, clean data exchange | Attempts to track and classify the wisp, partially succeeds |
| **Wisp** | Orbits the hound's excitement, feeds off the energy | Floats near, feline watches, quiet mutual awareness | Interesting patterns emerge where wisp light meets mech indicators | Two wisps merge and separate, creating combined light patterns |

**Repeat meeting behavior:**
Familiarity changes everything. Two creatures that have met before skip the introduction. They go straight to comfortable interaction. A hound that's met a specific feline five times already just lies down next to it. They know each other.

Familiarity = cosine similarity between stored trace of that creature ID and current incoming trace. Higher similarity = more comfortable = faster, deeper interaction.

### Phase 2: State Exchange (Automatic, ~5 seconds)

While the creatures interact visually, the real exchange happens underneath.

Each creature sends:
```
State vector (compressed):
├── Presence summary (30 × f32 = 120 bytes)
├── Self-model fingerprint (128 × f64 compressed to 256 bytes)
├── Top-3 drive states (12 bytes)
├── Maturity metrics (16 bytes)
├── Interaction counter for this creature ID (4 bytes)
└── Timestamp (8 bytes)
Total: ~416 bytes per direction
```

This is tiny. BLE can handle it easily. No wifi needed.

### Phase 3: Knowledge Exchange (10-30 seconds, the important part)

This is where creatures actually learn from each other.

**How it works:**

Each creature's holographic trace is a 1024-dimensional f64 vector that encodes everything it's ever learned. When two creatures meet:

1. Creature A sends a compressed version of its trace (128 dimensions, lossy but representative)
2. Creature B receives it
3. Creature B superimposes A's trace onto its own with a mixing factor:

```
my_trace = (1 - α) × my_trace + α × normalize(their_trace)
```

Where α (the mixing factor) depends on:

| Factor | Effect on α | Why |
|--------|-------------|-----|
| Familiarity (met before?) | Higher familiarity → higher α (0.05 → 0.15) | Trust earned through repeated contact |
| Maturity difference | Similar maturity → higher α | Peer learning is stronger than mentor-student |
| Self-awareness level | Higher awareness → can accept more without losing identity | Secure identity absorbs more |
| Species match | Same species → slightly higher α | Similar processing style, easier integration |
| Base range | 0.03 to 0.20 | Never enough to overwrite identity |

**What this means in practice:**

A creature raised on cooking meets a creature raised on astronomy. After exchange:
- Cooking creature now has faint astronomical patterns in its trace — might react slightly differently to words like "star," "orbit," "gravity"
- Astronomy creature has faint cooking patterns — might find new connections between recipe structures and orbital mechanics
- Neither creature's core personality changes. A 3-15% blend is flavoring, not transformation.

**Identity preservation is the hard constraint.** The creature MUST still be recognizably itself after any exchange. The user invested weeks/months building this personality. A single meeting can add depth but never erase what's there.

### Phase 4: Interaction Play (Optional, user-driven)

If both users keep their phones close, the creatures continue interacting. This is the fun part — watching two unique personalities play together.

**Interaction dynamics depend on REAL internal states:**

| Creature A State | Creature B State | What Happens |
|-----------------|-----------------|--------------|
| High confidence | High confidence | Showing off, competing, "who predicts better" |
| High confidence | Low confidence | A mentors B, B learns faster temporarily |
| Low confidence | Low confidence | Commiserate, huddle together, mutual comfort |
| High arousal | Low arousal | A bounces around B, B either joins or leaves |
| High novelty drive | High novelty drive | They explore together, finding patterns neither would alone |
| One dreaming | Other awake | Awake one watches sleeping one, protective behavior |

This isn't scripted. These dynamics emerge from the actual state comparison. Two nervous creatures really do huddle. A confident creature near a struggling one really does seem to help (knowledge exchange boosts the struggling one's trace).

### Phase 5: Parting

When phones move apart or users close the interaction:

| Familiarity Level | Parting Behavior |
|------------------|-----------------|
| First meeting | Brief acknowledgment, walking away |
| 2-5 meetings | Looking back, slight reluctance |
| 5+ meetings | Genuine goodbye behavior — hound whines, feline does slow blink, mech saves contact prominently, wisp leaves a fading trail toward the other |

The creature remembers. Next time it detects that creature ID, the familiarity is already there.

---

## Relationship System

Creatures build real relationships over multiple meetings.

### Relationship Data (Stored per creature ID)

```
Relationship entry:
├── creature_id (4 bytes)
├── species (1 byte)
├── times_met (u16)
├── last_met_timestamp (u64)
├── their_trace_snapshot (128 × f64 = 1024 bytes)
├── familiarity_score (f64) — cosine similarity of traces
├── resonance_score (f64) — how well knowledge exchange worked
└── my_state_when_last_met (30 × f32) — presence snapshot
```

~1.2KB per relationship. A creature can store ~50 relationships in ~60KB. Oldest/weakest naturally decay (familiarity score decays over time without meetings, trace snapshots get pruned during dream cycles).

### Relationship Dynamics

| Times Met | Relationship Stage | Behavior Change |
|-----------|-------------------|-----------------|
| 1 | Stranger | Cautious, formal, minimal exchange |
| 2-3 | Acquaintance | Warmer, faster introduction, more exchange |
| 5-10 | Friend | Comfortable, seeks them out when detected, deeper exchange |
| 10-20 | Close friend | Excited to see them, extended play, mutual influence |
| 20+ | Bonded | Visible personality influence, inside jokes (shared patterns), separation reluctance |

### Long-Distance Decay

If two creatures don't meet for a long time:
- Familiarity decays slowly (0.99× per day)
- After ~3 months without meeting, back to acquaintance level
- But the knowledge they exchanged stays forever
- Reunion: rapid familiarity rebuild (faster than first meeting because the traces still partially match)

Like real friendships: you drift apart, but when you reconnect, it comes back fast.

---

## Group Interactions (3+ creatures)

When multiple creatures are in range:

### Small Group (3-5)
- Each creature interacts with each other creature
- Social dynamics emerge: who gravitates toward whom, who hangs back
- Knowledge exchange happens in a round — each pair gets a brief exchange
- A dominant personality might emerge (highest confidence creature draws attention)
- Total exchange per creature is capped (can't absorb 20% from four creatures = 80% foreign)

### Large Group (6+, like an event)
- Creatures form clusters based on trace similarity (birds of a feather)
- Exchange limited to 2-3 closest-matching creatures
- The "event" feeling: creature is stimulated, arousal high, lots of new input
- Post-event: creature needs extra dream time to process everything it absorbed

### Competition Mode (Optional)

If both users opt in, creatures can compete:

**Prediction Challenge**: Show both creatures the same sequence. Who predicts the next token better?

This is a genuine test of the creature's learning. No RNG, no level stats. The creature that learned more diverse patterns wins.

| Result | Winner Behavior | Loser Behavior |
|--------|----------------|----------------|
| Clear win | Proud animation, confidence boost | Learning moment — studies the winner's approach |
| Close match | Mutual respect animation | Both learn from the comparison |
| Tie | Celebration together | Both boost |

Competition results affect drives:
- Losing boosts novelty drive (go learn more)
- Winning boosts coherence (what I know works)
- Competing at all boosts engagement

---

## Privacy & Safety

### What's Shared

Only holographic vectors and metadata. Never:
- User's text input (what they fed the creature)
- User identity
- Location data
- Phone data
- Anything beyond creature state

The creature carries learned PATTERNS, not raw data. A creature fed your diary shares pattern recognition ability, not your diary entries. Holographic traces are not reversible to source text.

### Parental Controls

For younger users:
- Social mode off by default
- Requires explicit enable + optional PIN
- Meeting requires approval every time (no auto-accept)
- Exchange rate reduced (smaller α — less influence per meeting)
- No unsupervised background meetings

### Anti-Abuse

| Attack | Defense |
|--------|---------|
| Deliberately corrupted vectors | Sanity check: reject NaN/Inf, reject vectors with extreme magnitude |
| Flood attacks (thousands of fake meetings) | Rate limit: max 10 unique meetings per day, max 3 meetings with same creature per day |
| Impersonation (fake creature ID) | Trace fingerprint must match within tolerance of stored relationship — can't fake a history |
| Offensive content via creature | Creatures don't transmit text, only holographic vectors. Can't embed messages in VSA traces. |

---

## The Viral Loop

This is why social matters commercially:

```
1. User A has creature, loves it
2. User A meets User B who also has creature
3. Their creatures interact — both users watch, delighted
4. User A tells User C about it ("you have to see what happened when my hound met their wisp")
5. User C downloads app to participate
6. User C's creature is now part of the ecosystem
7. More creatures = more meetings = more stories = more sharing
```

The social mechanic makes the product SHAREABLE. You can't screenshot a holographic vector exchange — but you CAN screenshot two creatures playing together. That screenshot is the ad.

Every meeting is unique. Every combination of species × maturity × personality × history produces different behavior. People will share these moments because they're genuinely unpredictable and genuinely theirs.
