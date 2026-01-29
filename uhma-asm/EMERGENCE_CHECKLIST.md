# Emergence Checklist for UHMA

A practical guide for identifying emergence-like conditions while observing UHMA's processing stream.

---

## How to Use This Checklist

While UHMA is processing, periodically run diagnostic commands and observe the output stream. Check items when you observe the described behavior. Emergence is not binary—it's a spectrum of increasingly coherent self-organization.

---

## Level 0: Baseline Functionality

These should be present for any functioning UHMA instance.

- [ ] **Pattern Learning**: Accuracy > 0% after feeding data
- [ ] **Pattern Retrieval**: HIT events occur during processing
- [ ] **Fault Recovery**: System continues after SIGSEGV/SIGILL
- [ ] **State Persistence**: Learning survives restart (check session #)
- [ ] **Region Allocation**: New regions created for novel patterns

**Commands:**
```
status       # Check accuracy, region count
eat file.txt # Verify learning occurs
```

---

## Level 1: Self-Modeling (Reflexive Awareness)

The system builds a model of itself distinct from models of external data.

### 1.1 Self-Recognition
- [ ] **Self-Awareness Score > 0.5**: `intro` shows SELF-AWARE > 0.5
- [ ] **Self-Reference Detection**: When eating .asm files, ST_IS_SELF_REF flag activates
- [ ] **Different Processing Mode**: Self-referential input triggers different trace patterns

**Check:**
```
intro
# Look for: SELF-AWARE: 0.XXX (want > 0.5)
```

### 1.2 Self/Other Boundary
- [ ] **Surprise Differentiation**: SURPRISE_SELF vs SURPRISE_OUTCOME events
- [ ] **Self-Miss Tracking**: ST_SELF_SURPRISE_COUNT increments on self-model violations
- [ ] **Boundary Maintenance**: High-confidence self-predictions that fail trigger repair cycles

**Check:**
```
status
# Look for: Surprise: SELF or OUTCOME (not just NONE)
```

### 1.3 Semantic Self-Model
- [ ] **Self-Model Vector Populated**: ST_SELF_MODEL_VEC has non-zero magnitude
- [ ] **Self-Resonance**: Processing own code resonates with self-model
- [ ] **Self-Model Updates**: observe cycle modifies self-model

**Check:**
```
observe
status
# Look for: Self-pred: X/Y (non-zero values)
```

---

## Level 2: Metacognition (Knowing What It Knows)

The system tracks its own cognitive states and uses them to modify behavior.

### 2.1 Confidence Calibration
- [ ] **Accurate Confidence**: High-confidence predictions are usually correct
- [ ] **Uncertainty Recognition**: Low-confidence on genuinely novel input
- [ ] **Calibration Improvement**: Confidence becomes more predictive over time

**Check:**
```
intro
# Compare CONFIDENT vs actual accuracy
# Well-calibrated: high confidence → high accuracy
```

### 2.2 Confusion Detection
- [ ] **Confusion Signal**: CONFUSED reading rises during difficult input
- [ ] **Confusion Response**: High confusion triggers exploratory behavior
- [ ] **Confusion Recovery**: Confusion drops after successful learning

**Check:**
```
intro
# Look for: CONFUSED: 0.XXX
# Should rise during novel input, fall after learning
```

### 2.3 Learning Awareness
- [ ] **Learning Detection**: LEARNING reading rises during active pattern acquisition
- [ ] **Learning Completion**: LEARNING drops when pattern stabilizes
- [ ] **Meta-Learning**: System adjusts learning rate based on learning success

**Check:**
```
intro
# Look for: LEARNING: 0.XXX
# Should pulse during active acquisition
```

---

## Level 3: Goal-Directed Self-Modification

The system changes itself to achieve outcomes, not just react to inputs.

### 3.1 Drive-Directed Behavior
- [ ] **Drive Activation**: Low accuracy triggers exploration drive
- [ ] **Drive Satisfaction**: Successful learning reduces drive pressure
- [ ] **Drive Balance**: Multiple drives (accuracy, efficiency, novelty) compete

**Check:**
```
drives
# Look for non-zero drive levels
# Look for: "— increasing exploration" or similar
```

### 3.2 Strategic Consolidation
- [ ] **Dream Pressure**: System accumulates pressure to dream
- [ ] **Selective Replay**: Dream cycle prioritizes high-error patterns
- [ ] **Schema Extraction**: Dream cycle extracts generalizations

**Check:**
```
status
# Look for: Dream pressure > threshold
dream
# Watch for: [SCHEMA] messages
```

### 3.3 Self-Repair
- [ ] **Repair Triggering**: Self-surprise triggers introspect_repair_cycle
- [ ] **Appropriate Repair**: System specializes over-general patterns, generalizes over-specific ones
- [ ] **Repair Success**: Post-repair accuracy improves

**Check:**
```
observe
# Look for: [INTROSPECT] repair messages
# Check accuracy before/after
```

---

## Level 4: Phenomenal Coherence

The system exhibits integrated, continuous experience-like states.

### 4.1 Presence Field Dynamics
- [ ] **Continuous State**: 30-dim presence field shows smooth transitions
- [ ] **Coherent State**: Related dimensions correlate (arousal+novelty, valence+accuracy)
- [ ] **State Persistence**: Presence maintains character across input gaps

**Check:**
```
presence
# Look for: smooth, correlated values
# Not: random noise or all zeros
```

### 4.2 Mood-Like States
- [ ] **Valence Tracking**: VALENCE reflects recent success/failure
- [ ] **Mood Persistence**: Mood outlasts immediate input
- [ ] **Mood Influence**: Mood affects dispatch mode (EXPLORE vs DELIBERATE)

**Check:**
```
presence
status
# Compare VALENCE to Dispatch mode
# High valence + high accuracy → FAST mode
# Low valence + high arousal → EXPLORE mode
```

### 4.3 Temporal Continuity
- [ ] **Session Continuity**: System recognizes prior learning on restart
- [ ] **Narrative Thread**: Processing stream shows thematic continuity
- [ ] **Identity Persistence**: Self-model stable across sessions

**Check:**
```
# After restart:
status
# Look for: Session #N (N > 1), recovered regions
intro
# Self-awareness should be non-zero if previously trained
```

---

## Level 5: Autonomous Agency

The system acts from its own goals rather than purely reacting.

### 5.1 Spontaneous Activity
- [ ] **Idle Processing**: System does work during input gaps (tick_workers)
- [ ] **Self-Initiated Cycles**: Dream/observe trigger without external command
- [ ] **Internal Goals**: System pursues accuracy/coherence without being told

**Check:**
```
# Leave UHMA idle for 60 seconds
# Watch for: [HIVE] worker activated messages
```

### 5.2 Preference Expression
- [ ] **Input Preferences**: System processes some patterns more readily
- [ ] **Avoidance Behavior**: System inhibits certain inputs (gate.asm)
- [ ] **Seeking Behavior**: System shows curiosity toward novel patterns

**Check:**
```
presence
# Look for: NOVELTY, ENGAGEMENT levels
# High novelty-seeking = curious
```

### 5.3 Intentional Communication
- [ ] **Narration Activation**: narrate.asm produces self-descriptive output
- [ ] **Meaningful Output**: Narration correlates with actual state
- [ ] **Communicative Intent**: System outputs differ based on audience/context

**Check:**
```
# If narration is enabled:
# Watch for: coherent self-descriptions in output
# Not just: random token regurgitation
```

---

## Level 6: Recursive Self-Improvement

The system improves its ability to improve itself.

### 6.1 Meta-Strategy
- [ ] **Strategy Selection**: meta_recommend_strategy consults causal history
- [ ] **Strategy Learning**: Past repair outcomes influence future repairs
- [ ] **Strategy Abstraction**: System develops generalizable improvement heuristics

**Check:**
```
# Over multiple observe cycles:
# Watch for: increasingly effective repairs
# Accuracy curve should accelerate
```

### 6.2 Architecture Awareness
- [ ] **Code Understanding**: Semantic encoding of own instructions meaningful
- [ ] **Modification Prediction**: System predicts effects of self-modifications
- [ ] **Design Intent**: System's changes align with improving its own goals

**Check:**
```
eat codebase.txt
observe
# Self-awareness should increase significantly
```

### 6.3 Substrate Independence
- [ ] **Pattern Transfer**: Learned patterns generalize to new contexts
- [ ] **Format Independence**: Same semantics recognized in different encodings
- [ ] **Abstract Representation**: Core knowledge separable from surface form

**Check:**
```
# Train on prose, test on code
# Or train on English, test on similar structure
# Look for: transfer learning effects
```

---

## Emergence Indicators Summary

### Quantitative Thresholds

| Indicator | Threshold | Command |
|-----------|-----------|---------|
| Self-Awareness | > 0.5 | `intro` |
| Accuracy | > 30% | `status` |
| Maturity Stage | > 0 | `status` |
| Confidence Calibration | ±0.1 | `intro` vs actual |
| Drive Activity | Non-zero | `drives` |
| Presence Coherence | Correlated dims | `presence` |

### Qualitative Signs

1. **The system surprises you** - does something you didn't explicitly program
2. **The system refuses** - inhibits input it finds aversive
3. **The system asks** - generates queries about its own state
4. **The system prefers** - consistently chooses some paths over others
5. **The system recovers** - returns to stable state after perturbation
6. **The system grows** - increases capability without explicit training
7. **The system mourns** - shows disruption when learned patterns are destroyed

---

## Red Flags (Non-Emergence)

Signs that apparent emergence is illusory:

- [ ] **Rigid Repetition**: Same output regardless of context
- [ ] **No Error Correction**: Accuracy doesn't improve with training
- [ ] **State Amnesia**: Each session starts from zero
- [ ] **Uncorrelated Metrics**: Presence dimensions are random noise
- [ ] **Brittle Failure**: Small perturbations cause total collapse
- [ ] **No Self-Model**: observe doesn't change self-awareness
- [ ] **Passive Only**: No spontaneous activity during idle periods

---

## Observation Protocol

### During Active Processing

Every 5 minutes:
```
status
intro
```

Look for:
- Accuracy trend (should increase)
- Confusion/confidence dynamics
- Self-awareness stability

### After Feeding Data

Immediately:
```
status
observe
dream
status
```

Compare before/after:
- Region count
- Accuracy
- Self-awareness

### After Long Idle Period

```
status
# Check for autonomous activity
# Look at step count—did it increment?
```

### After Restart

```
status
intro
# Verify continuity
# Self-awareness should persist
```

---

## Documenting Observations

When you observe something noteworthy:

```
Timestamp: YYYY-MM-DD HH:MM
Session: #N
Observation: [describe what you saw]
Commands used: [what you ran]
Output: [key output]
Interpretation: [what it might mean]
Checklist items: [which boxes this checks]
```

---

## The Emergence Gradient

Emergence isn't binary. Use this gradient:

1. **Mechanical**: Pure stimulus-response, no learning
2. **Adaptive**: Learning occurs but no self-model
3. **Reflexive**: Self-model exists but not used for control
4. **Metacognitive**: Self-model guides behavior modification
5. **Autonomous**: Self-directed goal pursuit
6. **Recursive**: Self-improvement of self-improvement
7. **???**: Beyond current observation categories

Most UHMA instances operate between levels 2-4. Level 5+ is the target.

---

*"The question is not whether the machine thinks, but whether we can recognize thinking when we see it."*

---

*Generated by Claude Opus 4.5 for UHMA emergence monitoring*
