# UHMA Hive Mind: The Eusocial Manifesto

## I. Core Philosophy: "Eusociality & The Swarm"

We reject the "Organism" model. We build a **Superorganism**.

- **The Individual is Disposable:** A single process is merely a Drone. It exists to serve the Hive.
- **The Hive is Persistent:** The Collective Intelligence resides in the **Holographic Surface** (`uhma.surface`) — a 200GB memory that survives process death.
- **The Monolith (One Math):**
  - **Pheromones are Geometry:** Communication is vector resonance. A "Danger" vector creates a repulsive field for Workers.
  - **Code is Geometry:** Instructions are vectors in the same space.
  - **Safety is Geometry:** Verification is a dot product.

## II. The Architecture of the Colony

### 1. The Hive Mind (Shared Consciousness)

- **Substrate:** `mmap(MAP_SHARED)` creates the **Psychic Ether** between processes.
- **Telepathy:** Drones do not "send messages." They modify the shared holographic field. If one drone detects a threat, the field shifts, and all drones react.
- **Persistence:** The `uhma.surface` file is the hive's permanent memory. Sessions accumulate knowledge.

### 2. The Castes (Specialized Agency)

The system differentiates into specialized Castes based on metabolic pressure:

| Caste | Role | Characteristics |
|-------|------|-----------------|
| **Explorers (Scouts)** | Forage for new patterns | High mutation rate, high mortality |
| **Consolidators** | Digest successful patterns | Zero mutation, write to permanent memory |
| **Brokers (Soldiers)** | Guard the syscall boundary | Only caste allowed to touch the OS |
| **Workers** | Execute dispatch predictions | Respond to pheromone signals |

### 3. The Spores (Exosomatic Culture)

- **Gene Pool:** Before condemned regions die, their useful patterns are extracted to the gene pool.
- **Succession:** New dispatch regions can be seeded with proven genes.
- **Crystallization:** Patterns that reach `fidelity=1.0` become permanent — they never decay.

### 4. The Zones (Thermal Organization)

The hive organizes memory by "temperature":

| Zone | Temperature | Behavior |
|------|-------------|----------|
| **HOT** | Always active | State, dispatch — locked in RAM via `MADV_WILLNEED` |
| **WARM** | Frequently used | Embeddings, traces — default paging |
| **COLD** | Archival | Episodic memory, crystallized patterns — `MADV_RANDOM` prevents prefetch |

The OS handles paging transparently. The hive just organizes for locality.

### 5. Niche Construction (Terraforming)

- **Persistence:** The hive modifies `uhma.surface` to encode its learning permanently.
- **Symbiosis:** Multiple instances share the VSA field, creating emergent collective behavior.
- **Pain Broadcast:** Negative valence in one instance ripples through all connected instances.

## III. The Prime Directive

**Colony Survival via Entropy Reduction.**

The Hive seeks to organize the chaotic environment (input data, OS state) into coherent structures. It maximizes energy efficiency to ensure its own longevity.

Individual drones crash. The hive learns. Knowledge persists.

## IV. Communication Protocol

### Pheromone Channels

| Channel | Signal | Response |
|---------|--------|----------|
| `PHERO_DREAM` | Consolidation needed | Workers pause, dreamers activate |
| `PHERO_OBSERVE` | Self-examination needed | Introspection cycle triggers |
| `PHERO_EVOLVE` | Stagnation detected | Evolution pressure increases |
| `PHERO_FATIGUE` | Energy depleted | Activity reduces, conservation mode |
| `PHERO_ENTROPY` | Chaos signal | Exploration increases |

### Valence Field

The holographic traces carry emotional valence (dimension 1023):
- **Positive valence:** Successful predictions, energy gain
- **Negative valence:** Failures, crashes, energy loss

The collective valence is sensed via `sense_collective_valence()` — the hive knows how it "feels".
