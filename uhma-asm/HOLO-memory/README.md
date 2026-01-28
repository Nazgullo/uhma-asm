# HOLO-memory: Holographic Memory Extension for Claude Code

A semantic memory system that gives Claude Code persistent, cross-session memory using holographic vector encoding and sentence transformers.

## What It Does

When Claude Code reads files, searches code, or edits, the hook system automatically injects relevant context from past sessions:
- Previous findings and insights
- Failed approaches (to avoid repeating mistakes)
- Successful patterns (to reuse what worked)
- Code locations and modifications
- Current cognitive state (confidence, confusion levels)

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Claude Code Session                       │
├─────────────────────────────────────────────────────────────┤
│  PreToolUse Hook (hook.py)                                  │
│    ↓                                                        │
│  Query holographic memory for relevant context              │
│    ↓                                                        │
│  Inject <holo-memory> block into tool context               │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│              Holographic Memory (holo_memory.py)            │
├─────────────────────────────────────────────────────────────┤
│  • Sentence-transformer embeddings (all-MiniLM-L6-v2)       │
│  • Semantic similarity search via cosine distance           │
│  • Category-based organization (finding, failed, success)   │
│  • Outcome tracking (what worked, what didn't)              │
│  • Cognitive state monitoring (confidence, confusion)       │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                 Persistent Storage                          │
├─────────────────────────────────────────────────────────────┤
│  memory/holo_entries.json  - Memory entries                 │
│  memory/holo_traces.npz    - Vector embeddings              │
│  memory/holo_state.json    - Cognitive state                │
│  memory/holo_surface.dat   - Holographic surface            │
└─────────────────────────────────────────────────────────────┘
```

## Components

### 1. holo_memory.py - Core Memory System

The holographic memory implementation using Vector Symbolic Architecture (VSA) principles:

```python
# Key classes and functions:

class HolographicMemory:
    """Main memory system with semantic search"""

    def add(category, content, context=None, source=None):
        """Add memory entry with embedding"""

    def query(query_text, limit=10, category=None):
        """Semantic similarity search"""

    def record_outcome(entry_id, worked: bool):
        """Track if an approach worked or failed"""

    def get_cognitive_state():
        """Returns confidence, confusion, hit ratio"""

    def get_failed_approaches(context=None):
        """Get approaches that failed (to avoid)"""

    def get_successful_approaches(context=None):
        """Get approaches that worked (to reuse)"""
```

**Categories:**
- `finding` - Discovered facts about the codebase
- `failed` - Approaches that didn't work
- `success` - Approaches that worked
- `insight` - Deeper understanding/patterns
- `warning` - Things to avoid
- `todo` - Pending items
- `question` - Unresolved questions
- `location` - File/code locations
- `code_high/mid/low` - Code snippets by importance

### 2. hook.py - Context Injection Hooks

Pre-tool hooks that fire before Read, Edit, Write, Grep, Glob operations:

```python
def handle_pre_tool_use(tool_name, tool_input):
    """
    Injects relevant holographic memory context before tool use.

    Returns context block like:
    <holo-memory format='ISON'>
    table.memories
    id category content outcome resonance
    abc123 finding "dispatch.asm handles token routing" true 0.85

    table.cognitive_state
    confidence confusion entries hit_ratio
    0.75 0.25 428 0.45
    </holo-memory>
    """
```

**ISON Format:**
A compact table format for injecting structured data:
```
table.name
column1 column2 column3
value1 value2 value3
```

### 3. session_capture.py - Session Recording

Captures conversation context for memory persistence:
- User requests and questions
- Tool usage patterns
- Modifications made
- Outcomes achieved

### 4. session_end.py - Session Persistence

Saves session state on exit:
- Consolidates findings into memory
- Updates cognitive state
- Triggers memory decay (older memories fade)

## Installation

1. Copy files to your project:
```bash
mkdir -p tools/rag/memory
cp holo_memory.py hook.py session_capture.py session_end.py tools/rag/
```

2. Install dependencies:
```bash
pip install sentence-transformers numpy
```

3. Configure Claude Code hooks in `.claude/settings.json`:
```json
{
  "hooks": {
    "preToolUse": [
      {
        "matcher": "Read|Edit|Write|Grep|Glob",
        "command": ["python3", "tools/rag/hook.py"],
        "timeout": 5000
      }
    ],
    "postToolUse": [
      {
        "matcher": ".*",
        "command": ["python3", "tools/rag/session_capture.py"],
        "timeout": 3000
      }
    ],
    "stop": [
      {
        "command": ["python3", "tools/rag/session_end.py"],
        "timeout": 10000
      }
    ]
  }
}
```

4. Restart Claude Code

## How It Works

### Memory Encoding

Each memory entry is encoded as a high-dimensional vector using sentence-transformers:

```python
from sentence_transformers import SentenceTransformer
model = SentenceTransformer('all-MiniLM-L6-v2')

# Content becomes a 384-dim vector
embedding = model.encode("dispatch.asm handles token prediction")
```

### Semantic Search

Queries find relevant memories via cosine similarity:

```python
def query(self, query_text, limit=10):
    query_vec = self.model.encode(query_text)

    # Compare against all stored vectors
    similarities = cosine_similarity(query_vec, self.trace_vectors)

    # Return top matches
    top_indices = np.argsort(similarities)[-limit:]
    return [self.entries[i] for i in top_indices]
```

### Cognitive State

The system tracks its own confidence and confusion:

```python
def get_cognitive_state(self):
    recent = self.entries[-100:]  # Last 100 entries

    hits = sum(1 for e in recent if e.outcome == True)
    misses = sum(1 for e in recent if e.outcome == False)

    return {
        "confidence": hits / (hits + misses + 1),
        "confusion": misses / (hits + misses + 1),
        "hit_ratio": self.total_hits / self.total_queries
    }
```

### Outcome Learning

When Claude discovers something worked or failed:

```python
# Mark an approach as failed
memory.record_outcome(entry_id, worked=False)

# Mark an approach as successful
memory.record_outcome(entry_id, worked=True)
```

This feedback improves future recommendations.

## Context Injection Example

When Claude reads `dispatch.asm`, the hook queries memory and injects:

```xml
<holo-memory format='ISON'>
table.memories
id category content outcome resonance
a1b2c3 finding "dispatch.asm:2680 compute_struct_ctx builds 8-token context" true 0.92
d4e5f6 warning "rcx is caller-saved - dont use in loops" null 0.87
g7h8i9 failed "Tried XORing somatic state into context hash - broke patterns" false 0.81

table.trace_resonance
category resonance
code_high 0.85
warning 0.72

table.cognitive_state
confidence confusion entries
0.65 0.35 428
</holo-memory>
```

Claude sees relevant past findings, warnings, and failed approaches before making changes.

## Cognitive Warnings

When confusion is high, warnings are injected:

```xml
<cognitive-warnings>
! HIGH CONFUSION - step back and reassess
! GOING IN CIRCLES - try different approach
</cognitive-warnings>
```

These help Claude recognize when it's stuck.

## Memory Decay

Older memories naturally fade (exponential decay):

```python
def decay_memories(self, factor=0.995):
    for entry in self.entries:
        entry.weight *= factor
```

Recent memories have more influence than old ones.

## Files Included

```
HOLO-memory/
├── README.md              # This documentation
├── holo_memory.py         # Core memory system
├── hook.py                # Pre-tool context injection
├── session_capture.py     # Session recording
├── session_end.py         # Session persistence
├── settings.json.example  # Example Claude Code config
└── memory/                # Storage directory
    └── .gitkeep
```

## Usage Tips

1. **Bootstrap**: On first run, feed the memory system with codebase info:
   ```python
   from holo_memory import HolographicMemory
   mem = HolographicMemory()
   mem.add("finding", "dispatch.asm handles token prediction", source="manual")
   ```

2. **Record outcomes**: When something works or fails, record it:
   ```python
   mem.record_outcome(entry_id, worked=True)  # or False
   ```

3. **Query before acting**: Check what's known:
   ```python
   relevant = mem.query("stack alignment x86")
   ```

4. **Monitor cognitive state**: Check if confused:
   ```python
   state = mem.get_cognitive_state()
   if state["confusion"] > 0.7:
       print("Warning: High confusion detected")
   ```

## Integration with UHMA

This holographic memory system is inspired by UHMA's VSA-based memory but runs in Python for Claude Code integration. UHMA uses:
- 1024-dim (now 8192-dim) f64 vectors in assembly
- Hardware-accelerated binding/superposition
- Self-modifying code regions

The Python version uses:
- 384-dim sentence-transformer embeddings
- NumPy for vector operations
- JSON persistence

Both share the holographic principle: similar concepts have similar vectors, enabling semantic search via dot product.

## License

MIT - Use freely, modify as needed.
