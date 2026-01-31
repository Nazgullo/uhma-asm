#!/usr/bin/env python3
"""
Holographic Memory System for Claude Code
==========================================

Replicates UHMA's VSA architecture for temporal continuity,
memory persistence, and common sense cognitive layer.

Architecture (from UHMA):
  - 1024-dim f64 vectors (orthogonal bases)
  - bind/unbind/superpose/cosine operations
  - 8-dim receipt trace (event, ctx, content, outcome, source, aux, session, time)
  - Category traces (findings, failures, insights, etc.)
  - Forced context injection via hooks

Key insight: orthogonal random vectors allow superposition without interference.
Query by unbinding - only matching patterns resonate.

Usage:
    from holo_memory import HoloMemory
    mem = HoloMemory()

    # Store
    mem.add('finding', 'rcx is caller-saved', context='register debugging')
    mem.add('failed', 'tried XORing somatic with context', context='dispatch fix')

    # Query by context resonance
    relevant = mem.query('debugging register clobbering')

    # Log outcomes
    mem.outcome(entry_id, worked=True)

    # Get state
    state = mem.get_state()  # confused? repeating? progressing?

    # Export for injection (ISON format)
    ison = mem.to_ison(limit=10)
"""

import json
import hashlib
import time
import numpy as np
from pathlib import Path
from typing import Optional, List, Dict, Any, Tuple
from datetime import datetime

# Lazy-load embeddings: only import when actually needed for queries
# This avoids 800MB+ memory hit on every hook call
HAVE_EMBEDDINGS = False
_SentenceTransformer = None  # Will be set on first use

def _get_sentence_transformer():
    """Lazy-load SentenceTransformer only when needed."""
    global HAVE_EMBEDDINGS, _SentenceTransformer
    if _SentenceTransformer is not None:
        return _SentenceTransformer
    try:
        from sentence_transformers import SentenceTransformer
        _SentenceTransformer = SentenceTransformer
        HAVE_EMBEDDINGS = True
        return _SentenceTransformer
    except ImportError:
        HAVE_EMBEDDINGS = False
        return None

# ============================================================================
# Configuration
# ============================================================================

HOLO_DIM = 1024  # Vector dimensionality (same as UHMA)
HOLO_DTYPE = np.float64  # 64-bit precision (use np.longdouble for 128-bit)
MEMORY_DIR = Path(__file__).parent / 'memory'
MEMORY_DIR.mkdir(exist_ok=True)

HOLO_ENTRIES_FILE = MEMORY_DIR / 'holo_entries.json'
HOLO_TRACES_FILE = MEMORY_DIR / 'holo_traces.npz'
HOLO_STATE_FILE = MEMORY_DIR / 'holo_state.json'
HOLO_SURFACE_FILE = MEMORY_DIR / 'holo_surface.dat'

# Surface configuration (memory-mapped like UHMA)
# 6GB = 6 * 1024^3 bytes = 6442450944 bytes
# At f64 (8 bytes), that's 805,306,368 floats
# At 1024-dim vectors, that's 786,432 vectors
SURFACE_SIZE_GB = 6
SURFACE_SIZE_BYTES = SURFACE_SIZE_GB * 1024 * 1024 * 1024
SURFACE_VECTOR_COUNT = SURFACE_SIZE_BYTES // (HOLO_DIM * np.dtype(HOLO_DTYPE).itemsize)

# Categories with decay rates (how fast they fade)
CATEGORIES = {
    'finding': 0.95,    # Confirmed facts - slow decay
    'failed': 0.90,     # What didn't work - medium decay
    'success': 0.95,    # What worked - slow decay
    'insight': 0.95,    # Aha moments - slow decay
    'question': 0.80,   # Open questions - fast decay (resolve or forget)
    'location': 0.98,   # Code locations - very slow decay
    'todo': 0.85,       # Tasks - medium-fast decay
    'warning': 0.92,    # Gotchas/warnings - slow-medium decay
    'context': 0.70,    # Session context - fast decay
    'session': 0.85,    # Session summaries - medium decay
    'request': 0.80,    # User requests - medium-fast decay
    'code_high': 0.98,  # File-level code info - very slow decay
    'code_mid': 0.96,   # Function-level code info - slow decay
    'code_low': 0.92,   # Implementation details - medium decay
}

# Event types for receipt trace (from UHMA)
EVENT_TYPES = {
    'ADD': 0,
    'QUERY': 1,
    'HIT': 2,      # Query found relevant memory
    'MISS': 3,     # Query found nothing
    'SUCCESS': 4,  # Outcome: it worked
    'FAILURE': 5,  # Outcome: it failed
    'INJECT': 6,   # Forced context injection
    'DECAY': 7,    # Trace decay applied
}

# ============================================================================
# VSA Operations (Holographic)
# ============================================================================

def random_vec(seed: int = None, dim: int = HOLO_DIM) -> np.ndarray:
    """Generate random unit vector. Same seed = same vector (orthogonal bases)."""
    if seed is not None:
        rng = np.random.default_rng(seed)
    else:
        rng = np.random.default_rng()
    vec = rng.standard_normal(dim).astype(HOLO_DTYPE)
    return vec / np.linalg.norm(vec)


def bind(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    """
    Bind two vectors (element-wise multiply + normalize).
    Result is orthogonal to both inputs.
    Self-inverse: bind(bind(a,b), b) ≈ a
    """
    result = a * b
    norm = np.linalg.norm(result)
    if norm > 1e-10:
        return result / norm
    return result


def unbind(a: np.ndarray, trace: np.ndarray) -> np.ndarray:
    """Unbind = bind (self-inverse in HRR-style VSA)."""
    return bind(a, trace)


def superpose(trace: np.ndarray, vec: np.ndarray, weight: float = 1.0) -> np.ndarray:
    """Add vector to trace (superposition). Normalize to prevent explosion."""
    result = trace + weight * vec
    norm = np.linalg.norm(result)
    if norm > 1e-10:
        return result / norm
    return result


def cosine(a: np.ndarray, b: np.ndarray) -> float:
    """Cosine similarity (normalized dot product)."""
    norm_a = np.linalg.norm(a)
    norm_b = np.linalg.norm(b)
    if norm_a < 1e-10 or norm_b < 1e-10:
        return 0.0
    return float(np.dot(a, b) / (norm_a * norm_b))


def magnitude(vec: np.ndarray) -> float:
    """Vector magnitude."""
    return float(np.linalg.norm(vec))


# ============================================================================
# Semantic Encoding
# ============================================================================

class SemanticEncoder:
    """Encode text to holographic vectors using embeddings + VSA."""

    def __init__(self):
        self.model = None
        self.projection = None  # Project from embedding dim to HOLO_DIM
        self._init_attempted = False

    def _lazy_init(self):
        """Load embedding model on first use (not at import time)."""
        if self._init_attempted:
            return
        self._init_attempted = True

        SentenceTransformer = _get_sentence_transformer()
        if SentenceTransformer is None:
            return

        try:
            # Use a small, fast model
            self.model = SentenceTransformer('all-MiniLM-L6-v2')
            embed_dim = self.model.get_sentence_embedding_dimension()
            # Create stable projection matrix (seeded for reproducibility)
            rng = np.random.default_rng(42)
            self.projection = rng.standard_normal((embed_dim, HOLO_DIM))
            # Normalize columns
            for i in range(HOLO_DIM):
                self.projection[:, i] /= np.linalg.norm(self.projection[:, i])
        except Exception as e:
            print(f"[holo] Embedding init failed: {e}")
            self.model = None

    def encode(self, text: str) -> np.ndarray:
        """Encode text to 1024-dim holographic vector."""
        # Lazy-load embeddings on first encode call
        if not self._init_attempted:
            self._lazy_init()

        if self.model is not None:
            # Get embedding and project to HOLO_DIM
            embedding = self.model.encode(text, convert_to_numpy=True)
            projected = embedding @ self.projection
            # Normalize
            norm = np.linalg.norm(projected)
            if norm > 1e-10:
                return projected / norm
            return projected
        else:
            # Fallback: hash-based encoding (deterministic but less semantic)
            return self._hash_encode(text)

    def _hash_encode(self, text: str) -> np.ndarray:
        """Fallback encoding using hash (when no embeddings available)."""
        # Use multiple hash seeds to build vector
        vec = np.zeros(HOLO_DIM)
        words = text.lower().split()
        for i, word in enumerate(words):
            seed = int(hashlib.md5(word.encode()).hexdigest()[:8], 16)
            word_vec = random_vec(seed)
            # Weight by position (earlier words matter more)
            weight = 1.0 / (1 + i * 0.1)
            vec += weight * word_vec
        norm = np.linalg.norm(vec)
        if norm > 1e-10:
            return vec / norm
        return random_vec()  # Fallback to random if empty


# ============================================================================
# Memory Entry
# ============================================================================

class HoloEntry:
    """A single holographic memory entry."""

    def __init__(self,
                 category: str,
                 content: str,
                 context: str = None,
                 source: str = None,
                 tags: List[str] = None,
                 metadata: Dict = None):

        self.id = self._generate_id(content)
        self.category = category
        self.content = content
        self.context = context or ""
        self.source = source or ""  # file:line or tool name
        self.tags = tags or []
        self.metadata = metadata or {}
        self.created_at = datetime.now().isoformat()
        self.session_id = self._get_session_id()

        # Outcome tracking
        self.outcome = None  # True=worked, False=failed, None=unknown
        self.outcome_count = 0  # How many times outcome was recorded
        self.resonance_count = 0  # How many times this was retrieved

        # Vectors (populated by HoloMemory)
        self.content_vec = None
        self.context_vec = None
        self.bound_vec = None  # bind(context, content)

    def _generate_id(self, content: str) -> str:
        h = hashlib.md5(f"{content}{time.time()}".encode()).hexdigest()[:12]
        return h

    def _get_session_id(self) -> str:
        return datetime.now().strftime("%Y%m%d")

    def to_dict(self) -> Dict:
        return {
            'id': self.id,
            'category': self.category,
            'content': self.content,
            'context': self.context,
            'source': self.source,
            'tags': self.tags,
            'metadata': self.metadata,
            'created_at': self.created_at,
            'session_id': self.session_id,
            'outcome': self.outcome,
            'outcome_count': self.outcome_count,
            'resonance_count': self.resonance_count,
        }

    @classmethod
    def from_dict(cls, data: Dict) -> 'HoloEntry':
        entry = cls(
            category=data['category'],
            content=data['content'],
            context=data.get('context', ''),
            source=data.get('source', ''),
            tags=data.get('tags', []),
            metadata=data.get('metadata', {})
        )
        entry.id = data['id']
        entry.created_at = data.get('created_at', entry.created_at)
        entry.session_id = data.get('session_id', entry.session_id)
        entry.outcome = data.get('outcome')
        entry.outcome_count = data.get('outcome_count', 0)
        entry.resonance_count = data.get('resonance_count', 0)
        return entry


# ============================================================================
# Cognitive State Tracking
# ============================================================================

class CognitiveState:
    """
    Track Claude's cognitive state (subset of UHMA's 30-dim presence).
    Used to detect: going in circles, confusion, progress, etc.
    """

    def __init__(self):
        # Core state dimensions
        self.confidence = 0.5      # How sure am I? (from outcome history)
        self.confusion = 0.0       # Am I lost? (query misses, repeated questions)
        self.progress = 0.0        # Moving forward? (new findings vs repetition)
        self.repetition = 0.0      # Going in circles? (similar queries)
        self.fatigue = 0.0         # Context pressure (tokens used)

        # Tracking
        self.recent_queries = []   # Last N query vectors (detect repetition)
        self.recent_outcomes = []  # Last N outcomes (True/False)
        self.query_count = 0
        self.hit_count = 0
        self.miss_count = 0
        self.success_count = 0
        self.failure_count = 0

    def update_query(self, query_vec: np.ndarray, found: bool):
        """Update state after a query."""
        self.query_count += 1

        if found:
            self.hit_count += 1
            self.confusion = max(0, self.confusion - 0.1)
        else:
            self.miss_count += 1
            self.confusion = min(1, self.confusion + 0.05)

        # Check repetition (similar to recent queries)
        for recent in self.recent_queries[-10:]:
            sim = cosine(query_vec, recent)
            if sim > 0.8:
                self.repetition = min(1, self.repetition + 0.1)
                break
        else:
            self.repetition = max(0, self.repetition - 0.05)

        self.recent_queries.append(query_vec)
        if len(self.recent_queries) > 20:
            self.recent_queries.pop(0)

    def update_outcome(self, worked: bool):
        """Update state after an outcome is recorded."""
        self.recent_outcomes.append(worked)
        if len(self.recent_outcomes) > 20:
            self.recent_outcomes.pop(0)

        if worked:
            self.success_count += 1
            self.confidence = min(1, self.confidence + 0.1)
            self.progress = min(1, self.progress + 0.15)
        else:
            self.failure_count += 1
            self.confidence = max(0, self.confidence - 0.15)
            self.confusion = min(1, self.confusion + 0.1)

        # Confidence from recent history
        if self.recent_outcomes:
            recent_success = sum(self.recent_outcomes) / len(self.recent_outcomes)
            self.confidence = 0.7 * self.confidence + 0.3 * recent_success

    def update_fatigue(self, tokens_used: int, tokens_limit: int = 128000):
        """Update fatigue based on context usage."""
        self.fatigue = min(1, tokens_used / tokens_limit)

    def decay(self, factor: float = 0.95):
        """Decay state toward neutral (call at session boundaries)."""
        self.confusion *= factor
        self.repetition *= factor
        self.progress *= factor

    def to_dict(self) -> Dict:
        return {
            'confidence': round(self.confidence, 3),
            'confusion': round(self.confusion, 3),
            'progress': round(self.progress, 3),
            'repetition': round(self.repetition, 3),
            'fatigue': round(self.fatigue, 3),
            'query_count': self.query_count,
            'hit_count': self.hit_count,
            'miss_count': self.miss_count,
            'success_count': self.success_count,
            'failure_count': self.failure_count,
            'hit_ratio': round(self.hit_count / max(1, self.query_count), 3),
            'success_ratio': round(self.success_count / max(1, self.success_count + self.failure_count), 3),
        }

    def from_dict(self, data: Dict):
        self.confidence = data.get('confidence', 0.5)
        self.confusion = data.get('confusion', 0.0)
        self.progress = data.get('progress', 0.0)
        self.repetition = data.get('repetition', 0.0)
        self.fatigue = data.get('fatigue', 0.0)
        self.query_count = data.get('query_count', 0)
        self.hit_count = data.get('hit_count', 0)
        self.miss_count = data.get('miss_count', 0)
        self.success_count = data.get('success_count', 0)
        self.failure_count = data.get('failure_count', 0)

    def get_warnings(self) -> List[str]:
        """Get warnings about current state."""
        warnings = []
        if self.confusion > 0.6:
            warnings.append("HIGH CONFUSION - step back and reassess")
        if self.repetition > 0.5:
            warnings.append("GOING IN CIRCLES - try different approach")
        if self.fatigue > 0.8:
            warnings.append("HIGH CONTEXT PRESSURE - summarize and continue")
        if self.confidence < 0.3 and self.failure_count > 2:
            warnings.append("LOW CONFIDENCE - check assumptions")
        return warnings


# ============================================================================
# Receipt Trace (8-dimensional, from UHMA)
# ============================================================================

class ReceiptTrace:
    """
    8-dimensional holographic trace for tracking actions.
    Dimensions: event, ctx, content, outcome, source, aux, session, time
    """

    def __init__(self):
        self.trace = np.zeros(HOLO_DIM, dtype=HOLO_DTYPE)
        self.count = 0

    def emit(self,
             event: int,
             ctx_vec: np.ndarray,
             content_vec: np.ndarray,
             outcome: float = 0.0,
             source_hash: int = 0,
             aux: int = 0,
             session_hash: int = 0) -> np.ndarray:
        """
        Emit a receipt to the trace.
        Returns the bound receipt vector.
        """
        # Generate base vectors for each dimension
        event_vec = random_vec(seed=1000 + event)
        outcome_vec = random_vec(seed=2000 + int(outcome * 100))
        source_vec = random_vec(seed=3000 + source_hash)
        aux_vec = random_vec(seed=4000 + aux)
        session_vec = random_vec(seed=5000 + session_hash)
        time_vec = random_vec(seed=6000 + int(time.time()) % 100000)

        # Bind all dimensions: event ⊗ ctx ⊗ content ⊗ outcome ⊗ source ⊗ aux ⊗ session ⊗ time
        receipt = bind(event_vec, ctx_vec)
        receipt = bind(receipt, content_vec)
        receipt = bind(receipt, outcome_vec)
        receipt = bind(receipt, source_vec)
        receipt = bind(receipt, aux_vec)
        receipt = bind(receipt, session_vec)
        receipt = bind(receipt, time_vec)

        # Superpose into trace
        self.trace = superpose(self.trace, receipt)
        self.count += 1

        return receipt

    def query(self, event: int = None, ctx_vec: np.ndarray = None) -> float:
        """Query trace for resonance with event/context."""
        if self.count == 0:
            return 0.0

        query_vec = np.ones(HOLO_DIM)
        if event is not None:
            event_vec = random_vec(seed=1000 + event)
            query_vec = bind(query_vec, event_vec)
        if ctx_vec is not None:
            query_vec = bind(query_vec, ctx_vec)

        return cosine(self.trace, query_vec)

    def decay(self, factor: float = 0.9):
        """Decay trace (forget old receipts)."""
        self.trace *= factor


# ============================================================================
# Main Holographic Memory
# ============================================================================

class HoloMemory:
    """
    Holographic memory system with orthogonal connections.

    Features:
    - 1024-dim holographic vectors
    - Semantic encoding via embeddings
    - Category traces (superposed memories)
    - Receipt trace (action logging)
    - Cognitive state tracking
    - ISON export for context injection
    """

    def __init__(self):
        self.encoder = SemanticEncoder()
        self.entries: Dict[str, HoloEntry] = {}
        self.traces: Dict[str, np.ndarray] = {}  # category -> superposed trace
        self.receipt_trace = ReceiptTrace()
        self.state = CognitiveState()
        self.surface = None  # Memory-mapped holographic surface

        # Initialize category traces with proper dtype
        for cat in CATEGORIES:
            self.traces[cat] = np.zeros(HOLO_DIM, dtype=HOLO_DTYPE)

        self._init_surface()
        self._load()
        self.ensure_bootstrapped()

    def _init_surface(self):
        """Initialize memory-mapped holographic surface (6GB preallocated)."""
        try:
            if not HOLO_SURFACE_FILE.exists():
                # Create and preallocate the file
                # Use sparse file creation (fast, doesn't actually write zeros)
                with open(HOLO_SURFACE_FILE, 'wb') as f:
                    f.seek(SURFACE_SIZE_BYTES - 1)
                    f.write(b'\x00')

            # Memory-map the surface
            self.surface = np.memmap(
                HOLO_SURFACE_FILE,
                dtype=HOLO_DTYPE,
                mode='r+',
                shape=(SURFACE_VECTOR_COUNT, HOLO_DIM)
            )
            self.surface_ptr = 0  # Next free vector slot

            # Read surface pointer from header area (first vector stores metadata)
            if self.surface[0, 0] != 0:
                self.surface_ptr = int(self.surface[0, 0])

        except Exception as e:
            print(f"[holo] Surface init failed: {e}")
            self.surface = None

    def _save_surface_ptr(self):
        """Save surface pointer to header."""
        if self.surface is not None:
            self.surface[0, 0] = self.surface_ptr
            self.surface.flush()

    def _load(self):
        """Load entries and traces from disk."""
        # Load entries
        if HOLO_ENTRIES_FILE.exists():
            try:
                data = json.loads(HOLO_ENTRIES_FILE.read_text())
                for entry_data in data:
                    entry = HoloEntry.from_dict(entry_data)
                    self.entries[entry.id] = entry
                    # Rebuild vectors
                    self._encode_entry(entry)
            except Exception as e:
                print(f"[holo] Error loading entries: {e}")

        # Load traces
        if HOLO_TRACES_FILE.exists():
            try:
                npz = np.load(HOLO_TRACES_FILE)
                for cat in CATEGORIES:
                    if cat in npz:
                        self.traces[cat] = npz[cat]
                if 'receipt_trace' in npz:
                    self.receipt_trace.trace = npz['receipt_trace']
            except Exception as e:
                print(f"[holo] Error loading traces: {e}")

        # Load state
        if HOLO_STATE_FILE.exists():
            try:
                data = json.loads(HOLO_STATE_FILE.read_text())
                self.state.from_dict(data)
            except Exception as e:
                print(f"[holo] Error loading state: {e}")

    def _save(self):
        """Save entries and traces to disk."""
        # Save entries
        entries_data = [e.to_dict() for e in self.entries.values()]
        HOLO_ENTRIES_FILE.write_text(json.dumps(entries_data, indent=2))

        # Save traces
        trace_data = {cat: trace for cat, trace in self.traces.items()}
        trace_data['receipt_trace'] = self.receipt_trace.trace
        np.savez(HOLO_TRACES_FILE, **trace_data)

        # Save state
        HOLO_STATE_FILE.write_text(json.dumps(self.state.to_dict(), indent=2))

        # Save surface pointer
        self._save_surface_ptr()

    def store_to_surface(self, vec: np.ndarray) -> int:
        """Store vector in memory-mapped surface. Returns slot index."""
        if self.surface is None:
            return -1

        # Reserve slot 0 for header
        if self.surface_ptr < 1:
            self.surface_ptr = 1

        slot = self.surface_ptr
        if slot >= SURFACE_VECTOR_COUNT - 1:
            # Surface full - wrap around (overwrite oldest)
            slot = 1

        self.surface[slot] = vec.astype(HOLO_DTYPE)
        self.surface_ptr = slot + 1
        return slot

    def read_from_surface(self, slot: int) -> np.ndarray:
        """Read vector from memory-mapped surface."""
        if self.surface is None or slot < 1 or slot >= SURFACE_VECTOR_COUNT:
            return np.zeros(HOLO_DIM, dtype=HOLO_DTYPE)
        return self.surface[slot].copy()

    def surface_stats(self) -> Dict:
        """Get surface statistics."""
        return {
            'surface_file': str(HOLO_SURFACE_FILE),
            'surface_size_gb': SURFACE_SIZE_GB,
            'vector_capacity': SURFACE_VECTOR_COUNT,
            'vectors_used': max(0, self.surface_ptr - 1) if self.surface is not None else 0,
            'utilization': (self.surface_ptr - 1) / SURFACE_VECTOR_COUNT if self.surface is not None and self.surface_ptr > 1 else 0,
            'dtype': str(HOLO_DTYPE),
            'precision_bits': np.dtype(HOLO_DTYPE).itemsize * 8,
        }

    def _encode_entry(self, entry: HoloEntry):
        """Encode entry content and context to vectors."""
        entry.content_vec = self.encoder.encode(entry.content)
        if entry.context:
            entry.context_vec = self.encoder.encode(entry.context)
        else:
            # Use content hash as context seed for consistency
            seed = int(hashlib.md5(entry.content.encode()).hexdigest()[:8], 16)
            entry.context_vec = random_vec(seed)
        entry.bound_vec = bind(entry.context_vec, entry.content_vec)

    # === Core Operations ===

    def add(self,
            category: str,
            content: str,
            context: str = None,
            source: str = None,
            tags: List[str] = None,
            metadata: Dict = None) -> HoloEntry:
        """
        Add a memory entry.

        Args:
            category: finding, failed, success, insight, question, location, todo, warning, context
            content: The memory content
            context: What were you doing when you learned this?
            source: File:line or tool name
            tags: Additional tags for filtering
            metadata: Arbitrary metadata

        Returns:
            The created entry
        """
        if category not in CATEGORIES:
            category = 'finding'  # Default

        entry = HoloEntry(
            category=category,
            content=content,
            context=context,
            source=source,
            tags=tags,
            metadata=metadata
        )

        # Encode to vectors
        self._encode_entry(entry)

        # Superpose into category trace
        self.traces[category] = superpose(self.traces[category], entry.bound_vec)

        # Emit receipt
        self.receipt_trace.emit(
            event=EVENT_TYPES['ADD'],
            ctx_vec=entry.context_vec,
            content_vec=entry.content_vec,
            source_hash=hash(source or '') % 100000,
            session_hash=hash(entry.session_id) % 100000
        )

        self.entries[entry.id] = entry
        self._save()

        return entry

    def query(self,
              query_text: str,
              category: str = None,
              limit: int = 10,
              threshold: float = 0.3) -> List[Tuple[HoloEntry, float]]:
        """
        Query memory by semantic similarity.

        Args:
            query_text: What you're looking for
            category: Limit to specific category (optional)
            limit: Max results
            threshold: Minimum similarity

        Returns:
            List of (entry, similarity) tuples, sorted by similarity
        """
        query_vec = self.encoder.encode(query_text)

        results = []
        for entry in self.entries.values():
            if category and entry.category != category:
                continue

            if entry.bound_vec is None:
                continue

            # Compute similarity
            sim = cosine(query_vec, entry.content_vec)

            # Also check trace resonance
            if entry.category in self.traces:
                trace_sim = cosine(query_vec, self.traces[entry.category])
                sim = 0.7 * sim + 0.3 * trace_sim  # Blend

            if sim >= threshold:
                results.append((entry, sim))
                entry.resonance_count += 1

        # Sort by similarity
        results.sort(key=lambda x: x[1], reverse=True)
        results = results[:limit]

        # Update state
        self.state.update_query(query_vec, found=len(results) > 0)

        # Emit receipt
        self.receipt_trace.emit(
            event=EVENT_TYPES['HIT'] if results else EVENT_TYPES['MISS'],
            ctx_vec=query_vec,
            content_vec=query_vec,
            aux=len(results)
        )

        self._save()
        return results

    def query_trace(self, query_text: str, category: str) -> float:
        """Query a category trace directly for resonance."""
        if category not in self.traces:
            return 0.0
        query_vec = self.encoder.encode(query_text)
        return cosine(query_vec, self.traces[category])

    def outcome(self, entry_id: str, worked: bool) -> bool:
        """
        Record outcome for an entry.

        Args:
            entry_id: The entry ID
            worked: True if it worked, False if it failed

        Returns:
            Success
        """
        if entry_id not in self.entries:
            return False

        entry = self.entries[entry_id]
        entry.outcome = worked
        entry.outcome_count += 1

        # Adjust trace weight based on outcome
        weight = 0.1 if worked else -0.05
        if entry.bound_vec is not None and entry.category in self.traces:
            self.traces[entry.category] = superpose(
                self.traces[entry.category],
                entry.bound_vec,
                weight=weight
            )

        # Update state
        self.state.update_outcome(worked)

        # Emit receipt
        ctx_vec = entry.context_vec if entry.context_vec is not None else np.zeros(HOLO_DIM, dtype=HOLO_DTYPE)
        content_vec = entry.content_vec if entry.content_vec is not None else np.zeros(HOLO_DIM, dtype=HOLO_DTYPE)
        self.receipt_trace.emit(
            event=EVENT_TYPES['SUCCESS'] if worked else EVENT_TYPES['FAILURE'],
            ctx_vec=ctx_vec,
            content_vec=content_vec,
            outcome=1.0 if worked else 0.0
        )

        self._save()
        return True

    def decay_traces(self, force: bool = False):
        """
        Apply decay to all traces (call at session end).

        Only decays if surface is at least 50% full (3GB of 6GB),
        unless force=True. This prevents forgetting before we've
        accumulated meaningful knowledge.
        """
        # Check if we have enough data to start decaying
        min_fill_ratio = 0.5  # 3GB of 6GB
        current_fill = (self.surface_ptr - 1) / SURFACE_VECTOR_COUNT if self.surface is not None and self.surface_ptr > 1 else 0

        if not force and current_fill < min_fill_ratio:
            # Not enough data yet - skip decay
            return False

        for cat, decay_rate in CATEGORIES.items():
            if cat in self.traces:
                self.traces[cat] *= decay_rate
        self.receipt_trace.decay()
        self.state.decay()
        self._save()
        return True

    # === State & Inspection ===

    def get_state(self) -> Dict:
        """Get current cognitive state."""
        state = self.state.to_dict()
        state['warnings'] = self.state.get_warnings()
        state['entry_count'] = len(self.entries)
        state['has_embeddings'] = HAVE_EMBEDDINGS
        return state

    def get_relevant(self, context: str, limit: int = 5) -> List[HoloEntry]:
        """Get entries most relevant to current context."""
        results = self.query(context, limit=limit)
        return [entry for entry, sim in results]

    def by_category(self, category: str, limit: int = 20) -> List[HoloEntry]:
        """Get entries by category."""
        entries = [e for e in self.entries.values() if e.category == category]
        entries.sort(key=lambda e: e.created_at, reverse=True)
        return entries[:limit]

    def recent(self, limit: int = 20) -> List[HoloEntry]:
        """Get most recent entries."""
        entries = sorted(self.entries.values(), key=lambda e: e.created_at, reverse=True)
        return entries[:limit]

    def failed_approaches(self, context: str = None, limit: int = 10) -> List[HoloEntry]:
        """Get failed approaches, optionally filtered by context."""
        if context:
            results = self.query(context, category='failed', limit=limit)
            return [entry for entry, sim in results]
        return self.by_category('failed', limit)

    def successful_approaches(self, context: str = None, limit: int = 10) -> List[HoloEntry]:
        """Get successful approaches, optionally filtered by context."""
        if context:
            results = self.query(context, category='success', limit=limit)
            return [entry for entry, sim in results]
        return self.by_category('success', limit)

    # === Export for Context Injection ===

    def to_ison(self,
                context: str = None,
                limit: int = 10,
                include_state: bool = True) -> str:
        """
        Export relevant memories as ISON format for context injection.

        ISON format:
        table.memories
        id category content outcome resonance
        abc123 finding "rcx is caller-saved" true 5
        """
        lines = []

        # State warnings first
        if include_state:
            warnings = self.state.get_warnings()
            if warnings:
                lines.append("# COGNITIVE WARNINGS")
                for w in warnings:
                    lines.append(f"# {w}")
                lines.append("")

        # Get relevant entries
        if context:
            results = self.query(context, limit=limit)
            entries = [entry for entry, sim in results]
        else:
            entries = self.recent(limit)

        if not entries:
            return "\n".join(lines) if lines else ""

        # ISON table
        lines.append("table.memories")
        lines.append("id category content outcome resonance")

        for entry in entries:
            # Escape content for ISON (quote if has spaces)
            content = entry.content.replace('"', '\\"').replace('\n', ' ')
            if ' ' in content or '"' in content:
                content = f'"{content[:100]}"'
            else:
                content = content[:100]

            outcome = "true" if entry.outcome else ("false" if entry.outcome is False else "null")

            lines.append(f"{entry.id} {entry.category} {content} {outcome} {entry.resonance_count}")

        # Add trace resonances
        if context:
            lines.append("")
            lines.append("table.trace_resonance")
            lines.append("category resonance")
            for cat in ['finding', 'failed', 'success', 'insight', 'warning']:
                res = self.query_trace(context, cat)
                if res > 0.1:
                    lines.append(f"{cat} {res:.3f}")

        return "\n".join(lines)

    def summary(self) -> str:
        """Get memory summary."""
        lines = [
            "=== Holographic Memory ===",
            f"Entries: {len(self.entries)}",
            f"Embeddings: {'yes' if HAVE_EMBEDDINGS else 'no (fallback)'}",
            "",
            "By category:"
        ]

        for cat in CATEGORIES:
            count = len([e for e in self.entries.values() if e.category == cat])
            if count > 0:
                trace_mag = magnitude(self.traces.get(cat, np.zeros(1)))
                lines.append(f"  {cat}: {count} (trace: {trace_mag:.3f})")

        lines.append("")
        lines.append("State:")
        for k, v in self.state.to_dict().items():
            if k not in ['recent_queries']:
                lines.append(f"  {k}: {v}")

        warnings = self.state.get_warnings()
        if warnings:
            lines.append("")
            lines.append("WARNINGS:")
            for w in warnings:
                lines.append(f"  ! {w}")

        return "\n".join(lines)

    # === File Digestion (Dynamic RAG Replacement) ===

    def digest_file(self, filepath: str, category: str = 'context') -> int:
        """
        Digest a file into holographic memory.
        Splits into chunks, encodes each, superposes into trace.
        Returns number of chunks processed.
        """
        p = Path(filepath)
        if not p.exists():
            return 0

        try:
            content = p.read_text()
        except:
            return 0

        # Split into chunks (paragraphs or sections)
        chunks = []
        if p.suffix in ['.md', '.txt']:
            # Split on double newlines or headers
            import re
            chunks = re.split(r'\n\n+|\n##+ ', content)
        elif p.suffix in ['.asm', '.py', '.c', '.h']:
            # Split on function/section boundaries
            import re
            chunks = re.split(r'\n;;\s*=+|\ndef |\nclass ', content)
        else:
            # Generic: split on double newlines
            chunks = content.split('\n\n')

        # Filter and limit chunk size
        chunks = [c.strip() for c in chunks if len(c.strip()) > 50]
        chunks = [c[:500] for c in chunks]  # Limit chunk size

        count = 0
        for chunk in chunks:
            # Encode and superpose into trace
            vec = self.encoder.encode(chunk)
            if category in self.traces:
                self.traces[category] = superpose(self.traces[category], vec, weight=0.5)
            count += 1

        self._save()
        return count

    def digest_project(self) -> int:
        """
        Digest key project files into holographic memory.
        Called on first load to bootstrap project knowledge.
        """
        project_root = Path(__file__).parent.parent.parent  # uhma-asm

        # Key files to digest
        key_files = [
            ('CLAUDE.md', 'context'),   # Project instructions
            ('README.md', 'context'),   # If exists
        ]

        total = 0
        for filename, category in key_files:
            filepath = project_root / filename
            if filepath.exists():
                count = self.digest_file(str(filepath), category)
                total += count

        return total

    def ensure_bootstrapped(self) -> bool:
        """Ensure project knowledge is loaded (call on first use)."""
        bootstrap_marker = MEMORY_DIR / '.bootstrapped'
        if bootstrap_marker.exists():
            return False

        count = self.digest_project()
        count += self.ingest_gotchas()
        if count > 0:
            bootstrap_marker.write_text(f"Bootstrapped {count} items at {datetime.now().isoformat()}")
            return True
        return False

    def ingest_gotchas(self) -> int:
        """
        Ingest gotchas from CLAUDE.md into holographic memory as warnings.
        Parses the Hard-Learned Lessons section.
        """
        project_root = Path(__file__).parent.parent.parent
        claude_md = project_root / 'CLAUDE.md'

        if not claude_md.exists():
            return 0

        content = claude_md.read_text()
        count = 0

        # Parse gotchas sections
        import re

        # Find Hard-Learned Lessons section
        lessons_match = re.search(r'## Hard-Learned Lessons(.*?)(?=\n## |\Z)', content, re.DOTALL)
        if lessons_match:
            lessons = lessons_match.group(1)

            # Extract bullet points
            bullets = re.findall(r'\*\*([^*]+)\*\*\s*[-–—]\s*([^\n]+)', lessons)
            for title, detail in bullets:
                gotcha = f"{title.strip()}: {detail.strip()}"
                self.add('warning', gotcha, context='CLAUDE.md gotchas', source='CLAUDE.md')
                count += 1

            # Also get sub-bullets
            sub_bullets = re.findall(r'^\s*-\s+([^\n]+)', lessons, re.MULTILINE)
            for bullet in sub_bullets:
                if len(bullet) > 30:  # Skip short ones
                    self.add('warning', bullet.strip(), context='CLAUDE.md gotchas', source='CLAUDE.md')
                    count += 1

        # Try to ingest from old index.json if it exists
        index_file = Path(__file__).parent / 'index.json'
        if index_file.exists():
            try:
                import json
                index = json.loads(index_file.read_text())
                for filename, data in index.get('files', {}).items():
                    for gotcha in data.get('gotchas', []):
                        self.add('warning', gotcha, context=f'{filename} gotcha', source=filename)
                        count += 1
            except:
                pass

        return count


# ============================================================================
# Convenience Functions
# ============================================================================

_memory = None

def get_memory() -> HoloMemory:
    """Get singleton memory instance."""
    global _memory
    if _memory is None:
        _memory = HoloMemory()
    return _memory


def add(category: str, content: str, context: str = None, **kwargs) -> HoloEntry:
    """Add memory entry."""
    return get_memory().add(category, content, context, **kwargs)


def query(query_text: str, **kwargs) -> List[Tuple[HoloEntry, float]]:
    """Query memory."""
    return get_memory().query(query_text, **kwargs)


def outcome(entry_id: str, worked: bool) -> bool:
    """Record outcome."""
    return get_memory().outcome(entry_id, worked)


def inject_context(context: str, limit: int = 5) -> str:
    """Get ISON-formatted context for injection."""
    return get_memory().to_ison(context, limit)


# ============================================================================
# CLI
# ============================================================================

def main():
    import sys

    mem = HoloMemory()

    if len(sys.argv) < 2:
        print(mem.summary())
        return

    cmd = sys.argv[1]
    args = sys.argv[2:]

    if cmd == 'add' and len(args) >= 2:
        category, content = args[0], ' '.join(args[1:])
        entry = mem.add(category, content)
        print(f"Added: {entry.id}")

    elif cmd == 'query' and args:
        query_text = ' '.join(args)
        results = mem.query(query_text)
        for entry, sim in results:
            print(f"[{sim:.3f}] [{entry.category}] {entry.content[:80]}")

    elif cmd == 'recent':
        limit = int(args[0]) if args else 10
        for entry in mem.recent(limit):
            print(f"[{entry.category}] {entry.content[:80]}")

    elif cmd == 'ison':
        context = ' '.join(args) if args else None
        print(mem.to_ison(context))

    elif cmd == 'state':
        for k, v in mem.get_state().items():
            print(f"{k}: {v}")

    elif cmd == 'failed':
        for entry in mem.failed_approaches():
            print(f"[FAILED] {entry.content[:80]}")

    elif cmd == 'success':
        for entry in mem.successful_approaches():
            print(f"[SUCCESS] {entry.content[:80]}")

    else:
        print("""
Usage:
  python holo_memory.py              # Summary
  python holo_memory.py add <cat> <content>
  python holo_memory.py query <text>
  python holo_memory.py recent [n]
  python holo_memory.py ison [context]
  python holo_memory.py state
  python holo_memory.py failed
  python holo_memory.py success

Categories: finding, failed, success, insight, question, location, todo, warning, context
        """)


if __name__ == '__main__':
    main()
