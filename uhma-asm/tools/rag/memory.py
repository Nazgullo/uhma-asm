#!/usr/bin/env python3
"""
Claude's Semantic Memory System
================================

Structured, searchable, persistent memory across sessions.

Categories:
  - finding: Confirmed facts discovered
  - hypothesis: Things we think might be true
  - failed: Things we tried that didn't work
  - success: Things that worked
  - insight: Aha moments, connections
  - location: Important code locations
  - question: Open questions
  - todo: Things to try next

Features:
  - Semantic search (TF-IDF + embeddings if available)
  - Theme clustering
  - Relationship tracking
  - Session grouping
  - Export/import

Usage:
  from memory import Memory
  mem = Memory()
  
  mem.add('finding', 'learn! receives NIL because experts never return values')
  mem.add('hypothesis', 'hits not incrementing because no owner set')
  mem.add('location', 'learn! defined at expert.lisp:215', tags=['learn', 'expert'])
  
  mem.search('NIL return')
  mem.by_category('hypothesis')
  mem.by_theme('expert system')
  mem.related_to(entry_id)
  mem.session_summary(session_id)
"""

import json
import math
import os
import re
import time
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Optional, List, Dict, Any, Tuple
import hashlib

# ============================================================================
# Configuration
# ============================================================================

RAG_DIR = Path(__file__).parent
MEMORY_DIR = RAG_DIR / 'memory'
MEMORY_DIR.mkdir(exist_ok=True)

ENTRIES_FILE = MEMORY_DIR / 'entries.json'
MEMORY_INDEX_FILE = MEMORY_DIR / 'memory_index.json'  # Renamed to avoid conflict with RAG index
SESSIONS_FILE = MEMORY_DIR / 'sessions.json'
THEMES_FILE = MEMORY_DIR / 'themes.json'

# Categories with their icons for display
CATEGORIES = {
    'finding': '✓',      # Confirmed facts
    'hypothesis': '?',   # Things we think might be true  
    'failed': '✗',       # Things that didn't work
    'success': '★',      # Things that worked
    'insight': '💡',     # Aha moments
    'location': '📍',    # Code locations
    'question': '❓',    # Open questions
    'todo': '→',         # Things to try
    'note': '•',         # General notes
    'error': '⚠',        # Errors encountered
    'trace': '↳',        # Trace/debug output
}

# ============================================================================
# Text Processing for Search
# ============================================================================

# Common stopwords to ignore
STOPWORDS = {
    'a', 'an', 'the', 'is', 'are', 'was', 'were', 'be', 'been', 'being',
    'have', 'has', 'had', 'do', 'does', 'did', 'will', 'would', 'could',
    'should', 'may', 'might', 'must', 'shall', 'can', 'need', 'dare',
    'to', 'of', 'in', 'for', 'on', 'with', 'at', 'by', 'from', 'as',
    'into', 'through', 'during', 'before', 'after', 'above', 'below',
    'between', 'under', 'again', 'further', 'then', 'once', 'here',
    'there', 'when', 'where', 'why', 'how', 'all', 'each', 'few', 'more',
    'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own',
    'same', 'so', 'than', 'too', 'very', 'just', 'and', 'but', 'if', 'or',
    'because', 'until', 'while', 'this', 'that', 'these', 'those', 'it',
    'its', 'i', 'we', 'you', 'he', 'she', 'they', 'what', 'which', 'who',
}

def tokenize(text: str) -> List[str]:
    """Tokenize text into searchable terms."""
    # Lowercase and split on non-alphanumeric
    tokens = re.findall(r'[a-z][a-z0-9\-\_]*', text.lower())
    # Filter stopwords and short tokens
    return [t for t in tokens if t not in STOPWORDS and len(t) > 2]

def extract_keywords(text: str, max_keywords: int = 10) -> List[str]:
    """Extract important keywords from text."""
    tokens = tokenize(text)
    # Count frequencies
    freq = defaultdict(int)
    for t in tokens:
        freq[t] += 1
    # Sort by frequency
    sorted_tokens = sorted(freq.items(), key=lambda x: x[1], reverse=True)
    return [t for t, _ in sorted_tokens[:max_keywords]]

# ============================================================================
# TF-IDF Search Index
# ============================================================================

class SearchIndex:
    """TF-IDF based search index."""
    
    def __init__(self):
        self.documents: Dict[str, List[str]] = {}  # id -> tokens
        self.doc_freq: Dict[str, int] = defaultdict(int)  # term -> num docs containing
        self.total_docs = 0
    
    def add(self, doc_id: str, text: str):
        """Add document to index."""
        tokens = tokenize(text)
        
        # Update doc frequencies
        seen = set()
        for t in tokens:
            if t not in seen:
                self.doc_freq[t] += 1
                seen.add(t)
        
        self.documents[doc_id] = tokens
        self.total_docs = len(self.documents)
    
    def remove(self, doc_id: str):
        """Remove document from index."""
        if doc_id in self.documents:
            tokens = self.documents[doc_id]
            seen = set()
            for t in tokens:
                if t not in seen:
                    self.doc_freq[t] -= 1
                    if self.doc_freq[t] <= 0:
                        del self.doc_freq[t]
                    seen.add(t)
            del self.documents[doc_id]
            self.total_docs = len(self.documents)
    
    def search(self, query: str, top_k: int = 20) -> List[Tuple[str, float]]:
        """Search for query, return [(doc_id, score)]."""
        query_tokens = tokenize(query)
        if not query_tokens:
            return []
        
        # Compute query TF-IDF
        query_tf = defaultdict(int)
        for t in query_tokens:
            query_tf[t] += 1
        
        query_vec = {}
        for t, tf in query_tf.items():
            if t in self.doc_freq:
                idf = math.log(self.total_docs / (self.doc_freq[t] + 1))
                query_vec[t] = tf * idf
        
        if not query_vec:
            return []
        
        # Score each document
        scores = []
        for doc_id, doc_tokens in self.documents.items():
            # Document TF
            doc_tf = defaultdict(int)
            for t in doc_tokens:
                doc_tf[t] += 1
            
            # Compute similarity
            score = 0.0
            for t, q_weight in query_vec.items():
                if t in doc_tf:
                    idf = math.log(self.total_docs / (self.doc_freq[t] + 1))
                    d_weight = doc_tf[t] * idf
                    score += q_weight * d_weight
            
            if score > 0:
                scores.append((doc_id, score))
        
        # Sort by score
        scores.sort(key=lambda x: x[1], reverse=True)
        return scores[:top_k]
    
    def save(self, path: Path):
        """Save index to file."""
        data = {
            'documents': self.documents,
            'doc_freq': dict(self.doc_freq),
            'total_docs': self.total_docs
        }
        path.write_text(json.dumps(data))
    
    def load(self, path: Path):
        """Load index from file."""
        if path.exists():
            try:
                data = json.loads(path.read_text())
                self.documents = data.get('documents', {})
                self.doc_freq = defaultdict(int, data.get('doc_freq', {}))
                self.total_docs = data.get('total_docs', 0)
            except:
                pass

# ============================================================================
# Memory Entry
# ============================================================================

class Entry:
    """A single memory entry."""
    
    def __init__(self, 
                 category: str,
                 content: str,
                 tags: List[str] = None,
                 related_to: List[str] = None,
                 session_id: str = None,
                 metadata: Dict = None):
        
        self.id = self._generate_id(content)
        self.category = category
        self.content = content
        self.tags = tags or []
        self.related_to = related_to or []
        self.session_id = session_id
        self.metadata = metadata or {}
        self.created_at = datetime.now().isoformat()
        self.updated_at = self.created_at
        self.keywords = extract_keywords(content)
    
    def _generate_id(self, content: str) -> str:
        """Generate unique ID from content + timestamp."""
        h = hashlib.md5(f"{content}{time.time()}".encode()).hexdigest()[:12]
        return h
    
    def to_dict(self) -> Dict:
        return {
            'id': self.id,
            'category': self.category,
            'content': self.content,
            'tags': self.tags,
            'related_to': self.related_to,
            'session_id': self.session_id,
            'metadata': self.metadata,
            'created_at': self.created_at,
            'updated_at': self.updated_at,
            'keywords': self.keywords
        }
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'Entry':
        # Accept either 'content' or 'text' for backwards compatibility
        content = data.get('content') or data.get('text', '')
        entry = cls(
            category=data['category'],
            content=content,
            tags=data.get('tags', []),
            related_to=data.get('related_to', []),
            session_id=data.get('session_id') or data.get('session'),
            metadata=data.get('metadata', {})
        )
        entry.id = data.get('id', entry.id)
        entry.created_at = data.get('created_at') or data.get('timestamp', entry.created_at)
        entry.updated_at = data.get('updated_at', entry.updated_at)
        entry.keywords = data.get('keywords', [])
        return entry
    
    def display(self, verbose: bool = False) -> str:
        """Format entry for display."""
        icon = CATEGORIES.get(self.category, '•')
        line = f"[{icon}] {self.content[:80]}{'...' if len(self.content) > 80 else ''}"
        
        if verbose:
            lines = [line]
            lines.append(f"    id: {self.id}")
            lines.append(f"    category: {self.category}")
            if self.tags:
                lines.append(f"    tags: {', '.join(self.tags)}")
            if self.keywords:
                lines.append(f"    keywords: {', '.join(self.keywords[:5])}")
            lines.append(f"    created: {self.created_at}")
            return '\n'.join(lines)
        
        return line

# ============================================================================
# Session Tracking
# ============================================================================

class Session:
    """Represents an investigation session."""
    
    def __init__(self, name: str = None, focus: str = None):
        self.id = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.name = name or f"Session {self.id}"
        self.focus = focus
        self.started_at = datetime.now().isoformat()
        self.ended_at = None
        self.entry_ids: List[str] = []
        self.summary: Optional[str] = None
    
    def to_dict(self) -> Dict:
        return {
            'id': self.id,
            'name': self.name,
            'focus': self.focus,
            'started_at': self.started_at,
            'ended_at': self.ended_at,
            'entry_ids': self.entry_ids,
            'summary': self.summary
        }
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'Session':
        session = cls(name=data.get('name'), focus=data.get('focus'))
        session.id = data['id']
        session.started_at = data.get('started_at', session.started_at)
        session.ended_at = data.get('ended_at')
        session.entry_ids = data.get('entry_ids', [])
        session.summary = data.get('summary')
        return session

# ============================================================================
# Theme Clustering
# ============================================================================

class ThemeCluster:
    """Groups entries by theme."""
    
    def __init__(self):
        self.themes: Dict[str, Dict] = {}  # theme_name -> {keywords, entry_ids}
    
    def add_theme(self, name: str, keywords: List[str]):
        """Define a theme."""
        self.themes[name] = {
            'keywords': [k.lower() for k in keywords],
            'entry_ids': []
        }
    
    def categorize(self, entry: Entry) -> List[str]:
        """Find themes that match an entry."""
        matches = []
        entry_text = f"{entry.content} {' '.join(entry.tags)}".lower()
        
        for theme_name, theme_data in self.themes.items():
            for keyword in theme_data['keywords']:
                if keyword in entry_text:
                    matches.append(theme_name)
                    if entry.id not in theme_data['entry_ids']:
                        theme_data['entry_ids'].append(entry.id)
                    break
        
        return matches
    
    def get_entries(self, theme: str) -> List[str]:
        """Get entry IDs for a theme."""
        if theme in self.themes:
            return self.themes[theme]['entry_ids']
        return []
    
    def to_dict(self) -> Dict:
        return self.themes
    
    def from_dict(self, data: Dict):
        self.themes = data

# ============================================================================
# Main Memory Class
# ============================================================================

class Memory:
    """
    Semantic memory system with structured storage and search.
    """
    
    # Default themes
    # UHMA-asm specific themes
    DEFAULT_THEMES = {
        'dispatch': ['dispatch', 'routing', 'predict', 'hits', 'misses', 'process_token'],
        'learning': ['learn', 'emit', 'pattern', 'region', 'holo_store'],
        'vsa-holographic': ['vsa', 'holographic', 'vector', 'bind', 'superpose', 'trace'],
        'registers': ['rcx', 'rax', 'rdi', 'rsi', 'r14', 'caller-saved', 'clobbered'],
        'fault-handling': ['fault', 'sigsegv', 'longjmp', 'recovery', 'signal'],
        'context-hash': ['context', 'hash', 'prev_token', 'ctx_hash'],
        'token-abstraction': ['token', 'abstract', 'hex', 'number', 'digest'],
        'receipt-trace': ['receipt', 'trace', 'unified', 'why', 'misses'],
        'dreams-observe': ['dream', 'observe', 'consolidate', 'prune', 'schema'],
        'performance': ['slow', 'fast', 'optimize', 'O(n)', 'O(1)'],
    }
    
    def __init__(self, workspace: Path = None):
        self.workspace = workspace or MEMORY_DIR
        self.entries: Dict[str, Entry] = {}
        self.sessions: Dict[str, Session] = {}
        self.current_session: Optional[Session] = None
        self.index = SearchIndex()
        self.themes = ThemeCluster()
        
        self._load()
        self._init_themes()
    
    def _init_themes(self):
        """Initialize default themes if not present."""
        if not self.themes.themes:
            for name, keywords in self.DEFAULT_THEMES.items():
                self.themes.add_theme(name, keywords)
    
    def _load(self):
        """Load all data from files."""
        # Load entries
        if ENTRIES_FILE.exists():
            try:
                data = json.loads(ENTRIES_FILE.read_text())
                for entry_data in data:
                    entry = Entry.from_dict(entry_data)
                    self.entries[entry.id] = entry
            except Exception as e:
                print(f"[memory] Error loading entries: {e}")
        
        # Load sessions
        if SESSIONS_FILE.exists():
            try:
                data = json.loads(SESSIONS_FILE.read_text())
                for session_data in data:
                    session = Session.from_dict(session_data)
                    self.sessions[session.id] = session
            except:
                pass
        
        # Load index
        self.index.load(MEMORY_INDEX_FILE)
        
        # Load themes
        if THEMES_FILE.exists():
            try:
                self.themes.from_dict(json.loads(THEMES_FILE.read_text()))
            except:
                pass
    
    def _save(self):
        """Save all data to files."""
        # Save entries
        entries_data = [e.to_dict() for e in self.entries.values()]
        ENTRIES_FILE.write_text(json.dumps(entries_data, indent=2))
        
        # Save sessions
        sessions_data = [s.to_dict() for s in self.sessions.values()]
        SESSIONS_FILE.write_text(json.dumps(sessions_data, indent=2))
        
        # Save index
        self.index.save(MEMORY_INDEX_FILE)
        
        # Save themes
        THEMES_FILE.write_text(json.dumps(self.themes.to_dict(), indent=2))
    
    # === Session Management ===
    
    def start_session(self, name: str = None, focus: str = None) -> Session:
        """Start a new session."""
        if self.current_session:
            self.end_session()
        
        self.current_session = Session(name=name, focus=focus)
        self.sessions[self.current_session.id] = self.current_session
        self._save()
        return self.current_session
    
    def end_session(self, summary: str = None):
        """End current session."""
        if self.current_session:
            self.current_session.ended_at = datetime.now().isoformat()
            self.current_session.summary = summary
            self._save()
            self.current_session = None
    
    def get_session(self, session_id: str) -> Optional[Session]:
        """Get session by ID."""
        return self.sessions.get(session_id)
    
    def list_sessions(self, limit: int = 20) -> List[Session]:
        """List recent sessions."""
        sessions = sorted(self.sessions.values(), 
                         key=lambda s: s.started_at, reverse=True)
        return sessions[:limit]
    
    # === Entry Management ===
    
    def add(self, 
            category: str, 
            content: str, 
            tags: List[str] = None,
            related_to: List[str] = None,
            metadata: Dict = None) -> Entry:
        """Add a new memory entry."""
        
        session_id = self.current_session.id if self.current_session else None
        
        entry = Entry(
            category=category,
            content=content,
            tags=tags,
            related_to=related_to,
            session_id=session_id,
            metadata=metadata
        )
        
        self.entries[entry.id] = entry
        
        # Update index
        self.index.add(entry.id, f"{content} {' '.join(tags or [])}")
        
        # Categorize by theme
        self.themes.categorize(entry)
        
        # Track in session
        if self.current_session:
            self.current_session.entry_ids.append(entry.id)
        
        self._save()
        return entry
    
    def get(self, entry_id: str) -> Optional[Entry]:
        """Get entry by ID."""
        return self.entries.get(entry_id)
    
    def update(self, entry_id: str, content: str = None, tags: List[str] = None):
        """Update an entry."""
        if entry_id in self.entries:
            entry = self.entries[entry_id]
            if content:
                entry.content = content
                entry.keywords = extract_keywords(content)
            if tags:
                entry.tags = tags
            entry.updated_at = datetime.now().isoformat()
            
            # Re-index
            self.index.remove(entry_id)
            self.index.add(entry_id, f"{entry.content} {' '.join(entry.tags)}")
            
            self._save()
            return entry
        return None
    
    def delete(self, entry_id: str) -> bool:
        """Delete an entry."""
        if entry_id in self.entries:
            self.index.remove(entry_id)
            del self.entries[entry_id]
            self._save()
            return True
        return False
    
    def link(self, entry_id: str, related_id: str):
        """Link two entries as related."""
        if entry_id in self.entries and related_id in self.entries:
            if related_id not in self.entries[entry_id].related_to:
                self.entries[entry_id].related_to.append(related_id)
            if entry_id not in self.entries[related_id].related_to:
                self.entries[related_id].related_to.append(entry_id)
            self._save()
    
    # === Search & Query ===
    
    def search(self, query: str, limit: int = 20) -> List[Entry]:
        """Semantic search across all entries."""
        results = self.index.search(query, top_k=limit)
        return [self.entries[doc_id] for doc_id, score in results if doc_id in self.entries]
    
    def by_category(self, category: str, limit: int = 50) -> List[Entry]:
        """Get entries by category."""
        entries = [e for e in self.entries.values() if e.category == category]
        entries.sort(key=lambda e: e.created_at, reverse=True)
        return entries[:limit]
    
    def by_tag(self, tag: str, limit: int = 50) -> List[Entry]:
        """Get entries by tag."""
        tag = tag.lower()
        entries = [e for e in self.entries.values() if tag in [t.lower() for t in e.tags]]
        entries.sort(key=lambda e: e.created_at, reverse=True)
        return entries[:limit]
    
    def by_theme(self, theme: str) -> List[Entry]:
        """Get entries by theme."""
        entry_ids = self.themes.get_entries(theme)
        return [self.entries[eid] for eid in entry_ids if eid in self.entries]
    
    def by_session(self, session_id: str) -> List[Entry]:
        """Get entries from a session."""
        session = self.sessions.get(session_id)
        if session:
            return [self.entries[eid] for eid in session.entry_ids if eid in self.entries]
        return []
    
    def related_to(self, entry_id: str) -> List[Entry]:
        """Get entries related to an entry."""
        entry = self.entries.get(entry_id)
        if entry:
            return [self.entries[rid] for rid in entry.related_to if rid in self.entries]
        return []
    
    def recent(self, limit: int = 20) -> List[Entry]:
        """Get most recent entries."""
        entries = sorted(self.entries.values(), key=lambda e: e.created_at, reverse=True)
        return entries[:limit]
    
    # === Convenience Methods ===
    
    def finding(self, content: str, tags: List[str] = None) -> Entry:
        """Add a finding."""
        return self.add('finding', content, tags)
    
    def hypothesis(self, content: str, tags: List[str] = None) -> Entry:
        """Add a hypothesis."""
        return self.add('hypothesis', content, tags)
    
    def failed(self, content: str, tags: List[str] = None) -> Entry:
        """Record something that failed."""
        return self.add('failed', content, tags)
    
    def success(self, content: str, tags: List[str] = None) -> Entry:
        """Record a success."""
        return self.add('success', content, tags)
    
    def insight(self, content: str, tags: List[str] = None) -> Entry:
        """Record an insight."""
        return self.add('insight', content, tags)
    
    def location(self, content: str, file: str = None, line: int = None, tags: List[str] = None) -> Entry:
        """Record a code location."""
        metadata = {}
        if file:
            metadata['file'] = file
        if line:
            metadata['line'] = line
        return self.add('location', content, tags, metadata=metadata)
    
    def question(self, content: str, tags: List[str] = None) -> Entry:
        """Record an open question."""
        return self.add('question', content, tags)
    
    def todo(self, content: str, tags: List[str] = None) -> Entry:
        """Record something to try."""
        return self.add('todo', content, tags)
    
    # === Themes ===
    
    def add_theme(self, name: str, keywords: List[str]):
        """Add a custom theme."""
        self.themes.add_theme(name, keywords)
        # Re-categorize existing entries
        for entry in self.entries.values():
            self.themes.categorize(entry)
        self._save()
    
    def list_themes(self) -> Dict[str, int]:
        """List themes with entry counts."""
        return {name: len(data['entry_ids']) for name, data in self.themes.themes.items()}
    
    # === Display ===
    
    def display_entries(self, entries: List[Entry], verbose: bool = False) -> str:
        """Format entries for display."""
        if not entries:
            return "[no entries]"
        
        lines = []
        for entry in entries:
            lines.append(entry.display(verbose))
        return '\n'.join(lines)
    
    def summary(self) -> str:
        """Get memory summary."""
        lines = [
            "=== Memory Summary ===",
            f"Total entries: {len(self.entries)}",
            f"Sessions: {len(self.sessions)}",
            "",
            "By category:"
        ]
        
        # Count by category
        cat_counts = defaultdict(int)
        for entry in self.entries.values():
            cat_counts[entry.category] += 1
        
        for cat, count in sorted(cat_counts.items(), key=lambda x: x[1], reverse=True):
            icon = CATEGORIES.get(cat, '•')
            lines.append(f"  {icon} {cat}: {count}")
        
        lines.append("")
        lines.append("By theme:")
        for theme, count in sorted(self.list_themes().items(), key=lambda x: x[1], reverse=True):
            if count > 0:
                lines.append(f"  {theme}: {count}")
        
        return '\n'.join(lines)
    
    def export_markdown(self, filepath: Path = None) -> str:
        """Export all memories as markdown."""
        lines = [
            "# Memory Export",
            f"Generated: {datetime.now().isoformat()}",
            f"Total entries: {len(self.entries)}",
            ""
        ]
        
        # Group by category
        by_cat = defaultdict(list)
        for entry in self.entries.values():
            by_cat[entry.category].append(entry)
        
        for cat in CATEGORIES:
            entries = by_cat.get(cat, [])
            if entries:
                icon = CATEGORIES[cat]
                lines.append(f"## {icon} {cat.title()} ({len(entries)})")
                lines.append("")
                
                for entry in sorted(entries, key=lambda e: e.created_at, reverse=True):
                    lines.append(f"### {entry.content[:60]}...")
                    lines.append(f"- ID: `{entry.id}`")
                    lines.append(f"- Created: {entry.created_at}")
                    if entry.tags:
                        lines.append(f"- Tags: {', '.join(entry.tags)}")
                    lines.append(f"\n{entry.content}\n")
                
                lines.append("")
        
        content = '\n'.join(lines)
        
        if filepath:
            filepath.write_text(content)
        
        return content

# ============================================================================
# CLI Interface
# ============================================================================

def main():
    import sys
    
    mem = Memory()
    
    if len(sys.argv) < 2:
        print(mem.summary())
        return
    
    cmd = sys.argv[1]
    args = sys.argv[2:]
    
    if cmd == 'add' and len(args) >= 2:
        category, content = args[0], ' '.join(args[1:])
        entry = mem.add(category, content)
        print(f"Added: {entry.id}")
    
    elif cmd == 'search' and args:
        query = ' '.join(args)
        results = mem.search(query)
        print(mem.display_entries(results))
    
    elif cmd == 'category' and args:
        results = mem.by_category(args[0])
        print(mem.display_entries(results))
    
    elif cmd == 'theme' and args:
        results = mem.by_theme(args[0])
        print(mem.display_entries(results))
    
    elif cmd == 'recent':
        limit = int(args[0]) if args else 20
        results = mem.recent(limit)
        print(mem.display_entries(results))
    
    elif cmd == 'themes':
        for theme, count in sorted(mem.list_themes().items(), key=lambda x: x[1], reverse=True):
            print(f"  {theme}: {count}")
    
    elif cmd == 'export':
        filepath = Path(args[0]) if args else None
        content = mem.export_markdown(filepath)
        if not filepath:
            print(content)
    
    elif cmd == 'summary':
        print(mem.summary())
    
    else:
        print("""
Usage:
  python memory.py                          # Show summary
  python memory.py add <category> <content> # Add entry
  python memory.py search <query>           # Search entries
  python memory.py category <category>      # List by category
  python memory.py theme <theme>            # List by theme
  python memory.py recent [n]               # Recent entries
  python memory.py themes                   # List themes
  python memory.py export [file]            # Export markdown
  python memory.py summary                  # Show summary

Categories: finding, hypothesis, failed, success, insight, location, question, todo, note, error, trace
        """)

if __name__ == '__main__':
    main()
