#!/usr/bin/env python3
"""
server.py — MCP Server for UHMA (Claude Code integration via stdio)

@entry main() -> MCP protocol loop over stdin/stdout
@calls uhma binary via subprocess (stdin/stdout pipe)

ARCHITECTURE:
  Claude Code ──(stdio JSON-RPC)──→ MCP Server ──(pipe)──→ UHMA subprocess

NOTE: This is for Claude Code MCP integration only.
      For 6-channel TCP I/O, connect directly to UHMA (ports 9999-9994).

CONFIG: PROJECT_ROOT/.mcp.json (restart Claude Code after changes)

TOOLS (28):
  Input:   input, raw
  Status:  help, status, self, metacog, intro, debugger, genes, subroutines,
           regions, presence, drives
  Debug:   why, misses, receipts, listen, trace
  Actions: dream, observe, compact, reset
  I/O:     save, load, eat
  Hive:    hive, share, colony, export, import_gene
  Other:   geom, web_fetch, quit

GOTCHAS:
  - MCP config at PROJECT_ROOT/.mcp.json, not ~/.claude/mcp.json
  - Restart Claude Code after .mcp.json changes
  - UHMA auto-spawns on first tool call
"""

import json
import sys
import subprocess
import threading
import queue
import time
import socket
import select
import urllib.request
import urllib.error
from pathlib import Path

# UHMA process and communication
uhma_process = None
uhma_output_queue = queue.Queue()
uhma_lock = threading.Lock()

UHMA_DIR = Path(__file__).parent.parent.parent  # uhma-asm root
UHMA_BIN = UHMA_DIR / "uhma"


def read_uhma_output(proc):
    """Background thread to read UHMA stdout."""
    while True:
        try:
            line = proc.stdout.readline()
            if not line:
                break
            uhma_output_queue.put(line)
        except:
            break


def start_uhma():
    """Spawn UHMA process if not running."""
    global uhma_process

    if uhma_process and uhma_process.poll() is None:
        return True  # Already running

    if not UHMA_BIN.exists():
        return False

    try:
        uhma_process = subprocess.Popen(
            [str(UHMA_BIN)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1,
            cwd=str(UHMA_DIR)
        )

        # Start output reader thread
        reader = threading.Thread(target=read_uhma_output, args=(uhma_process,), daemon=True)
        reader.start()

        # Wait for banner/prompt
        time.sleep(0.5)
        return True
    except Exception as e:
        return False


def send_to_uhma(text):
    """Send text to UHMA and collect response. No timeout - waits for prompt."""
    global uhma_process

    # Check if process died
    if uhma_process and uhma_process.poll() is not None:
        log(f"UHMA process died with code {uhma_process.returncode}, restarting...")
        uhma_process = None

    if not uhma_process:
        if not start_uhma():
            return "Error: UHMA not running and failed to start"
        time.sleep(1.5)
        while not uhma_output_queue.empty():
            try:
                uhma_output_queue.get_nowait()
            except:
                break

    # Send input
    log(f"Sending to UHMA: {text}")
    with uhma_lock:
        try:
            uhma_process.stdin.write(text + "\n")
            uhma_process.stdin.flush()
        except Exception as e:
            return f"Error sending to UHMA: {e}"

    # Collect output until prompt or 3s idle (after first output)
    output_lines = []
    last_output = time.time()
    while True:
        try:
            line = uhma_output_queue.get(timeout=0.5)
            output_lines.append(line.rstrip())
            last_output = time.time()
            log(f"Got line: {line.rstrip()[:60]}")
            if line.strip().endswith("uhma>") or line.strip() == "uhma>":
                break
        except queue.Empty:
            # If we have output and 3s passed with no new output, done
            if output_lines and (time.time() - last_output) > 3.0:
                break
            continue

    log(f"Collected {len(output_lines)} lines")
    return "\n".join(output_lines)


def send_to_uhma_async(text):
    """Send text to UHMA without waiting for response (fire and forget)."""
    global uhma_process

    if uhma_process and uhma_process.poll() is not None:
        uhma_process = None

    if not uhma_process:
        if not start_uhma():
            return "Error: UHMA not running"
        time.sleep(1.5)
        while not uhma_output_queue.empty():
            try:
                uhma_output_queue.get_nowait()
            except:
                break

    with uhma_lock:
        try:
            uhma_process.stdin.write(text + "\n")
            uhma_process.stdin.flush()
            return "sent"
        except Exception as e:
            return f"Error: {e}"


def web_fetch(url):
    """Fetch URL content for UHMA digestion."""
    try:
        req = urllib.request.Request(url, headers={'User-Agent': 'UHMA/1.0'})
        with urllib.request.urlopen(req, timeout=30) as resp:
            content = resp.read().decode('utf-8', errors='replace')
            return content[:100000]  # Limit size
    except Exception as e:
        return f"Error fetching {url}: {e}"


# MCP protocol
# Track whether client uses Content-Length framing (some use raw JSON lines)
_use_content_length = True

def send_response(id, result):
    global _use_content_length
    response = {"jsonrpc": "2.0", "id": id, "result": result}
    msg = json.dumps(response)
    if _use_content_length:
        sys.stdout.write(f"Content-Length: {len(msg)}\r\n\r\n{msg}")
    else:
        sys.stdout.write(msg + "\n")
    sys.stdout.flush()
    log(f"Sent response: {msg[:100]}...")


def send_error(id, code, message):
    global _use_content_length
    response = {"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message}}
    msg = json.dumps(response)
    if _use_content_length:
        sys.stdout.write(f"Content-Length: {len(msg)}\r\n\r\n{msg}")
    else:
        sys.stdout.write(msg + "\n")
    sys.stdout.flush()


# All UHMA commands as MCP tools
TOOLS = [
    # Input processing
    {
        "name": "input",
        "description": "Send text to UHMA for token processing and prediction",
        "inputSchema": {
            "type": "object",
            "properties": {
                "text": {"type": "string", "description": "Text to process"}
            },
            "required": ["text"]
        }
    },

    # Status commands
    {
        "name": "help",
        "description": "Show UHMA help and available commands",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "status",
        "description": "Show system status (regions, accuracy, drives)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "self",
        "description": "Show self-knowledge (strengths/weaknesses)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "metacog",
        "description": "Show metacognitive state (per-context confidence)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "debugger",
        "description": "Show self-debugger status (breakpoints, learning events)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "genes",
        "description": "Show gene pool status (composted patterns)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "subroutines",
        "description": "Show shared subroutines (recursive schemas)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "regions",
        "description": "List all regions with hit/miss stats",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "presence",
        "description": "Show presence field values (hormonal state)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "drives",
        "description": "Show drive levels and thresholds",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "intro",
        "description": "Show introspective state (CONFUSED, CONFIDENT, LEARNING, SELF-AWARE readings)",
        "inputSchema": {"type": "object", "properties": {}}
    },

    # Debug/trace commands
    {
        "name": "why",
        "description": "Explain last prediction failure using unified trace",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "misses",
        "description": "Show last n misses with predicted vs actual",
        "inputSchema": {
            "type": "object",
            "properties": {
                "n": {"type": "integer", "description": "Number of misses to show (default 10)"}
            }
        }
    },
    {
        "name": "receipts",
        "description": "Show last n receipts from unified trace",
        "inputSchema": {
            "type": "object",
            "properties": {
                "n": {"type": "integer", "description": "Number of receipts to show (default 10)"}
            }
        }
    },
    {
        "name": "listen",
        "description": "Enable receipt stream (ring+print)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "trace",
        "description": "Toggle debug tracing on/off",
        "inputSchema": {"type": "object", "properties": {}}
    },

    # Action commands
    {
        "name": "dream",
        "description": "Trigger dream/consolidation cycle (replay misses, extract schemas)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "observe",
        "description": "Trigger observation cycle (scan regions, update metrics)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "compact",
        "description": "Compact condemned regions",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "reset",
        "description": "Reset counters (not knowledge)",
        "inputSchema": {"type": "object", "properties": {}}
    },

    # I/O commands
    {
        "name": "save",
        "description": "Save surface to file",
        "inputSchema": {
            "type": "object",
            "properties": {
                "file": {"type": "string", "description": "File path to save to"}
            },
            "required": ["file"]
        }
    },
    {
        "name": "load",
        "description": "Load surface from file",
        "inputSchema": {
            "type": "object",
            "properties": {
                "file": {"type": "string", "description": "File path to load from"}
            },
            "required": ["file"]
        }
    },
    {
        "name": "eat",
        "description": "Digest file as food (extract tokens, learn patterns)",
        "inputSchema": {
            "type": "object",
            "properties": {
                "file": {"type": "string", "description": "File path to digest"}
            },
            "required": ["file"]
        }
    },

    # Hive/colony commands
    {
        "name": "hive",
        "description": "Show hive pheromone levels (swarm intelligence)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "share",
        "description": "Enable shared VSA (Mycorrhiza collective consciousness)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "colony",
        "description": "Show colony status (hive mind instances)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "export",
        "description": "Export region as .gene file (spore)",
        "inputSchema": {
            "type": "object",
            "properties": {
                "region": {"type": "integer", "description": "Region number to export"}
            },
            "required": ["region"]
        }
    },
    {
        "name": "import_gene",
        "description": "Import .gene file (infect with culture)",
        "inputSchema": {
            "type": "object",
            "properties": {
                "file": {"type": "string", "description": ".gene file to import"}
            },
            "required": ["file"]
        }
    },

    # Geometry/verification
    {
        "name": "geom",
        "description": "Show/set geometric gate mode (Rosetta Stone verification)",
        "inputSchema": {
            "type": "object",
            "properties": {
                "mode": {"type": "string", "description": "Mode to set (optional)"}
            }
        }
    },

    # Web fetch for digestion
    {
        "name": "web_fetch",
        "description": "Fetch URL content, optionally feed to UHMA for digestion",
        "inputSchema": {
            "type": "object",
            "properties": {
                "url": {"type": "string", "description": "URL to fetch"},
                "digest": {"type": "boolean", "description": "If true, feed content to UHMA (default false)"}
            },
            "required": ["url"]
        }
    },

    # Raw command (escape hatch)
    {
        "name": "raw",
        "description": "Send raw command to UHMA REPL",
        "inputSchema": {
            "type": "object",
            "properties": {
                "command": {"type": "string", "description": "Raw command text"}
            },
            "required": ["command"]
        }
    },

    # Lifecycle
    {
        "name": "quit",
        "description": "Gracefully shutdown UHMA (saves surface)",
        "inputSchema": {"type": "object", "properties": {}}
    },

    # =========================================================================
    # Holographic Memory (Claude's cognitive layer)
    # =========================================================================
    {
        "name": "mem_add",
        "description": "Add entry to holographic memory (finding, failed, success, insight, warning, todo)",
        "inputSchema": {
            "type": "object",
            "properties": {
                "category": {"type": "string", "description": "Category: finding, failed, success, insight, warning, todo, question, location"},
                "content": {"type": "string", "description": "What to remember"},
                "context": {"type": "string", "description": "What were you doing when you learned this?"},
                "source": {"type": "string", "description": "File:line or tool name (optional)"}
            },
            "required": ["category", "content"]
        }
    },
    {
        "name": "mem_query",
        "description": "Query holographic memory by semantic similarity",
        "inputSchema": {
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "What to search for"},
                "category": {"type": "string", "description": "Limit to category (optional)"},
                "limit": {"type": "integer", "description": "Max results (default 10)"}
            },
            "required": ["query"]
        }
    },
    {
        "name": "mem_outcome",
        "description": "Record outcome for a memory entry (worked or failed)",
        "inputSchema": {
            "type": "object",
            "properties": {
                "entry_id": {"type": "string", "description": "Entry ID"},
                "worked": {"type": "boolean", "description": "True if it worked, False if failed"}
            },
            "required": ["entry_id", "worked"]
        }
    },
    {
        "name": "mem_failed",
        "description": "Get failed approaches (to avoid repeating mistakes)",
        "inputSchema": {
            "type": "object",
            "properties": {
                "context": {"type": "string", "description": "Current context (optional)"},
                "limit": {"type": "integer", "description": "Max results (default 10)"}
            }
        }
    },
    {
        "name": "mem_success",
        "description": "Get successful approaches (to reuse what worked)",
        "inputSchema": {
            "type": "object",
            "properties": {
                "context": {"type": "string", "description": "Current context (optional)"},
                "limit": {"type": "integer", "description": "Max results (default 10)"}
            }
        }
    },
    {
        "name": "mem_state",
        "description": "Get cognitive state (confidence, confusion, warnings)",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "mem_summary",
        "description": "Get holographic memory summary",
        "inputSchema": {"type": "object", "properties": {}}
    },
    {
        "name": "mem_recent",
        "description": "Get recent memory entries",
        "inputSchema": {
            "type": "object",
            "properties": {
                "limit": {"type": "integer", "description": "Max results (default 10)"}
            }
        }
    },
    {
        "name": "mem_ison",
        "description": "Export relevant memories as ISON (for context injection)",
        "inputSchema": {
            "type": "object",
            "properties": {
                "context": {"type": "string", "description": "Current context to match against"},
                "limit": {"type": "integer", "description": "Max entries (default 10)"}
            }
        }
    }
]


def handle_tool_call(name, args):
    """Execute a tool and return result."""

    # Simple commands (no args)
    simple_cmds = ['help', 'status', 'self', 'metacog', 'debugger', 'genes',
                   'subroutines', 'regions', 'presence', 'drives', 'intro', 'why',
                   'listen', 'trace', 'dream', 'observe', 'compact', 'reset',
                   'hive', 'share', 'colony', 'quit']

    if name in simple_cmds:
        return send_to_uhma(name)

    # Commands with optional/required args
    if name == 'input':
        return send_to_uhma(args.get('text', ''))

    if name == 'misses':
        n = args.get('n', 10)
        return send_to_uhma(f"misses {n}")

    if name == 'receipts':
        n = args.get('n', 10)
        return send_to_uhma(f"receipts {n}")

    if name == 'save':
        return send_to_uhma(f"save {args['file']}")

    if name == 'load':
        return send_to_uhma(f"load {args['file']}")

    if name == 'eat':
        # Async - return immediately, output streams via trace channel
        send_to_uhma_async(f"eat {args['file']}")
        return f"Digesting {args['file']} - output streaming via trace channel"

    if name == 'export':
        return send_to_uhma(f"export {args['region']}")

    if name == 'import_gene':
        return send_to_uhma(f"import {args['file']}")

    if name == 'geom':
        mode = args.get('mode', '')
        return send_to_uhma(f"geom {mode}".strip())

    if name == 'raw':
        return send_to_uhma(args['command'])

    if name == 'web_fetch':
        url = args['url']
        content = web_fetch(url)

        if args.get('digest', False):
            # Write to temp file and digest
            import tempfile
            with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
                f.write(content)
                tmp_path = f.name
            result = send_to_uhma(f"eat {tmp_path}")
            return f"Fetched {len(content)} chars from {url}\n\n{result}"
        else:
            return content

    # =========================================================================
    # Holographic Memory Tools
    # =========================================================================
    if name == 'mem_add':
        from holo_memory import HoloMemory
        mem = HoloMemory()
        entry = mem.add(
            category=args['category'],
            content=args['content'],
            context=args.get('context'),
            source=args.get('source')
        )
        return f"Added [{entry.category}] {entry.id}: {entry.content[:80]}"

    if name == 'mem_query':
        from holo_memory import HoloMemory
        mem = HoloMemory()
        results = mem.query(
            query_text=args['query'],
            category=args.get('category'),
            limit=args.get('limit', 10)
        )
        if not results:
            return "No matching memories found"
        lines = []
        for entry, sim in results:
            outcome = "✓" if entry.outcome else ("✗" if entry.outcome is False else "?")
            lines.append(f"[{sim:.2f}] [{entry.category}] {outcome} {entry.content[:100]}")
            lines.append(f"       id={entry.id}")
        return "\n".join(lines)

    if name == 'mem_outcome':
        from holo_memory import HoloMemory
        mem = HoloMemory()
        success = mem.outcome(args['entry_id'], args['worked'])
        if success:
            return f"Recorded outcome: {'SUCCESS' if args['worked'] else 'FAILURE'} for {args['entry_id']}"
        return f"Entry not found: {args['entry_id']}"

    if name == 'mem_failed':
        from holo_memory import HoloMemory
        mem = HoloMemory()
        failed = mem.failed_approaches(
            context=args.get('context'),
            limit=args.get('limit', 10)
        )
        if not failed:
            return "No failed approaches recorded"
        lines = ["FAILED APPROACHES (don't repeat these):"]
        for entry in failed:
            lines.append(f"- {entry.content[:150]}")
            if entry.context:
                lines.append(f"  context: {entry.context[:80]}")
        return "\n".join(lines)

    if name == 'mem_success':
        from holo_memory import HoloMemory
        mem = HoloMemory()
        success = mem.successful_approaches(
            context=args.get('context'),
            limit=args.get('limit', 10)
        )
        if not success:
            return "No successful approaches recorded"
        lines = ["SUCCESSFUL APPROACHES (reuse these):"]
        for entry in success:
            lines.append(f"- {entry.content[:150]}")
            if entry.context:
                lines.append(f"  context: {entry.context[:80]}")
        return "\n".join(lines)

    if name == 'mem_state':
        from holo_memory import HoloMemory
        mem = HoloMemory()
        state = mem.get_state()
        lines = ["COGNITIVE STATE:"]
        lines.append(f"  Confidence: {state['confidence']:.2f}")
        lines.append(f"  Confusion:  {state['confusion']:.2f}")
        lines.append(f"  Progress:   {state['progress']:.2f}")
        lines.append(f"  Repetition: {state['repetition']:.2f}")
        lines.append(f"  Fatigue:    {state['fatigue']:.2f}")
        lines.append(f"  Entries:    {state['entry_count']}")
        lines.append(f"  Hit ratio:  {state['hit_ratio']:.2f}")
        warnings = state.get('warnings', [])
        if warnings:
            lines.append("\nWARNINGS:")
            for w in warnings:
                lines.append(f"  ! {w}")
        return "\n".join(lines)

    if name == 'mem_summary':
        from holo_memory import HoloMemory
        mem = HoloMemory()
        return mem.summary()

    if name == 'mem_recent':
        from holo_memory import HoloMemory
        mem = HoloMemory()
        recent = mem.recent(args.get('limit', 10))
        if not recent:
            return "No memory entries"
        lines = ["RECENT MEMORIES:"]
        for entry in recent:
            outcome = "✓" if entry.outcome else ("✗" if entry.outcome is False else "?")
            lines.append(f"[{entry.category}] {outcome} {entry.content[:100]}")
        return "\n".join(lines)

    if name == 'mem_ison':
        from holo_memory import HoloMemory
        mem = HoloMemory()
        return mem.to_ison(
            context=args.get('context'),
            limit=args.get('limit', 10)
        )

    return f"Unknown tool: {name}"


def handle_request(request):
    method = request.get("method")
    id = request.get("id")
    params = request.get("params", {})

    if method == "initialize":
        # Echo back the client's protocol version for compatibility
        client_version = params.get("protocolVersion", "2024-11-05")
        send_response(id, {
            "protocolVersion": client_version,
            "capabilities": {"tools": {}},
            "serverInfo": {
                "name": "uhma-control",
                "version": "2.0.0"
            }
        })

    elif method == "tools/list":
        send_response(id, {"tools": TOOLS})

    elif method == "tools/call":
        tool_name = params.get("name")
        args = params.get("arguments", {})

        try:
            result = handle_tool_call(tool_name, args)
            send_response(id, {"content": [{"type": "text", "text": result}]})
        except Exception as e:
            send_error(id, -32000, str(e))

    elif method == "notifications/initialized":
        pass  # No response needed

    elif method == "ping":
        # Health check - respond immediately
        send_response(id, {})

    else:
        if id is not None:
            send_error(id, -32601, f"Unknown method: {method}")


def log(msg):
    """Debug logging to file"""
    with open("/tmp/uhma_mcp_debug.log", "a") as f:
        f.write(f"{time.time()}: {msg}\n")
        f.flush()


# TCP server for GUI connections
TCP_PORT = 9999
tcp_clients = []
tcp_lock = threading.Lock()


def send_tcp_response(client_socket, id, result):
    """Send JSON-RPC response to TCP client."""
    response = {"jsonrpc": "2.0", "id": id, "result": result}
    msg = json.dumps(response) + "\n"
    try:
        client_socket.sendall(msg.encode())
    except:
        pass


def send_tcp_error(client_socket, id, code, message):
    """Send JSON-RPC error to TCP client."""
    response = {"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message}}
    msg = json.dumps(response) + "\n"
    try:
        client_socket.sendall(msg.encode())
    except:
        pass


def handle_tcp_request(client_socket, request):
    """Handle a request from TCP client (GUI)."""
    method = request.get("method")
    id = request.get("id")
    params = request.get("params", {})

    if method == "initialize":
        client_version = params.get("protocolVersion", "2024-11-05")
        send_tcp_response(client_socket, id, {
            "protocolVersion": client_version,
            "capabilities": {"tools": {}},
            "serverInfo": {"name": "uhma-control", "version": "2.0.0"}
        })

    elif method == "tools/list":
        send_tcp_response(client_socket, id, {"tools": TOOLS})

    elif method == "tools/call":
        tool_name = params.get("name")
        args = params.get("arguments", {})
        try:
            result = handle_tool_call(tool_name, args)
            send_tcp_response(client_socket, id, {"content": [{"type": "text", "text": result}]})
        except Exception as e:
            send_tcp_error(client_socket, id, -32000, str(e))

    elif method == "ping":
        send_tcp_response(client_socket, id, {})


def handle_tcp_client(client_socket, addr):
    """Handle a single TCP client connection."""
    log(f"TCP client connected from {addr}")
    buffer = ""

    try:
        while True:
            data = client_socket.recv(4096)
            if not data:
                break
            buffer += data.decode()

            # Process complete JSON lines
            while "\n" in buffer:
                line, buffer = buffer.split("\n", 1)
                line = line.strip()
                if not line:
                    continue

                # Handle Content-Length format
                if line.startswith("Content-Length:"):
                    content_length = int(line.split(":", 1)[1].strip())
                    # Read until we have blank line + content
                    while "\r\n\r\n" not in buffer and "\n\n" not in buffer:
                        more = client_socket.recv(4096)
                        if not more:
                            return
                        buffer += more.decode()
                    # Skip blank line
                    if "\r\n\r\n" in buffer:
                        _, buffer = buffer.split("\r\n\r\n", 1)
                    else:
                        _, buffer = buffer.split("\n\n", 1)
                    # Read content
                    while len(buffer) < content_length:
                        more = client_socket.recv(4096)
                        if not more:
                            return
                        buffer += more.decode()
                    content = buffer[:content_length]
                    buffer = buffer[content_length:]
                    try:
                        request = json.loads(content)
                        handle_tcp_request(client_socket, request)
                    except json.JSONDecodeError:
                        pass
                    continue

                # Handle raw JSON
                if line.startswith("{"):
                    try:
                        request = json.loads(line)
                        handle_tcp_request(client_socket, request)
                    except json.JSONDecodeError:
                        pass
    except Exception as e:
        log(f"TCP client error: {e}")
    finally:
        log(f"TCP client disconnected from {addr}")
        client_socket.close()


def tcp_server_thread():
    """Background thread running TCP server for GUI connections."""
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    try:
        server.bind(("127.0.0.1", TCP_PORT))
        server.listen(5)
        log(f"TCP server listening on port {TCP_PORT}")

        while True:
            client_socket, addr = server.accept()
            client_thread = threading.Thread(
                target=handle_tcp_client,
                args=(client_socket, addr),
                daemon=True
            )
            client_thread.start()
    except Exception as e:
        log(f"TCP server error: {e}")


def main():
    sys.stderr.write("UHMA MCP server starting...\n")
    sys.stderr.flush()
    log("Server starting")

    # Start TCP server for GUI connections in background
    tcp_thread = threading.Thread(target=tcp_server_thread, daemon=True)
    tcp_thread.start()
    log(f"TCP server started on port {TCP_PORT}")

    # Main loop handles stdio for Claude Code
    while True:
        try:
            log("Waiting for input...")
            line = sys.stdin.readline()
            log(f"Got line: {repr(line[:100] if len(line) > 100 else line)}")
            if not line:
                log("EOF received")
                return  # EOF

            line = line.strip()
            if not line:
                continue  # Skip empty lines

            # Check if this is raw JSON (Claude Code sends JSON-RPC without Content-Length headers)
            if line.startswith("{"):
                global _use_content_length
                _use_content_length = False  # Switch to raw JSON mode
                log("Raw JSON detected, switching to JSON-line mode")
                request = json.loads(line)
                log(f"Parsed request: {request.get('method')}")
                handle_request(request)
                log("Request handled")
                continue

            # Otherwise try Content-Length header format
            if line.startswith("Content-Length:"):
                content_length = int(line.split(":", 1)[1].strip())
                log(f"Content-Length: {content_length}")
                # Read blank line
                sys.stdin.readline()
                # Read content
                content = sys.stdin.read(content_length)
                log(f"Got content: {content[:100]}...")
                if content:
                    request = json.loads(content)
                    log(f"Parsed request: {request.get('method')}")
                    handle_request(request)
                    log("Request handled")
        except json.JSONDecodeError as e:
            # Log but don't crash on malformed JSON
            sys.stderr.write(f"JSON decode error: {e}\n")
            sys.stderr.flush()
        except Exception as e:
            # Log unexpected errors but keep running
            sys.stderr.write(f"Server error: {e}\n")
            sys.stderr.flush()


def tcp_only_main(port=TCP_PORT):
    """Run as TCP-only server (for GUI standalone mode)."""
    global TCP_PORT
    TCP_PORT = port
    sys.stderr.write(f"UHMA MCP server (TCP-only) starting on port {port}...\n")
    sys.stderr.flush()
    log(f"TCP-only server starting on port {port}")

    # Start UHMA immediately
    start_uhma()

    # Run TCP server in foreground
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    try:
        server.bind(("127.0.0.1", port))
        server.listen(5)
        log(f"TCP server listening on port {port}")
        sys.stderr.write(f"TCP server listening on port {port}\n")

        while True:
            client_socket, addr = server.accept()
            client_thread = threading.Thread(
                target=handle_tcp_client,
                args=(client_socket, addr),
                daemon=True
            )
            client_thread.start()
    except KeyboardInterrupt:
        log("Server shutting down")
    except Exception as e:
        log(f"TCP server error: {e}")
        sys.stderr.write(f"TCP server error: {e}\n")


def handle_feed_client(client_socket, addr):
    """Handle feed port client - async send, no wait."""
    log(f"Feed client connected from {addr}")
    try:
        client_socket.settimeout(None)
        data = b""
        while True:
            chunk = client_socket.recv(4096)
            if not chunk:
                break
            data += chunk
            if b"\n" in data:
                break

        for line in data.decode('utf-8', errors='replace').split('\n'):
            line = line.strip()
            if not line:
                continue
            try:
                request = json.loads(line)
                method = request.get("method", "")
                params = request.get("params", {})
                req_id = request.get("id", 1)

                if method == "tools/call":
                    tool_name = params.get("name", "")
                    args = params.get("arguments", {})

                    if tool_name == "raw":
                        cmd = args.get("command", "")
                        result = send_to_uhma_async(cmd)
                    elif tool_name in ["eat", "dream", "observe", "compact"]:
                        if tool_name == "eat":
                            cmd = f"eat {args.get('path', args.get('file', ''))}"
                        else:
                            cmd = tool_name
                        result = send_to_uhma_async(cmd)
                    else:
                        result = f"Feed port only accepts: raw, eat, dream, observe, compact"

                    response = {"jsonrpc": "2.0", "id": req_id, "result": {"content": [{"type": "text", "text": result}]}}
                    client_socket.sendall((json.dumps(response) + "\n").encode())
            except json.JSONDecodeError:
                pass
    except Exception as e:
        log(f"Feed client error: {e}")
    finally:
        client_socket.close()


def tcp_dual_main(feed_port, query_port):
    """Run with two TCP ports: feed_port (async) for feeding, query_port (sync) for queries."""
    start_uhma()

    feed_server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    feed_server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    query_server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    query_server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

    try:
        feed_server.bind(("127.0.0.1", feed_port))
        feed_server.listen(5)
        query_server.bind(("127.0.0.1", query_port))
        query_server.listen(5)
        log(f"Dual TCP: feed={feed_port} (async), query={query_port} (sync)")
        sys.stderr.write(f"Dual TCP: feed={feed_port} (async), query={query_port} (sync)\n")

        feed_server.setblocking(False)
        query_server.setblocking(False)

        while True:
            readable, _, _ = select.select([feed_server, query_server], [], [], 1.0)
            for sock in readable:
                client_socket, addr = sock.accept()
                if sock is feed_server:
                    # Feed port: async, fire and forget
                    client_thread = threading.Thread(
                        target=handle_feed_client,
                        args=(client_socket, addr),
                        daemon=True
                    )
                else:
                    # Query port: sync, wait for response
                    client_thread = threading.Thread(
                        target=handle_tcp_client,
                        args=(client_socket, addr),
                        daemon=True
                    )
                client_thread.start()
    except KeyboardInterrupt:
        log("Server shutting down")
    except Exception as e:
        log(f"Dual TCP error: {e}")
        sys.stderr.write(f"Dual TCP error: {e}\n")


def handle_trace_client(client_socket, addr):
    """Handle trace channel - streams receipts/debug output."""
    log(f"Trace client connected from {addr}")
    trace_clients.append(client_socket)
    try:
        # Keep connection alive, just echo back any input
        while True:
            data = client_socket.recv(1024)
            if not data:
                break
    except:
        pass
    finally:
        if client_socket in trace_clients:
            trace_clients.remove(client_socket)
        client_socket.close()
        log(f"Trace client disconnected from {addr}")


def handle_stream_client(client_socket, addr):
    """Handle stream channel - real-time event stream."""
    log(f"Stream client connected from {addr}")
    stream_clients.append(client_socket)
    try:
        while True:
            data = client_socket.recv(1024)
            if not data:
                break
    except:
        pass
    finally:
        if client_socket in stream_clients:
            stream_clients.remove(client_socket)
        client_socket.close()
        log(f"Stream client disconnected from {addr}")


def handle_llm_client(client_socket, addr):
    """Handle LLM channel - bidirectional with another UHMA or LLM."""
    log(f"LLM client connected from {addr}")
    try:
        client_socket.settimeout(None)  # No timeout for LLM channel
        buffer = ""
        while True:
            data = client_socket.recv(4096)
            if not data:
                break
            buffer += data.decode('utf-8', errors='replace')

            while "\n" in buffer:
                line, buffer = buffer.split("\n", 1)
                line = line.strip()
                if not line:
                    continue

                try:
                    request = json.loads(line)
                    method = request.get("method", "")
                    params = request.get("params", {})
                    req_id = request.get("id", 1)

                    if method == "tools/call":
                        tool_name = params.get("name", "")
                        args = params.get("arguments", {})
                        result = handle_tool_call(tool_name, args)
                        response = {"jsonrpc": "2.0", "id": req_id,
                                    "result": {"content": [{"type": "text", "text": result}]}}
                        client_socket.sendall((json.dumps(response) + "\n").encode())
                    elif method == "initialize":
                        response = {"jsonrpc": "2.0", "id": req_id,
                                    "result": {"protocolVersion": "2024-11-05",
                                               "capabilities": {"tools": {}},
                                               "serverInfo": {"name": "uhma-llm", "version": "1.0"}}}
                        client_socket.sendall((json.dumps(response) + "\n").encode())
                except json.JSONDecodeError:
                    pass
    except Exception as e:
        log(f"LLM client error: {e}")
    finally:
        client_socket.close()
        log(f"LLM client disconnected from {addr}")


# Channel client lists for broadcasting
trace_clients = []
stream_clients = []


def broadcast_trace(msg):
    """Send message to all trace channel clients."""
    for client in trace_clients[:]:
        try:
            client.sendall((msg + "\n").encode())
        except:
            trace_clients.remove(client)


def broadcast_stream(event):
    """Send event to all stream channel clients."""
    for client in stream_clients[:]:
        try:
            client.sendall((json.dumps(event) + "\n").encode())
        except:
            stream_clients.remove(client)


def tcp_multi_main(num_channels, base_port):
    """Run with 6 TCP channels:

    Channel 0 (9999): FEED   - async fire-and-forget (eat, dream, observe)
    Channel 1 (9998): QUERY  - sync request/response (status, why, misses)
    Channel 2 (9997): TRACE  - receipts/debug output stream
    Channel 3 (9996): STREAM - real-time event stream
    Channel 4 (9995): LLM    - bidirectional UHMA↔LLM communication
    Channel 5 (9994): RESERVED
    """
    start_uhma()

    CHANNEL_NAMES = ['FEED', 'QUERY', 'TRACE', 'STREAM', 'LLM', 'RESERVED']

    servers = []
    for i in range(num_channels):
        port = base_port - i
        server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server.bind(("127.0.0.1", port))
        server.listen(5)
        server.setblocking(False)
        name = CHANNEL_NAMES[i] if i < len(CHANNEL_NAMES) else f'CH{i}'
        servers.append((server, port, i, name))
        log(f"Channel {i} ({name}) listening on port {port}")

    ports_str = ", ".join([f"{name}:{p}" for s, p, i, name in servers])
    sys.stderr.write(f"6-channel MCP: {ports_str}\n")

    # Channel handlers by index
    channel_handlers = {
        0: handle_feed_client,    # FEED - async
        1: handle_tcp_client,     # QUERY - sync
        2: handle_trace_client,   # TRACE - stream
        3: handle_stream_client,  # STREAM - events
        4: handle_llm_client,     # LLM - bidirectional
        5: handle_tcp_client,     # RESERVED - default sync
    }

    try:
        while True:
            server_socks = [s for s, p, i, n in servers]
            readable, _, _ = select.select(server_socks, [], [], 1.0)
            for sock in readable:
                # Find which channel this is
                channel = None
                for s, p, i, n in servers:
                    if s is sock:
                        channel = i
                        break

                client_socket, addr = sock.accept()
                handler = channel_handlers.get(channel, handle_tcp_client)

                client_thread = threading.Thread(
                    target=handler,
                    args=(client_socket, addr),
                    daemon=True
                )
                client_thread.start()
    except KeyboardInterrupt:
        log("Server shutting down")
    except Exception as e:
        log(f"Multi TCP error: {e}")
        sys.stderr.write(f"Multi TCP error: {e}\n")
    finally:
        for s, p, i, n in servers:
            s.close()


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--tcp":
        # TCP-only mode for GUI
        port = int(sys.argv[2]) if len(sys.argv) > 2 else TCP_PORT
        tcp_only_main(port)
    elif len(sys.argv) > 1 and sys.argv[1] == "--dual":
        # Dual port mode: --dual FEED_PORT QUERY_PORT
        feed_port = int(sys.argv[2]) if len(sys.argv) > 2 else 9999
        query_port = int(sys.argv[3]) if len(sys.argv) > 3 else 9998
        tcp_dual_main(feed_port, query_port)
    elif len(sys.argv) > 1 and sys.argv[1] == "--multi":
        # Multi-channel mode: --multi NUM_CHANNELS BASE_PORT
        num_channels = int(sys.argv[2]) if len(sys.argv) > 2 else 4
        base_port = int(sys.argv[3]) if len(sys.argv) > 3 else 9999
        tcp_multi_main(num_channels, base_port)
    else:
        # Standard MCP mode (stdio + TCP background)
        main()
