#!/usr/bin/env python3
"""
server.py — MCP Server for UHMA command/control/communication.

@entry main() -> runs MCP protocol loop
@calls uhma binary via subprocess

FLOW: MCP client (Claude Code) → JSON-RPC → this server → UHMA stdin/stdout
CONFIG: Project-level .mcp.json in uhma-asm root (NOT ~/.claude/mcp.json)

TOOLS EXPOSED (27 total):
  - Input: input, raw
  - Status: help, status, self, metacog, debugger, genes, subroutines, regions, presence, drives
  - Debug: why, misses, receipts, listen, trace
  - Actions: dream, observe, compact, reset
  - I/O: save, load, eat
  - Hive: hive, share, colony, export, import_gene
  - Other: geom, web_fetch, quit

GOTCHAS:
  - MCP config must be at PROJECT_ROOT/.mcp.json, not ~/.claude/mcp.json
  - Claude Code must be restarted after adding/modifying .mcp.json
  - eat command uses 60s timeout (large files take time)
  - UHMA auto-spawns on first tool call if not running
"""

import json
import sys
import subprocess
import threading
import queue
import time
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


def send_to_uhma(text, timeout=5.0):
    """Send text to UHMA and collect response."""
    global uhma_process

    if not uhma_process or uhma_process.poll() is not None:
        if not start_uhma():
            return "Error: UHMA not running and failed to start"

    # Clear queue
    while not uhma_output_queue.empty():
        try:
            uhma_output_queue.get_nowait()
        except:
            break

    # Send input
    with uhma_lock:
        try:
            uhma_process.stdin.write(text + "\n")
            uhma_process.stdin.flush()
        except Exception as e:
            return f"Error sending to UHMA: {e}"

    # Collect output until prompt or timeout
    output_lines = []
    deadline = time.time() + timeout

    while time.time() < deadline:
        try:
            line = uhma_output_queue.get(timeout=0.1)
            output_lines.append(line.rstrip())
            # Check for prompt (indicates command complete)
            if line.strip().endswith("uhma>") or line.strip() == "uhma>":
                break
        except queue.Empty:
            continue

    return "\n".join(output_lines)


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
    }
]


def handle_tool_call(name, args):
    """Execute a tool and return result."""

    # Simple commands (no args)
    simple_cmds = ['help', 'status', 'self', 'metacog', 'debugger', 'genes',
                   'subroutines', 'regions', 'presence', 'drives', 'why',
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
        return send_to_uhma(f"eat {args['file']}", timeout=60.0)  # Longer timeout for digestion

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
            result = send_to_uhma(f"eat {tmp_path}", timeout=60.0)
            return f"Fetched {len(content)} chars from {url}\n\n{result}"
        else:
            return content

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


def main():
    sys.stderr.write("UHMA MCP server starting...\n")
    sys.stderr.flush()
    log("Server starting")

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


if __name__ == "__main__":
    main()
