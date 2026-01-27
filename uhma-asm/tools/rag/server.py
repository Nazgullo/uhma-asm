#!/usr/bin/env python3
"""
server.py — MCP Server for UHMA command/control/communication.

@entry main() -> runs MCP protocol loop (stdio for Claude Code)
@entry tcp_main() -> runs TCP server for GUI connections
@calls uhma binary via subprocess

ARCHITECTURE:
  GUI (TCP:9999) ─┐
                  ├──→ MCP Server ──→ UHMA (subprocess)
  Claude Code ────┘     (stdio)

FLOW:
  - Claude Code: stdin/stdout JSON-RPC (standard MCP)
  - GUI: TCP socket JSON-RPC on port 9999
  - Both share the same UHMA instance

CONFIG: Project-level .mcp.json in uhma-asm root (NOT ~/.claude/mcp.json)

TOOLS EXPOSED (28 total):
  - Input: input, raw
  - Status: help, status, self, metacog, intro, debugger, genes, subroutines, regions, presence, drives
  - Debug: why, misses, receipts, listen, trace
  - Actions: dream, observe, compact, reset
  - I/O: save, load, eat
  - Hive: hive, share, colony, export, import_gene
  - Other: geom, web_fetch, quit

NOTE: intro shows SELF-AWARE reading (0.0-1.0). Run observe first to build semantic self-model.

GOTCHAS:
  - MCP config must be at PROJECT_ROOT/.mcp.json, not ~/.claude/mcp.json
  - Claude Code must be restarted after adding/modifying .mcp.json
  - eat command uses 60s timeout (large files take time)
  - UHMA auto-spawns on first tool call if not running
  - GUI connects via TCP:9999, Claude Code via stdio
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


def send_to_uhma(text, timeout=10.0):
    """Send text to UHMA and collect response."""
    global uhma_process

    # Check if process died
    if uhma_process and uhma_process.poll() is not None:
        log(f"UHMA process died with code {uhma_process.returncode}, restarting...")
        uhma_process = None

    if not uhma_process:
        if not start_uhma():
            return "Error: UHMA not running and failed to start"
        # Give UHMA time to fully initialize and print banner
        time.sleep(1.5)
        # Drain the startup banner
        drained = 0
        while not uhma_output_queue.empty():
            try:
                uhma_output_queue.get_nowait()
                drained += 1
            except:
                break
        log(f"Drained {drained} startup lines")

    # Send input
    log(f"Sending to UHMA: {text}")
    with uhma_lock:
        try:
            uhma_process.stdin.write(text + "\n")
            uhma_process.stdin.flush()
        except Exception as e:
            log(f"Error sending: {e}")
            return f"Error sending to UHMA: {e}"

    # Collect output until prompt or timeout
    output_lines = []
    deadline = time.time() + timeout
    last_output_time = time.time()
    got_first_output = False

    while time.time() < deadline:
        try:
            line = uhma_output_queue.get(timeout=0.2)
            output_lines.append(line.rstrip())
            last_output_time = time.time()
            got_first_output = True
            log(f"Got line: {line.rstrip()[:60]}")
            # Check for prompt (indicates command complete)
            if line.strip().endswith("uhma>") or line.strip() == "uhma>":
                break
        except queue.Empty:
            # If we have output and haven't seen more for 0.5s, command is probably done
            if got_first_output and (time.time() - last_output_time) > 0.5:
                break
            # If we haven't seen any output for 2s, something's wrong
            if not got_first_output and (time.time() - last_output_time) > 2.0:
                log("No output received after 2s")
                break
            continue

    log(f"Collected {len(output_lines)} lines")
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


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--tcp":
        # TCP-only mode for GUI
        port = int(sys.argv[2]) if len(sys.argv) > 2 else TCP_PORT
        tcp_only_main(port)
    else:
        # Standard MCP mode (stdio + TCP background)
        main()
