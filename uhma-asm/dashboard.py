#!/usr/bin/env python3
"""
UHMA Dashboard CLI
Connects to UHMA server (tools/rag/server.py --multi)
Provides non-blocking input and file feeding.
"""

import socket
import threading
import sys
import json
import time
import os
import queue

# Ports from server.py --multi defaults (Changed to avoid 9999 conflict)
FEED_PORT = 5555   # Async (fire and forget)
QUERY_PORT = 5554  # Sync (request/response)
TRACE_PORT = 5553  # Stream (debug output)

def connect_socket(port, name):
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect(('127.0.0.1', port))
        return s
    except ConnectionRefusedError:
        print(f"Could not connect to {name} on port {port}. Is 'python3 tools/rag/server.py --multi' running?")
        return None

def reader_thread(sock, name):
    """Reads lines from a socket and prints them."""
    buffer = ""
    while True:
        try:
            data = sock.recv(4096)
            if not data:
                break
            buffer += data.decode(errors='replace')
            while "\n" in buffer:
                line, buffer = buffer.split("\n", 1)
                # Try to parse JSON response if it looks like one
                try:
                    if line.strip().startswith('{'):
                        resp = json.loads(line)
                        if 'result' in resp and 'content' in resp['result']:
                            for item in resp['result']['content']:
                                if item['type'] == 'text':
                                    print(f"\n[{name}] {item['text']}")
                        elif 'error' in resp:
                            print(f"\n[{name} ERROR] {resp['error']['message']}")
                        else:
                            print(f"\n[{name}] {line}")
                    else:
                        print(f"\n[{name}] {line}")
                except:
                    print(f"\n[{name}] {line}")
                
                # Reprint prompt
                print("> ", end="", flush=True)
        except Exception:
            break

def feed_file(sock, filepath):
    """Feeds a file to the FEED port."""
    filepath = os.path.abspath(filepath)
    if not os.path.exists(filepath):
        print(f"File not found: {filepath}")
        return

    req = {
        "jsonrpc": "2.0",
        "method": "tools/call",
        "params": {
            "name": "eat",
            "arguments": {"file": filepath}
        },
        "id": int(time.time())
    }
    sock.sendall((json.dumps(req) + "\n").encode())
    print(f"Sent eat command for {filepath}")

def main():
    print("Connecting to UHMA server...")
    
    # We mainly need QUERY for commands and FEED for files
    query_sock = connect_socket(QUERY_PORT, "QUERY")
    if not query_sock:
        return

    feed_sock = connect_socket(FEED_PORT, "FEED")
    if not feed_sock:
        query_sock.close()
        return

    trace_sock = connect_socket(TRACE_PORT, "TRACE")
    if trace_sock:
        # Start reader for trace output (background digestion logs)
        threading.Thread(target=reader_thread, args=(trace_sock, "TRACE"), daemon=True).start()
    else:
        print("Warning: Could not connect to TRACE channel. You won't see background digestion output.")

    # Start reader for query responses
    threading.Thread(target=reader_thread, args=(query_sock, "UHMA"), daemon=True).start()

    print("Connected! Type commands (e.g., 'help', 'status').")
    print("Special commands:")
    print("  /feed <filename>  - Feed a text file to UHMA")
    print("  /quit             - Exit dashboard")
    print("> ", end="", flush=True)

    try:
        while True:
            line = sys.stdin.readline()
            if not line:
                break
            line = line.strip()
            if not line:
                print("> ", end="", flush=True)
                continue

            if line.lower() == "/quit":
                break
            
            if line.startswith("/feed "):
                path = line[6:].strip()
                feed_file(feed_sock, path)
                print("> ", end="", flush=True)
                continue

            if line.startswith("/feed_dir "):
                path = line[10:].strip()
                path = os.path.abspath(path)
                if not os.path.exists(path) or not os.path.isdir(path):
                    print(f"Directory not found: {path}")
                else:
                    count = 0
                    print(f"Scanning {path}...")
                    for root, dirs, files in os.walk(path):
                        # Skip hidden directories
                        dirs[:] = [d for d in dirs if not d.startswith('.')]
                        for file in files:
                            if file.startswith('.'): continue
                            full_path = os.path.join(root, file)
                            # Skip binary/large/irrelevant files based on extension
                            if file.endswith(('.pyc', '.o', '.bin', '.surface', '.zip', '.png', '.jpg')):
                                continue
                            feed_file(feed_sock, full_path)
                            count += 1
                            # Slight delay to avoid overwhelming the socket buffer if huge
                            time.sleep(0.01) 
                    print(f"Queued {count} files for digestion.")
                print("> ", end="", flush=True)
                continue

            # Default: send as tool call to QUERY port
            # If it's a known command, treat as tool. Otherwise raw input?
            # server.py 'input' tool takes 'text'. 
            # But simple commands like 'status' are also tools.
            
            # Simple heuristic: treat everything as a tool call to 'input' unless it matches a known command
            # But wait, server.py handles simple commands directly if name matches.
            # Let's try sending as 'input' tool for text, or raw command if it looks like one.
            
            # Actually, server.py's handle_tcp_client expects JSON-RPC.
            # Let's wrap it.
            
            cmd_name = line.split()[0]
            args = {}
            tool_name = "input"
            tool_args = {"text": line}

            # Map common commands to their tools
            simple_tools = ['help', 'status', 'self', 'regions', 'drives', 'dream', 'observe', 'save', 'load']
            if cmd_name in simple_tools:
                tool_name = cmd_name
                tool_args = {}
                # handle args for save/load
                if cmd_name in ['save', 'load'] and len(line.split()) > 1:
                    tool_args = {'file': line.split(maxsplit=1)[1]}

            req = {
                "jsonrpc": "2.0",
                "method": "tools/call",
                "params": {
                    "name": tool_name,
                    "arguments": tool_args
                },
                "id": int(time.time())
            }
            query_sock.sendall((json.dumps(req) + "\n").encode())

    except KeyboardInterrupt:
        print("\nExiting...")
    finally:
        query_sock.close()
        feed_sock.close()

if __name__ == "__main__":
    main()
