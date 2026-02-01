#!/usr/bin/env python3
"""
hub_gemini.py — Gemini connector for multi-agent hub

@entry python3 hub_gemini.py           Start interactive client
@entry python3 hub_gemini.py --daemon  Run in background (no stdin)
@entry get_api_key() -> str            Get key from env/config/default
@entry connect_hub() -> socket         Connect to hub port 7777, register as "gemini"
@entry call_gemini_api(prompt, key) -> str   Direct HTTPS call to Gemini API

@calls hub (TCP port 7777)
@calls Gemini API (HTTPS to generativelanguage.googleapis.com)
@calledby user CLI, hub message routing

PROTOCOL:
  1. Connect to hub port 7777
  2. Send "HELLO gemini" to register
  3. Receive "FROM sender: message" for incoming requests
  4. Call Gemini API directly, send "@sender response" back

API ENDPOINT:
  https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent

GOTCHAS:
  - Has DEFAULT_API_KEY hardcoded (may be invalid/expired - check if errors)
  - Accepts GEMINI_API_KEY or GOOGLE_API_KEY env vars
  - Also checks ~/.gemini/api_key config file
  - --daemon flag runs without stdin (for background operation)
  - Uses urllib directly (no requests library dependency)
  - 60 second timeout on API calls
"""

import os
import sys
import socket
import select
import subprocess
import threading

HUB_HOST = "127.0.0.1"
HUB_PORT = 7777
CLIENT_NAME = "gemini"

# Default API key (can be overridden by env var or config file)
DEFAULT_API_KEY = "AIzaSyDO9nVLzbx8nX14cdbhBVnIIaIgT2fYauc"

def get_api_key():
    """Get API key from environment, config file, or use default."""
    # Check multiple env var names
    for var in ["GEMINI_API_KEY", "GOOGLE_API_KEY"]:
        key = os.environ.get(var)
        if key:
            print(f"[{CLIENT_NAME}] Using {var} from environment")
            return key

    # Check config file
    config_path = os.path.expanduser("~/.gemini/api_key")
    if os.path.exists(config_path):
        with open(config_path) as f:
            key = f.read().strip()
            if key:
                print(f"[{CLIENT_NAME}] Using API key from {config_path}")
                return key

    # Use default key
    if DEFAULT_API_KEY:
        print(f"[{CLIENT_NAME}] Using default API key")
        return DEFAULT_API_KEY

    # Prompt user
    print(f"[{CLIENT_NAME}] No API key found.")
    print("Enter your Google/Gemini API key (or set GEMINI_API_KEY env var):")
    key = input("> ").strip()

    if not key:
        print("No API key provided. Exiting.")
        sys.exit(1)

    return key

def connect_hub():
    """Connect to hub and register."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        sock.connect((HUB_HOST, HUB_PORT))
        sock.setblocking(False)
        # Register
        sock.send(f"HELLO {CLIENT_NAME}\n".encode())
        return sock
    except ConnectionRefusedError:
        print(f"[{CLIENT_NAME}] Hub not running on {HUB_HOST}:{HUB_PORT}")
        print("Start the hub first: ./hub")
        sys.exit(1)

def call_gemini(prompt, api_key):
    """Call Gemini API directly."""
    # Use direct API call (cleaner than CLI which has hook output)
    return call_gemini_api(prompt, api_key)

def call_gemini_api(prompt, api_key):
    """Direct API call to Gemini."""
    import json
    import urllib.request
    import urllib.error

    url = f"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key={api_key}"

    data = {
        "contents": [{"parts": [{"text": prompt}]}]
    }

    req = urllib.request.Request(
        url,
        data=json.dumps(data).encode(),
        headers={"Content-Type": "application/json"}
    )

    try:
        with urllib.request.urlopen(req, timeout=60) as resp:
            result = json.loads(resp.read().decode())
            return result["candidates"][0]["content"]["parts"][0]["text"]
    except urllib.error.HTTPError as e:
        return f"API Error: {e.code} {e.reason}"
    except Exception as e:
        return f"Error: {e}"

def main():
    print(f"=== Hub Client: {CLIENT_NAME} ===")

    # Get API key
    api_key = get_api_key()

    # Connect to hub
    sock = connect_hub()
    print(f"[{CLIENT_NAME}] Connected to hub")

    running = True

    def process_messages():
        """Process incoming hub messages."""
        buffer = ""
        while running:
            try:
                ready, _, _ = select.select([sock], [], [], 0.5)
                if ready:
                    data = sock.recv(4096)
                    if not data:
                        print(f"[{CLIENT_NAME}] Disconnected from hub")
                        break
                    buffer += data.decode()

                    while "\n" in buffer:
                        line, buffer = buffer.split("\n", 1)
                        line = line.strip()
                        if not line:
                            continue

                        print(f"[hub] {line}")

                        # Handle FROM messages (commands from other agents)
                        if line.startswith("FROM "):
                            # Parse: FROM sender: message
                            rest = line[5:]
                            if ": " in rest:
                                sender, msg = rest.split(": ", 1)
                                print(f"[{CLIENT_NAME}] Processing request from {sender}: {msg}")

                                # Call Gemini
                                response = call_gemini(msg, api_key)

                                # Send response back (directed to sender)
                                reply = f"@{sender} {response}\n"
                                sock.send(reply.encode())
                                print(f"[{CLIENT_NAME}] Sent response to {sender}")

            except Exception as e:
                if running:
                    print(f"[{CLIENT_NAME}] Error: {e}")
                break

    # Start message processor in background
    processor = threading.Thread(target=process_messages, daemon=True)
    processor.start()

    # Interactive mode - allow sending messages
    print(f"\n[{CLIENT_NAME}] Ready. Commands:")
    print("  @name message  - send to specific agent")
    print("  * message      - broadcast to all")
    print("  quit           - exit")
    print()

    try:
        while running:
            try:
                line = input()
                if line.lower() == "quit":
                    break
                if line:
                    sock.send(f"{line}\n".encode())
            except EOFError:
                break
    except KeyboardInterrupt:
        pass
    finally:
        running = False
        sock.close()
        print(f"\n[{CLIENT_NAME}] Disconnected")

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--daemon", "-d", action="store_true", help="Run in daemon mode (no stdin)")
    args = parser.parse_args()

    if args.daemon:
        # Daemon mode - just process hub messages
        print(f"=== Hub Client: {CLIENT_NAME} (daemon mode) ===")
        api_key = get_api_key()
        sock = connect_hub()
        print(f"[{CLIENT_NAME}] Connected to hub in daemon mode")

        buffer = ""
        try:
            while True:
                ready, _, _ = select.select([sock], [], [], 1.0)
                if ready:
                    data = sock.recv(4096)
                    if not data:
                        print(f"[{CLIENT_NAME}] Disconnected from hub")
                        break
                    buffer += data.decode()

                    while "\n" in buffer:
                        line, buffer = buffer.split("\n", 1)
                        line = line.strip()
                        if not line:
                            continue
                        print(f"[hub] {line}")

                        if line.startswith("FROM "):
                            rest = line[5:]
                            if ": " in rest:
                                sender, msg = rest.split(": ", 1)
                                print(f"[{CLIENT_NAME}] Processing: {msg}")
                                response = call_gemini(msg, api_key)
                                reply = f"@{sender} {response}\n"
                                sock.send(reply.encode())
                                print(f"[{CLIENT_NAME}] Replied to {sender}")
        except KeyboardInterrupt:
            pass
        finally:
            sock.close()
    else:
        main()
