#!/usr/bin/env python3
"""Hub client for Claude CLI - connects Claude to the multi-agent hub."""

import os
import sys
import socket
import select
import subprocess
import threading
import queue

HUB_HOST = "127.0.0.1"
HUB_PORT = 7777
CLIENT_NAME = "claude"

def get_api_key():
    """Get API key from environment or prompt user."""
    key = os.environ.get("ANTHROPIC_API_KEY")
    if key:
        print(f"[{CLIENT_NAME}] Using ANTHROPIC_API_KEY from environment")
        return key

    # Check config file
    config_path = os.path.expanduser("~/.anthropic/api_key")
    if os.path.exists(config_path):
        with open(config_path) as f:
            key = f.read().strip()
            if key:
                print(f"[{CLIENT_NAME}] Using API key from {config_path}")
                return key

    # Prompt user
    print(f"[{CLIENT_NAME}] No API key found.")
    print("Enter your Anthropic API key (or set ANTHROPIC_API_KEY env var):")
    key = input("> ").strip()

    if not key:
        print("No API key provided. Exiting.")
        sys.exit(1)

    # Optionally save
    save = input("Save to ~/.anthropic/api_key? [y/N] ").strip().lower()
    if save == 'y':
        os.makedirs(os.path.dirname(config_path), exist_ok=True)
        with open(config_path, 'w') as f:
            f.write(key)
        os.chmod(config_path, 0o600)
        print(f"Saved to {config_path}")

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

def call_claude(prompt, api_key):
    """Call Claude CLI with a prompt."""
    env = os.environ.copy()
    env["ANTHROPIC_API_KEY"] = api_key

    try:
        # Use claude CLI in non-interactive mode
        result = subprocess.run(
            ["claude", "-p", prompt],
            capture_output=True,
            text=True,
            timeout=60,
            env=env
        )
        return result.stdout.strip() if result.returncode == 0 else f"Error: {result.stderr}"
    except subprocess.TimeoutExpired:
        return "Error: Claude request timed out"
    except FileNotFoundError:
        return "Error: claude CLI not found"

def main():
    print(f"=== Hub Client: {CLIENT_NAME} ===")

    # Get API key
    api_key = get_api_key()

    # Connect to hub
    sock = connect_hub()
    print(f"[{CLIENT_NAME}] Connected to hub")

    response_queue = queue.Queue()
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

                                # Call Claude
                                response = call_claude(msg, api_key)

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
    main()
