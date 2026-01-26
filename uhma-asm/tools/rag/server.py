#!/usr/bin/env python3
"""
MCP Server for UHMA RAG context injection.

Exposes tools that Claude Code can call automatically:
- uhma_before_edit: Get context before editing a file
- uhma_before_call: Get context before calling a function
- uhma_search: Search the codebase
- uhma_gotchas: List all gotchas

To use: Add to Claude Code MCP settings.
"""

import json
import sys
from pathlib import Path

# MCP protocol uses JSON-RPC over stdio
def send_response(id, result):
    response = {"jsonrpc": "2.0", "id": id, "result": result}
    msg = json.dumps(response)
    sys.stdout.write(f"Content-Length: {len(msg)}\r\n\r\n{msg}")
    sys.stdout.flush()

def send_error(id, code, message):
    response = {"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message}}
    msg = json.dumps(response)
    sys.stdout.write(f"Content-Length: {len(msg)}\r\n\r\n{msg}")
    sys.stdout.flush()

# Import context functions
from context import (
    context_before_edit,
    context_before_call,
    context_search,
    get_all_gotchas,
    load_index
)

TOOLS = [
    {
        "name": "uhma_before_edit",
        "description": "Get context (gotchas, dependencies, entry points) before editing an UHMA .asm file. ALWAYS call this before editing any .asm file.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "filename": {
                    "type": "string",
                    "description": "The .asm filename (e.g., 'dispatch.asm' or just 'dispatch')"
                }
            },
            "required": ["filename"]
        }
    },
    {
        "name": "uhma_before_call",
        "description": "Get context for a function before calling it (signature, gotchas).",
        "inputSchema": {
            "type": "object",
            "properties": {
                "function": {
                    "type": "string",
                    "description": "The function name (e.g., 'process_token')"
                }
            },
            "required": ["function"]
        }
    },
    {
        "name": "uhma_search",
        "description": "Search UHMA codebase for files, functions, or concepts.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "Search query"
                }
            },
            "required": ["query"]
        }
    },
    {
        "name": "uhma_gotchas",
        "description": "List all gotchas across the UHMA codebase.",
        "inputSchema": {
            "type": "object",
            "properties": {}
        }
    }
]

def handle_request(request):
    method = request.get("method")
    id = request.get("id")
    params = request.get("params", {})

    if method == "initialize":
        send_response(id, {
            "protocolVersion": "2024-11-05",
            "capabilities": {"tools": {}},
            "serverInfo": {"name": "uhma-rag", "version": "1.0.0"}
        })

    elif method == "tools/list":
        send_response(id, {"tools": TOOLS})

    elif method == "tools/call":
        tool_name = params.get("name")
        args = params.get("arguments", {})

        try:
            if tool_name == "uhma_before_edit":
                result = context_before_edit(args.get("filename", ""))
            elif tool_name == "uhma_before_call":
                result = context_before_call(args.get("function", ""))
            elif tool_name == "uhma_search":
                result = context_search(args.get("query", ""))
            elif tool_name == "uhma_gotchas":
                result = get_all_gotchas()
            else:
                send_error(id, -32601, f"Unknown tool: {tool_name}")
                return

            send_response(id, {"content": [{"type": "text", "text": result}]})
        except Exception as e:
            send_error(id, -32000, str(e))

    elif method == "notifications/initialized":
        pass  # No response needed for notifications

    else:
        if id is not None:
            send_error(id, -32601, f"Unknown method: {method}")

def main():
    buffer = ""
    while True:
        # Read headers
        headers = {}
        while True:
            line = sys.stdin.readline()
            if not line:
                return  # EOF
            line = line.strip()
            if not line:
                break
            if ":" in line:
                key, value = line.split(":", 1)
                headers[key.strip()] = value.strip()

        # Read content
        content_length = int(headers.get("Content-Length", 0))
        if content_length > 0:
            content = sys.stdin.read(content_length)
            request = json.loads(content)
            handle_request(request)

if __name__ == "__main__":
    main()
