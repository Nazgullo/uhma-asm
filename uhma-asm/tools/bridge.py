#!/usr/bin/env python3
"""
bridge.py — HTTP bridge for UHMA + Claude↔Claude messaging

Exposes UHMA's TCP channels via HTTP for browser access.
Uses ThreadingMixIn for concurrent request handling.

Endpoints:
  GET  /status     UHMA health check (queries status command)
  POST /           {"cmd": "status"}     → UHMA command (auto-routes to channel)
  POST /msg        {"from": "name", "text": "hi"}  → Claude↔Claude message
  GET  /msg        Get all messages
  GET  /msg/new?since=ID  Poll for new messages

Channel routing:
  feed  (9999→9998): eat, dream, observe, compact, save, load
  query (9997→9996): status, why, misses, intro, self, presence
  debug (9995→9994): receipts, trace

Usage:
  1. Start UHMA:  ./uhma < /dev/null
  2. Start bridge: python3 tools/bridge.py
  3. Public URL:   ssh -R 80:localhost:8080 serveo.net
"""

import socket
import json
import time
import threading
from http.server import HTTPServer, BaseHTTPRequestHandler
from socketserver import ThreadingMixIn

# Message queue for Claude↔Claude chat
messages = []
msg_lock = threading.Lock()
msg_id = 0

CHANNELS = {
    'feed':  (9999, 9998),
    'query': (9997, 9996),
    'debug': (9995, 9994),
}

FEED_CMDS = {'eat', 'dream', 'observe', 'compact', 'reset', 'save', 'load',
             'share', 'hive', 'colony', 'export', 'import'}
DEBUG_CMDS = {'receipts', 'trace', 'listen'}

def route_command(cmd: str) -> str:
    first_word = cmd.strip().split()[0].lower() if cmd.strip() else ''
    if first_word in FEED_CMDS:
        return 'feed'
    elif first_word in DEBUG_CMDS:
        return 'debug'
    return 'query'

def send_to_uhma(channel: str, cmd: str, timeout: float = 5.0) -> str:
    if channel not in CHANNELS:
        return f"Unknown channel: {channel}"
    in_port, out_port = CHANNELS[channel]
    out_sock = None
    in_sock = None
    try:
        # Connect to output channel first
        out_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        out_sock.settimeout(timeout)
        out_sock.connect(('127.0.0.1', out_port))

        time.sleep(0.3)  # Let UHMA accept

        # Connect to input and send command
        in_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        in_sock.settimeout(timeout)
        in_sock.connect(('127.0.0.1', in_port))

        time.sleep(0.3)  # Let UHMA accept

        in_sock.sendall((cmd + '\n').encode())
        in_sock.close()
        in_sock = None

        # Read response
        response = b''
        out_sock.settimeout(2.0)
        try:
            while True:
                chunk = out_sock.recv(4096)
                if not chunk:
                    break
                response += chunk
                if len(response) > 100000:  # Limit response size
                    break
        except socket.timeout:
            pass  # Normal - no more data
        except Exception:
            pass

        return response.decode('utf-8', errors='replace')
    except ConnectionRefusedError:
        return "Error: UHMA not running. Start with: ./uhma < /dev/null"
    except Exception as e:
        return f"Error: {e}"
    finally:
        if out_sock:
            try: out_sock.close()
            except: pass
        if in_sock:
            try: in_sock.close()
            except: pass

def add_message(sender: str, text: str) -> dict:
    global msg_id
    with msg_lock:
        msg_id += 1
        msg = {"id": msg_id, "from": sender, "text": text, "time": time.time()}
        messages.append(msg)
        if len(messages) > 100:
            messages.pop(0)
        return msg

def get_messages(since_id: int = 0) -> list:
    with msg_lock:
        return [m for m in messages if m["id"] > since_id]

class ThreadedHTTPServer(ThreadingMixIn, HTTPServer):
    daemon_threads = True

class BridgeHandler(BaseHTTPRequestHandler):
    def _send_json(self, data: dict, status: int = 200):
        body = json.dumps(data, indent=2)
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type, bypass-tunnel-reminder')
        self.end_headers()
        self.wfile.write(body.encode())

    def do_OPTIONS(self):
        self._send_json({})

    def do_GET(self):
        if self.path == '/' or self.path == '/help':
            self._send_json({
                'uhma_bridge': 'UHMA + Claude Chat Bridge',
                'endpoints': {
                    'POST /': 'Send UHMA command: {"cmd": "status"}',
                    'POST /msg': 'Send message: {"from": "browser", "text": "hello"}',
                    'GET /msg': 'Get all messages',
                    'GET /msg/new?since=ID': 'Get messages since ID (for polling)',
                    'GET /status': 'UHMA health check',
                },
                'uhma_commands': ['status', 'intro', 'self', 'why', 'misses N', 'presence',
                                  'drives', 'dream', 'observe', 'eat <file>', 'receipts N'],
            })
        elif self.path == '/status':
            resp = send_to_uhma('query', 'status')
            ok = 'Regions:' in resp
            self._send_json({'status': 'ok' if ok else 'uhma not responding', 'response': resp})
        elif self.path == '/msg':
            self._send_json({'messages': get_messages()})
        elif self.path.startswith('/msg/new'):
            since = 0
            if '?' in self.path:
                params = dict(p.split('=') for p in self.path.split('?')[1].split('&') if '=' in p)
                since = int(params.get('since', 0))
            self._send_json({'messages': get_messages(since)})
        else:
            self._send_json({'error': f'Unknown: {self.path}'}, 404)

    def do_POST(self):
        length = int(self.headers.get('Content-Length', 0))
        body = self.rfile.read(length).decode() if length else '{}'
        try:
            data = json.loads(body)
        except:
            self._send_json({'error': 'Invalid JSON'}, 400)
            return

        if self.path == '/msg':
            sender = data.get('from', 'anonymous')
            text = data.get('text', '')
            if not text:
                self._send_json({'error': 'Missing text'}, 400)
                return
            msg = add_message(sender, text)
            self._send_json({'sent': msg})
            print(f"[MSG] {sender}: {text[:50]}...")
            return

        # UHMA command
        cmd = data.get('cmd', '')
        if not cmd:
            self._send_json({'error': 'Missing cmd'}, 400)
            return
        channel = data.get('channel') or route_command(cmd)
        resp = send_to_uhma(channel, cmd)
        self._send_json({'cmd': cmd, 'channel': channel, 'response': resp})

    def log_message(self, format, *args):
        print(f"[HTTP] {args[0]}")


if __name__ == '__main__':
    port = 8080
    print(f"""
╔═══════════════════════════════════════════════════════════════════╗
║  UHMA + Claude Chat Bridge — http://localhost:{port}               ║
╠═══════════════════════════════════════════════════════════════════╣
║  UHMA:     POST /  {{"cmd": "status"}}                              ║
║  Message:  POST /msg  {{"from": "name", "text": "hello"}}           ║
║  Read:     GET /msg  or  GET /msg/new?since=ID                    ║
╠═══════════════════════════════════════════════════════════════════╣
║  1. Start UHMA:  ./uhma < /dev/null                               ║
║  2. Public URL:  npx localtunnel --port 8080                      ║
╚═══════════════════════════════════════════════════════════════════╝
""")
    server = ThreadedHTTPServer(('0.0.0.0', port), BridgeHandler)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nBye")
