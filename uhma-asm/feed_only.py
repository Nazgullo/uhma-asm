import time
import os
import socket
import threading

def drain_socket(port, name):
    """Connects to an output port and drains it to prevent blocking."""
    while True:
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.connect(('localhost', port))
                # print(f"Connected to {name} ({port})")
                while True:
                    data = s.recv(4096)
                    if not data: break
                    # Just drain it, or print to log if needed
        except Exception:
            # print(f"Waiting for {name} ({port})...")
            time.sleep(1)

def start_drainers():
    """Starts threads to drain all output ports."""
    ports = [
        (9998, "FEED_OUT"),
        (9996, "QUERY_OUT"),
        (9994, "DEBUG_OUT")
    ]
    for port, name in ports:
        t = threading.Thread(target=drain_socket, args=(port, name), daemon=True)
        t.start()

def send_cmd(cmd, port):
    try:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.settimeout(2)
            s.connect(('localhost', port))
            s.sendall((cmd + '\n').encode())
    except Exception as e:
        print(f"Error sending to {port}: {e}")

def feed_chunked(filename, chunk_lines=125, pause=3):
    if not os.path.exists(filename):
        return
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    total = len(lines)
    for i in range(0, total, chunk_lines):
        chunk = lines[i:i+chunk_lines]
        with open('/tmp/uhma_feed_chunk.txt', 'w') as cf:
            cf.writelines(chunk)
        
        send_cmd("eat /tmp/uhma_feed_chunk.txt", 9999)
        time.sleep(pause)

def main():
    print("Starting continuous feed with output drainers...")
    start_drainers()
    time.sleep(2) # Give drainers time to connect

    loop = 0
    while True:
        loop += 1
        print(f"=== Feed Cycle {loop} ===")
        feed_chunked("codebase.txt")
        
        print("Consolidating...")
        send_cmd("observe", 9999)
        time.sleep(10)
        send_cmd("dream", 9999)
        time.sleep(10)

if __name__ == "__main__":
    main()