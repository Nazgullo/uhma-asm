#!/usr/bin/env python3
"""Simple UHMA trainer - keeps stdin alive to prevent idle loop"""
import subprocess
import time
import os
import signal
from datetime import datetime

CORPUS_DIR = "corpus"
CHUNK_LINES = 50
CHUNK_PAUSE = 3
ALERT_FILE = "ALERT_CLAUDE.txt"

def log(msg):
    print(f"[{datetime.now().strftime('%H:%M:%S')}] {msg}", flush=True)

def alert(msg):
    with open(ALERT_FILE, "a") as f:
        f.write(f"[{datetime.now()}] ALERT: {msg}\n")
    log(f"ALERT: {msg}")

def get_corpus_files():
    files = []
    for root, _, filenames in os.walk(CORPUS_DIR):
        for f in sorted(filenames):
            if f.endswith('.txt'):
                files.append(os.path.join(root, f))
    return files

def main():
    if os.path.exists(ALERT_FILE):
        os.remove(ALERT_FILE)

    log("Starting UHMA trainer")

    # Start UHMA
    proc = subprocess.Popen(
        ["./uhma"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        bufsize=0  # unbuffered
    )
    log(f"UHMA PID {proc.pid}")

    def send(cmd):
        try:
            proc.stdin.write((cmd + "\n").encode())
            proc.stdin.flush()
        except:
            return False
        return True

    # Wait for startup
    time.sleep(3)

    loop = 0
    total_chunks = 0

    try:
        while True:
            loop += 1
            log(f"=== LOOP {loop} ===")

            for filepath in get_corpus_files():
                try:
                    with open(filepath, 'r', errors='ignore') as f:
                        lines = f.readlines()
                except:
                    continue

                if not lines:
                    continue

                log(f"Feeding {os.path.basename(filepath)} ({len(lines)} lines)")

                for i in range(0, len(lines), CHUNK_LINES):
                    chunk = lines[i:i+CHUNK_LINES]

                    with open('/tmp/uhma_chunk.txt', 'w') as cf:
                        cf.writelines(chunk)

                    if not send("eat /tmp/uhma_chunk.txt"):
                        alert("Send failed")
                        return

                    total_chunks += 1

                    # Keep alive with empty line
                    time.sleep(CHUNK_PAUSE)
                    send("")  # empty keeps stdin active

                    if proc.poll() is not None:
                        alert(f"UHMA died at chunk {total_chunks}")
                        return

            # Consolidate
            log("Consolidating...")
            send("observe")
            time.sleep(2)
            send("")

            send("dream")
            time.sleep(2)
            send("")

            send("status")
            time.sleep(1)

            log(f"Loop {loop} done ({total_chunks} chunks)")

            if loop % 5 == 0:
                send(f"save loop{loop}")
                log("Saved")

    except KeyboardInterrupt:
        log("Stopped")
    finally:
        send("save final")
        time.sleep(1)
        proc.terminate()
        log("Done")

if __name__ == "__main__":
    main()
