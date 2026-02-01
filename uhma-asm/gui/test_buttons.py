#!/usr/bin/env python3
"""
test_buttons.py — UHMA GUI Button Tester (automated crash detection)

@entry main()                       Run button click tests
@entry click_button(name) -> bool   Click button, return True if no crash

@calls subprocess (spawns viz_uhma)
@calls xdotool (simulates mouse clicks)
@calledby CLI (python3 gui/test_buttons.py)

GOTCHAS:
  - Requires xdotool installed (apt install xdotool)
  - Button positions hardcoded - update if layout changes
  - Monitors subprocess for crashes after each click
"""

import subprocess
import time
import os
import signal

# Button positions (x_center, y_center, name)
BUTTONS = {
    'DREAM':   (55, 25),
    'OBSERVE': (140, 25),
    'EVOLVE':  (225, 25),
    'STEP':    (300, 25),
    'RUN100':  (385, 25),
    'SAVE':    (480, 25),
    'LOAD':    (550, 25),
    'FILE':    (617, 25),
    'DIR':     (677, 25),
    'QUIT':    (1230, 25),
    'SEND':    (1130, 395),
    'CLEAR':   (1205, 395),
}

def get_window_id():
    """Find UHMA window by size (1280x800)"""
    try:
        # Get all windows
        result = subprocess.run(['xdotool', 'search', '--name', ''],
                              capture_output=True, text=True, timeout=5)
        windows = result.stdout.strip().split('\n')

        for wid in windows:
            if not wid:
                continue
            try:
                # Check geometry
                geo = subprocess.run(['xdotool', 'getwindowgeometry', wid],
                                   capture_output=True, text=True, timeout=2)
                if '1280x800' in geo.stdout:
                    return wid
            except:
                continue

        # Fallback: try wmctrl to find N/A window
        result = subprocess.run(['wmctrl', '-l'], capture_output=True, text=True, timeout=5)
        for line in result.stdout.split('\n'):
            if 'N/A N/A' in line or 'N/A' in line.split()[-1]:
                wid = line.split()[0]
                # Convert hex to decimal
                return str(int(wid, 16))

        return None
    except:
        return None

def click_button(winid, name):
    """Click a button by name"""
    if name not in BUTTONS:
        print(f"Unknown button: {name}")
        return False

    x, y = BUTTONS[name]
    try:
        subprocess.run(['xdotool', 'windowactivate', winid], timeout=2)
        time.sleep(0.1)
        subprocess.run(['xdotool', 'mousemove', '--window', winid, str(x), str(y)], timeout=2)
        time.sleep(0.1)
        subprocess.run(['xdotool', 'click', '1'], timeout=2)
        return True
    except Exception as e:
        print(f"Click failed: {e}")
        return False

def is_running(pid):
    """Check if process is still running"""
    try:
        os.kill(pid, 0)
        return True
    except OSError:
        return False

def start_gui():
    """Start the GUI and return process"""
    stderr_log = '/tmp/viz_stderr.log'
    stdout_log = '/tmp/viz_stdout.log'

    # Clean old logs
    for f in [stderr_log, stdout_log]:
        if os.path.exists(f):
            os.remove(f)

    with open(stdout_log, 'w') as out, open(stderr_log, 'w') as err:
        proc = subprocess.Popen(['./uhma-viz'], stdout=out, stderr=err)

    time.sleep(2)
    return proc, stderr_log

def read_log(path):
    """Read log file contents"""
    try:
        with open(path, 'r') as f:
            return f.read()
    except:
        return ""

def test_button(button_name, wait_time=2):
    """Test a single button"""
    print(f"\n{'='*50}")
    print(f"Testing: {button_name}")
    print('='*50)

    proc, stderr_log = start_gui()
    time.sleep(1)

    winid = get_window_id()
    if not winid:
        print("ERROR: Could not find window")
        proc.terminate()
        return False

    print(f"Window ID: {winid}")
    print(f"Clicking {button_name}...")

    click_button(winid, button_name)
    time.sleep(wait_time)

    alive = is_running(proc.pid)
    trace = read_log(stderr_log)

    print(f"\nTrace output:")
    print(trace if trace else "(none)")
    print(f"\nProcess alive: {alive}")

    if alive:
        proc.terminate()
        try:
            proc.wait(timeout=2)
        except:
            proc.kill()

    return alive

def test_all_buttons():
    """Test all buttons one by one"""
    results = {}

    for name in ['DREAM', 'OBSERVE', 'EVOLVE', 'STEP', 'SAVE', 'LOAD', 'FILE', 'DIR', 'CLEAR']:
        # Skip QUIT and RUN100 for now
        if name == 'QUIT':
            continue
        results[name] = test_button(name, wait_time=3 if name in ['FILE', 'DIR'] else 1)
        time.sleep(1)

    print("\n" + "="*50)
    print("RESULTS")
    print("="*50)
    for name, passed in results.items():
        status = "PASS" if passed else "FAIL (crashed)"
        print(f"  {name:10} : {status}")

def interactive_test():
    """Interactive testing mode"""
    proc, stderr_log = start_gui()
    winid = get_window_id()

    if not winid:
        print("Could not find window!")
        proc.terminate()
        return

    print(f"GUI started (PID: {proc.pid}, Window: {winid})")
    print(f"Available buttons: {', '.join(BUTTONS.keys())}")
    print("Commands: <button_name> | 'log' | 'status' | 'quit'")

    try:
        while True:
            cmd = input("\n> ").strip().upper()

            if cmd == 'QUIT' or cmd == 'Q':
                break
            elif cmd == 'LOG':
                print(read_log(stderr_log))
            elif cmd == 'STATUS':
                print(f"Running: {is_running(proc.pid)}")
            elif cmd in BUTTONS:
                click_button(winid, cmd)
                time.sleep(0.5)
                if not is_running(proc.pid):
                    print("!!! CRASHED !!!")
                    print("Trace:")
                    print(read_log(stderr_log))
                    break
                print("OK")
            else:
                print(f"Unknown: {cmd}")
    finally:
        if is_running(proc.pid):
            proc.terminate()

if __name__ == '__main__':
    import sys

    if len(sys.argv) > 1:
        if sys.argv[1] == 'all':
            test_all_buttons()
        elif sys.argv[1] == 'interactive' or sys.argv[1] == 'i':
            interactive_test()
        elif sys.argv[1] in BUTTONS:
            test_button(sys.argv[1])
        else:
            print(f"Usage: {sys.argv[0]} [all|interactive|<button_name>]")
            print(f"Buttons: {', '.join(BUTTONS.keys())}")
    else:
        # Default: test FILE button since that's what was crashing
        test_button('FILE', wait_time=5)
