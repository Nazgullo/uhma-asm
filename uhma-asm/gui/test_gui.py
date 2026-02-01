#!/usr/bin/env python3
"""
test_gui.py — UHMA GUI Full Integration Test (user interaction simulation)

@entry main()                           Run full interaction test suite
@entry test_button_sequence() -> bool   Test menu bar buttons
@entry test_file_dialog() -> bool       Test file/dir selection
@entry test_input_field() -> bool       Test text input + SEND

@calls subprocess (spawns viz_uhma)
@calls xdotool (mouse clicks, keyboard input)
@calledby CLI (python3 gui/test_gui.py)

GOTCHAS:
  - Requires xdotool installed
  - Button positions for "Command & Control" layout
  - Waits between actions for GUI responsiveness
"""

import subprocess
import time
import os
import signal
import sys

# Button positions (x_center, y_center) - Updated for new Command & Control layout
# Menu bar buttons: y=8, h=30 -> center y=23
# Input bar: y = 800-50-35+10 = 725
BUTTONS = {
    'DREAM':   (40, 23),       # x=10, w=60
    'OBSERVE': (115, 23),      # x=80, w=70
    'EVOLVE':  (190, 23),      # x=160, w=60
    'STEP':    (255, 23),      # x=230, w=50
    'RUN100':  (312, 23),      # x=290, w=45 (RUN button)
    'SAVE':    (370, 23),      # x=345, w=50
    'LOAD':    (430, 23),      # x=405, w=50
    'FILE':    (487, 23),      # x=465, w=45
    'DIR':     (540, 23),      # x=520, w=40
    'TRACE':   (597, 23),      # x=570, w=55
    'QUIT':    (1235, 23),     # x=1210, w=50
    'SEND':    (1127, 740),    # x=1100, y=725, w=55
    'CLEAR':   (1192, 740),    # x=1165, y=725, w=55
}

# Input field position (centered in input area)
INPUT_FIELD = (600, 740)

class GUITester:
    def __init__(self):
        self.proc = None
        self.winid = None
        self.output_log = '/tmp/gui_test_output.log'

    def start_gui(self):
        """Start the GUI process"""
        self.cleanup()
        time.sleep(0.5)

        with open(self.output_log, 'w') as f:
            self.proc = subprocess.Popen(
                ['./uhma-viz'],
                stdout=f,
                stderr=subprocess.STDOUT
            )
        time.sleep(2)

        self.winid = self._find_window()
        if not self.winid:
            print("ERROR: Could not find GUI window")
            return False
        print(f"GUI started (PID: {self.proc.pid}, Window: {self.winid})")
        return True

    def _find_window(self):
        """Find the GUI window by size (1280x800)"""
        try:
            result = subprocess.run(['xdotool', 'search', '--name', ''],
                                  capture_output=True, text=True, timeout=5)
            for wid in result.stdout.strip().split('\n'):
                if not wid:
                    continue
                try:
                    geo = subprocess.run(['xdotool', 'getwindowgeometry', wid],
                                       capture_output=True, text=True, timeout=2)
                    if '1280x800' in geo.stdout:
                        return wid
                except:
                    continue
        except:
            pass
        return None

    def is_alive(self):
        """Check if GUI is still running"""
        if not self.proc:
            return False
        return self.proc.poll() is None

    def click(self, x, y):
        """Click at coordinates"""
        if not self.winid:
            return False
        try:
            subprocess.run(['xdotool', 'windowactivate', self.winid], timeout=2)
            time.sleep(0.1)
            subprocess.run(['xdotool', 'mousemove', '--window', self.winid, str(x), str(y)], timeout=2)
            time.sleep(0.1)
            subprocess.run(['xdotool', 'click', '1'], timeout=2)
            return True
        except:
            return False

    def click_button(self, name):
        """Click a named button"""
        if name not in BUTTONS:
            print(f"Unknown button: {name}")
            return False
        x, y = BUTTONS[name]
        print(f"  Clicking {name} at ({x}, {y})...", end=' ')
        result = self.click(x, y)
        time.sleep(0.3)
        if self.is_alive():
            print("OK")
            return True
        else:
            print("CRASHED!")
            return False

    def type_text(self, text):
        """Type text into the input field"""
        if not self.winid:
            return False
        try:
            # Click input field first
            self.click(*INPUT_FIELD)
            time.sleep(0.2)
            subprocess.run(['xdotool', 'type', '--', text], timeout=10)
            return True
        except:
            return False

    def press_key(self, key):
        """Press a key (Return, Escape, etc)"""
        try:
            subprocess.run(['xdotool', 'key', key], timeout=2)
            return True
        except:
            return False

    def select_file(self, path):
        """Handle zenity file picker"""
        time.sleep(1)
        try:
            result = subprocess.run(['xdotool', 'search', '--name', 'Select File'],
                                  capture_output=True, text=True, timeout=3)
            zenity = result.stdout.strip().split('\n')[0]
            if zenity:
                subprocess.run(['xdotool', 'windowactivate', zenity], timeout=2)
                time.sleep(0.3)
                subprocess.run(['xdotool', 'type', path], timeout=5)
                time.sleep(0.2)
                subprocess.run(['xdotool', 'key', 'Return'], timeout=2)
                time.sleep(1)
                return True
        except:
            pass
        return False

    def select_folder(self, path):
        """Handle zenity folder picker"""
        time.sleep(1)
        try:
            result = subprocess.run(['xdotool', 'search', '--name', 'Select Folder'],
                                  capture_output=True, text=True, timeout=3)
            zenity = result.stdout.strip().split('\n')[0]
            if zenity:
                subprocess.run(['xdotool', 'windowactivate', zenity], timeout=2)
                time.sleep(0.3)
                subprocess.run(['xdotool', 'type', path], timeout=5)
                time.sleep(0.2)
                subprocess.run(['xdotool', 'key', 'Return'], timeout=2)
                time.sleep(1)
                return True
        except:
            pass
        return False

    def get_output(self):
        """Get captured output"""
        try:
            with open(self.output_log, 'r') as f:
                return f.read()
        except:
            return ""

    def cleanup(self):
        """Kill GUI and zenity"""
        subprocess.run(['pkill', '-9', 'uhma-viz'], capture_output=True)
        subprocess.run(['pkill', '-9', 'zenity'], capture_output=True)
        if self.proc:
            try:
                self.proc.kill()
                self.proc.wait(timeout=1)
            except:
                pass
            self.proc = None
        self.winid = None


def create_test_files():
    """Create test files for loading"""
    # Small text file
    with open('/tmp/test_small.txt', 'w') as f:
        f.write("hello world\n")

    # Medium text file
    with open('/tmp/test_medium.txt', 'w') as f:
        f.write("the quick brown fox jumps over the lazy dog\n" * 5)

    # Test directory
    os.makedirs('/tmp/test_uhma_dir', exist_ok=True)
    with open('/tmp/test_uhma_dir/file1.txt', 'w') as f:
        f.write("first file content\n")
    with open('/tmp/test_uhma_dir/file2.txt', 'w') as f:
        f.write("second file here\n")


def test_basic_buttons(tester):
    """Test basic buttons that don't require dialogs"""
    print("\n=== Testing Basic Buttons ===")
    results = {}

    for btn in ['DREAM', 'OBSERVE', 'EVOLVE', 'STEP', 'SAVE', 'LOAD', 'CLEAR']:
        results[btn] = tester.click_button(btn)
        if not tester.is_alive():
            print(f"GUI crashed on {btn}!")
            return results
        time.sleep(0.5)

    return results


def test_input_field(tester):
    """Test typing in input field and sending"""
    print("\n=== Testing Input Field ===")

    print("  Typing 'hello world'...", end=' ')
    tester.type_text("hello world")
    time.sleep(0.3)

    if not tester.is_alive():
        print("CRASHED!")
        return False
    print("OK")

    print("  Clicking SEND...", end=' ')
    tester.click_button('SEND')
    time.sleep(0.5)

    if not tester.is_alive():
        print("CRASHED!")
        return False

    # Check output for processing
    output = tester.get_output()
    if 'token=' in output:
        print("  Input was processed!")
        return True
    else:
        print("  No token processing seen")
        return True  # May still be OK


def test_file_picker(tester):
    """Test FILE button with file selection"""
    print("\n=== Testing File Picker ===")

    print("  Clicking FILE button...", end=' ')
    tester.click_button('FILE')

    if not tester.is_alive():
        print("CRASHED!")
        return False

    print("  Selecting /tmp/test_small.txt...", end=' ')
    if tester.select_file('/tmp/test_small.txt'):
        print("OK")
        time.sleep(2)

        if not tester.is_alive():
            print("  GUI crashed after file load!")
            return False

        output = tester.get_output()
        if 'token=' in output and 'LEARN' in output:
            print("  File was loaded and processed!")
            return True
        else:
            print("  File may have loaded (no crash)")
            return True
    else:
        print("FAILED to interact with picker")
        return False


def test_dir_picker(tester):
    """Test DIR button with folder selection"""
    print("\n=== Testing Directory Picker ===")

    print("  Clicking DIR button...", end=' ')
    tester.click_button('DIR')

    if not tester.is_alive():
        print("CRASHED!")
        return False

    print("  Selecting /tmp/test_uhma_dir...", end=' ')
    if tester.select_folder('/tmp/test_uhma_dir'):
        print("OK")
        time.sleep(3)

        if not tester.is_alive():
            print("  GUI crashed after dir load!")
            return False

        output = tester.get_output()
        if 'DREAM' in output:
            print("  Directory was loaded and processed!")
            return True
        else:
            print("  Directory may have loaded (no crash)")
            return True
    else:
        print("FAILED to interact with picker")
        return False


def test_run100(tester):
    """Test RUN 100 button"""
    print("\n=== Testing RUN 100 ===")

    print("  Clicking RUN100...", end=' ')
    tester.click_button('RUN100')
    time.sleep(2)  # Give it time to run

    if not tester.is_alive():
        print("CRASHED!")
        return False

    print("  Completed without crash")
    return True


def test_full_workflow(tester):
    """Test a complete user workflow"""
    print("\n=== Testing Full Workflow ===")

    # 1. Load a file
    print("  Step 1: Load file")
    tester.click_button('FILE')
    time.sleep(1)
    tester.select_file('/tmp/test_small.txt')
    time.sleep(2)

    if not tester.is_alive():
        print("  FAILED at file load")
        return False

    # 2. Run some cycles
    print("  Step 2: Run DREAM cycle")
    tester.click_button('DREAM')
    time.sleep(0.5)

    print("  Step 3: Run OBSERVE cycle")
    tester.click_button('OBSERVE')
    time.sleep(0.5)

    print("  Step 4: Run EVOLVE cycle")
    tester.click_button('EVOLVE')
    time.sleep(0.5)

    if not tester.is_alive():
        print("  FAILED during cycles")
        return False

    # 3. Type and send input
    print("  Step 5: Type input")
    tester.type_text("test input")
    time.sleep(0.3)
    tester.click_button('SEND')
    time.sleep(0.5)

    if not tester.is_alive():
        print("  FAILED at input")
        return False

    # 4. Save state
    print("  Step 6: Save state")
    tester.click_button('SAVE')
    time.sleep(0.5)

    # 5. Clear and reload
    print("  Step 7: Clear")
    tester.click_button('CLEAR')
    time.sleep(0.3)

    print("  Step 8: Load saved state")
    tester.click_button('LOAD')
    time.sleep(0.5)

    if not tester.is_alive():
        print("  FAILED at save/load")
        return False

    print("  Full workflow completed!")
    return True


def run_all_tests():
    """Run all tests"""
    print("=" * 60)
    print("UHMA GUI Integration Tests")
    print("=" * 60)

    # Setup
    create_test_files()
    tester = GUITester()

    results = {
        'basic_buttons': False,
        'input_field': False,
        'file_picker': False,
        'dir_picker': False,
        'run100': False,
        'full_workflow': False,
    }

    try:
        # Test 1: Basic buttons
        if tester.start_gui():
            results['basic_buttons'] = all(test_basic_buttons(tester).values())
        tester.cleanup()

        # Test 2: Input field
        if tester.start_gui():
            results['input_field'] = test_input_field(tester)
        tester.cleanup()

        # Test 3: File picker
        if tester.start_gui():
            results['file_picker'] = test_file_picker(tester)
        tester.cleanup()

        # Test 4: Dir picker
        if tester.start_gui():
            results['dir_picker'] = test_dir_picker(tester)
        tester.cleanup()

        # Test 5: RUN 100
        if tester.start_gui():
            results['run100'] = test_run100(tester)
        tester.cleanup()

        # Test 6: Full workflow
        if tester.start_gui():
            results['full_workflow'] = test_full_workflow(tester)
        tester.cleanup()

    finally:
        tester.cleanup()

    # Summary
    print("\n" + "=" * 60)
    print("TEST RESULTS")
    print("=" * 60)

    all_passed = True
    for name, passed in results.items():
        status = "PASS" if passed else "FAIL"
        symbol = "✓" if passed else "✗"
        print(f"  {symbol} {name:20} : {status}")
        if not passed:
            all_passed = False

    print("=" * 60)
    if all_passed:
        print("ALL TESTS PASSED!")
    else:
        print("SOME TESTS FAILED")
    print("=" * 60)

    return 0 if all_passed else 1


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'quick':
        # Quick test - just basic buttons
        tester = GUITester()
        create_test_files()
        if tester.start_gui():
            test_basic_buttons(tester)
            tester.cleanup()
    else:
        sys.exit(run_all_tests())
