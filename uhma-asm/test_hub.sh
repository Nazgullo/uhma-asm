#!/bin/bash
# Test hub + gemini integration

# Clean
pkill -9 hub 2>/dev/null
pkill -f hub_gemini 2>/dev/null
sleep 2

# Start hub
./hub > /tmp/hub.log 2>&1 &
HUB_PID=$!
sleep 1

# Start Gemini
python3 hub_gemini.py --daemon > /tmp/gem.log 2>&1 &
GEM_PID=$!
sleep 1

echo "Hub PID: $HUB_PID"
echo "Gemini PID: $GEM_PID"

# Test
echo "=== Testing ==="
{
  echo "HELLO tester"
  sleep 1
  echo "@gemini What is 10 divided by 2?"
  sleep 4
} | nc localhost 7777

echo ""
echo "=== Gemini log ==="
cat /tmp/gem.log

# Cleanup
kill $HUB_PID $GEM_PID 2>/dev/null
