#!/bin/bash
cd /home/peter/Desktop/STARWARS/uhma-asm

# Config
CHUNK_LINES=500
PAUSE_BETWEEN_CHUNKS=5
PAUSE_BETWEEN_FILES=30

# Kill old instances, start fresh headless
pkill -9 uhma 2>/dev/null
sleep 1
rm -f uhma_training.log
./uhma < /dev/null > uhma_training.log 2>&1 &
UHMA_PID=$!
echo "Started UHMA PID: $UHMA_PID"
sleep 3

# Check if UHMA is running
if ! kill -0 $UHMA_PID 2>/dev/null; then
    echo "UHMA failed to start"
    exit 1
fi

# Function: feed file in chunks
feed_chunked() {
    local file="$1"
    local total_lines=$(wc -l < "$file")
    local chunks=$(( (total_lines + CHUNK_LINES - 1) / CHUNK_LINES ))

    echo "[$(date)] Feeding $file ($total_lines lines in $chunks chunks)"

    local offset=1
    local chunk_num=1

    while [ $offset -le $total_lines ]; do
        # Extract chunk to temp file
        sed -n "${offset},$((offset + CHUNK_LINES - 1))p" "$file" > /tmp/uhma_chunk.txt

        # Feed chunk via TCP
        echo "eat /tmp/uhma_chunk.txt" | nc -q 1 localhost 9999 2>/dev/null

        echo "  Chunk $chunk_num/$chunks (lines $offset-$((offset + CHUNK_LINES - 1)))"

        sleep $PAUSE_BETWEEN_CHUNKS

        # Check UHMA still alive
        if ! kill -0 $UHMA_PID 2>/dev/null; then
            echo "UHMA died during feeding!"
            exit 1
        fi

        offset=$((offset + CHUNK_LINES))
        chunk_num=$((chunk_num + 1))
    done

    echo "[$(date)] Finished feeding $file"
    rm -f /tmp/uhma_chunk.txt
}

# Function: query status
query_status() {
    echo "status" | nc -q 1 localhost 9997 2>/dev/null
    sleep 1
    timeout 2 nc -w 2 localhost 9996 2>/dev/null
}

# Function: send single command
send_cmd() {
    echo "$1" | nc -q 1 localhost 9999 2>/dev/null
    echo "[$(date)] Sent: $1"
}

echo "=== UHMA Training Started ==="
echo ""

# Collect all corpus files (recursively)
CORPUS_DIR="${1:-corpus}"
CORPUS_FILES=$(find "$CORPUS_DIR" -type f -name "*.txt" | sort)
FILE_COUNT=$(echo "$CORPUS_FILES" | wc -l)
CONSOLIDATE_EVERY=5  # observe+dream after every N files

echo "Corpus: $CORPUS_DIR ($FILE_COUNT files)"
echo "Consolidate every: $CONSOLIDATE_EVERY files"
echo ""

file_num=0
for file in $CORPUS_FILES; do
    file_num=$((file_num + 1))
    echo "--- File $file_num/$FILE_COUNT: $file ---"
    feed_chunked "$file"
    sleep $PAUSE_BETWEEN_FILES

    # Consolidate periodically (RFLAG_ANALYZED makes this efficient)
    if [ $((file_num % CONSOLIDATE_EVERY)) -eq 0 ]; then
        echo "--- Consolidation cycle ---"
        send_cmd "observe"
        sleep 10
        send_cmd "dream"
        sleep 10
        query_status
    fi
done

# Final consolidation
echo "--- Final Consolidation ---"
send_cmd "observe"
sleep 10
send_cmd "dream"
sleep 10

echo ""
echo "=== Training Complete ==="
query_status