#!/bin/bash
RESULT="$(pwd)/result"

sigIntHandler() {
  echo ""
  echo "[${BASH_SOURCE[0]}] Killing $PROGRAM..."
  kill $PID
  exit
}

trap sigIntHandler SIGINT

PROGRAM=$@

cd $RESULT

while true; do
  echo "[${BASH_SOURCE[0]}] Starting $@..."
  $PROGRAM &
  PID=$!
  # Wait for the server executable to change, then kill it, and let it
  # start again in the next iteration.
  fswatch -0 -1 -m poll_monitor \
          --event AttributeModified --event Created --event Updated \
          "$RESULT/bin/server" | xargs -0 -n 1 -I {} kill $PID
  echo "[${BASH_SOURCE[0]}] Relaunching $PROGRAM..."
  echo ""
done
