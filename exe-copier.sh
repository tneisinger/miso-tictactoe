#!/bin/bash

echo "[${BASH_SOURCE[0]}] Waiting for files to update..."

sigIntHandler() {
  echo ""
  echo "[${BASH_SOURCE[0]}] Killing rebuilder..."
  exit
}

trap sigIntHandler SIGINT

JSDIR=$(stack path --stack-yaml=frontend/stack.yaml --local-install-root)/bin/frontend.jsexe
SERVERDIR=$(stack path --stack-yaml=backend/stack.yaml --local-install-root)/bin

# Copy the build files into place whenver the stack build artifacts change.
fswatch -0 --event AttributeModified --event Created --event Updated \
           "$JSDIR/all.js" "$SERVERDIR/backend" | xargs -0 -n 1 -I {} \
  sh -c "echo '[${BASH_SOURCE[0]}] Copying build files...'; \
         cp -f \"$SERVERDIR/backend\" result/bin/server; \
         chmod 777 result/bin/server; \
         cp -f \"$JSDIR/all.js\" result/static/all.js;"
