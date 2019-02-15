#!/bin/bash

# Tell stack to rebuild the backend whenever a relevant source file is changed
$(stack --stack-yaml=backend/stack.yaml build --fast --file-watch) &

# Tell stack to rebuild all.js whenever a relevant source file is changed
$(stack --stack-yaml=frontend/stack.yaml build --fast --file-watch) &

# Use fswatch to wait for changes to the files built by stack.
# When a change occurs copy the backend executable to result/bin/ and copy
# all.js to result/static/
./exe-copier.sh &

# Use fs watch to wait for changes to /result/bin/server or
# /result/static/all.js.  When either of these files change, restart
# result/bin/server
./runner.sh bin/server
