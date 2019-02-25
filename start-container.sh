#!/bin/bash

docker run -it -p 3003:3003 --rm \
  --mount type=bind,source=$(pwd),target=/project \
  --name tictactoe_container miso_tictactoe
