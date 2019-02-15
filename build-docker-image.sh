#!/bin/bash

docker build --tag miso_isomorphic .

id=$(docker create miso_isomorphic) \
  && echo "Copy backend/.stack-work..." \
  && docker cp $id:/project/backend/.stack-work ./backend/.stack-work \
  && echo "Copy frontend/.stack-work..." \
  && docker cp $id:/project/frontend/.stack-work ./frontend/.stack-work \
  && docker cp $id:/project/frontend/.stack-work/install/x86_64-linux/lts-9.21/ghcjs-0.2.1.9009021_ghc-8.0.2/bin/frontend.jsexe/all.js ./result/static/all.js \
  && docker cp $id:/project/backend/.stack-work/install/x86_64-linux/lts-10.10/8.2.2/bin/backend ./result/bin/server \
  && docker rm -v $id
