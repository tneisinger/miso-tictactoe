# Miso Isomorphic Example using Docker and Stack

Run a simple isomorphic [Miso](https://github.com/dmjio/miso) app inside a
[docker](https://www.docker.com) container.  Code changes will automatically
trigger the server to rebuild and restart, so your changes will be viewable
after a browser refresh.

This repo borrows heavily from the very helpful [Tehnix/miso-isomorphic-stack
repo](https://github.com/Tehnix/miso-isomorphic-stack), which itself is based
on [this Miso example](https://github.com/FPtje/miso-isomorphic-example).

The goal here is to create a simple editor-agnostic development environment for
an isomorphic Miso app that will be easy to deploy.  I also wanted to avoid
using [Nix](https://nixos.org/nix/), since I'm already familiar with
[Stack](https://docs.haskellstack.org/en/stable/README/).

This is a work-in-progress, so suggestions are welcome.

## Prerequisites

You will need to have [docker](https://docs.docker.com/install/) installed
on your system.  This repo was built using docker version 18.09.1.

## Clone and Build

Clone this repo and run build-docker-image.sh:
```bash
git clone https://github.com/tneisinger/miso-stack-docker
cd miso-stack-docker
./build-docker-image.sh
```
**Note: Running build-docker-image.sh will take a long time.**

## Run It!

To start the app, simply run: 
```bash
./start-container.sh
```
This will start a docker container based on the docker image that was built by
`build-docker-image.sh`.  This docker container will automatically run
`reloader.sh`, which is responsible for running the app server, as well as
rebuilding and restarting the server on code changes.

Go to `http://localhost:3003` in your browser to see the result.

## Areas For Improvement

- It's great that the server automatically rebuilds and restarts on code
  changes, but it's not so great that it does it so slowly.  In the worst case,
  this process has taken up to 16 seconds on my machine!
