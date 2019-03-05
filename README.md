# Miso Tic-Tac-Toe App

An isomorphic Tic-Tac-Toe web app, built with
[Miso](https://github.com/dmjio/miso).

The app is built and run inside a [Docker](https://www.docker.com) container.
[This project scaffold](https://github.com/tneisinger/miso-stack-docker) was
used as a starting point for development of this app.

## Prerequisites for Installation

You will need to have [docker](https://docs.docker.com/install/) installed
on your system.  This project was built using docker version 18.09.1.

If you want to modify the project, I would recommend installing
[stack](https://docs.haskellstack.org/en/stable/README/) as well.

## Clone and Build

Clone this repo and run build-docker-image.sh:
```bash
git clone https://github.com/tneisinger/miso-tictactoe
cd miso-tictactoe
./build-docker-image.sh
```
**Note: Running build-docker-image.sh will take a long time.**

## Run It!

To start the app, simply run: 
```bash
./start-container.sh
```
This will start a docker container based on the docker image that was built by
`build-docker-image.sh`.  The docker container will automatically run
`reloader.sh`, which is responsible for running the app server, as well as
rebuilding and restarting the server on code changes.

Go to `http://localhost:3003` in your browser to see the result.

## Attributions

The [scaffold](https://github.com/tneisinger/miso-stack-docker) for
this project was heavily inspired by [Tehnix/miso-isomorphic-stack
repo](https://github.com/Tehnix/miso-isomorphic-stack).
