#!/usr/bin/env bash
set -exo pipefail
docker run --rm -it -p 8888:8888/udp \
  -v $PWD:/workspaces/protohacks \
  --workdir=/workspaces/protohacks \
  cdaringe/ocaml5-devcontainer
