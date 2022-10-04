# protohacks

[protohackers](https://protohackers.com/) using OCaml v5.x

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ci.ocamllabs.io/badge/cdaringe/protohacks/main&logo=ocaml)](https://ci.ocamllabs.io/github/cdaringe/protohacks)

## usage

At the time of writing, ocaml 5.x is cutting edge. This is used intentionally
to enjoy and explore OCaml's built-in effect system. This project also uses [eio](https://github.com/ocaml-multicore/eio), which is an effect based event loop and toolkit, e.g. effects+libuv.

To make everything work as easy as possible on all architectures supported by opam2 docker images, I roll a [devcontainer](https://code.visualstudio.com/docs/remote/create-dev-container), such that the OCaml platform extension can work seamlessly.
This is paramount, because this required opam switch's dependencies are not yet all compatible with my mac M1.

- Build the container
  - `docker buildx build --progress=plain --platform linux/amd64 -t cdaringe/ocaml5-devcontainer .`
- Launch VSCode > Select "Reload in Container"
- `dune exec bin/servers.exe`

Profit.

## deployment

I do not deploy the servers, per se. I open an SSH tunnel to a cloud server, relaying my local server into the cloud. Protohackers then hits my cloud server, which hits my local server... and their massive test suites do great work.


