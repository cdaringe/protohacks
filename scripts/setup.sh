#!/usr/bin/env bash
set -exo pipefail
eval $(opam env)
dune build || true # it's ok if it fails, but we need a fresh .opam file!
eval $(opam env)
