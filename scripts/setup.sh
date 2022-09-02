#!/usr/bin/env bash
set -exo pipefail
dune build || true # it's ok if it fails, but we need a fresh .opam file!
opam install . --deps-only --with-test --with-doc
