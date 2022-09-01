set -exo pipefail
opam install dune
dune build || true # it's ok if it fails, but we need a fresh .opam file!
opam install . --deps-only --with-test --with-doc
