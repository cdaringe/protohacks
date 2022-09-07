# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.245.0/containers/ubuntu/.devcontainer/base.Dockerfile
# FROM ocaml/opam:ubuntu-lts-ocaml-5.1
FROM denoland/deno:alpine as deno
FROM ocaml/opam:alpine-ocaml-5.0
USER root
RUN apk update
RUN apk add pkgconfig linux-headers
RUN apk add gmp gmp-dev
USER opam
RUN test -r /home/opam/.opam/opam-init/init.sh \
  && . /home/opam/.opam/opam-init/init.sh > /dev/null 2> /dev/null \
  && eval $(opam env) \
  && opam install dune
RUN opam pin stdint https://github.com/andrenth/ocaml-stdint.git#master
WORKDIR /app
COPY ./dune-project ./dune-project
RUN opam exec dune build --best-effort \
  && opam install . --deps-only --with-test --with-doc
