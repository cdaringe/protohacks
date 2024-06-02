# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.245.0/containers/ubuntu/.devcontainer/base.Dockerfile
# FROM ocaml/opam:ubuntu-lts-ocaml-5.1
# FROM ocaml/opam:alpine-ocaml-5.0 as switch
FROM ocaml/opam:alpine-3.19-ocaml-5.1 as switch
USER root
RUN apk update
RUN apk add pkgconfig linux-headers gmp gmp-dev curl
USER opam
WORKDIR /app
COPY ./dune-project ./dune-project
RUN eval $(opam env) \
  && opam install dune \
  && dune build @install @runtest
