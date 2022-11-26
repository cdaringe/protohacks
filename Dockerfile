# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.245.0/containers/ubuntu/.devcontainer/base.Dockerfile
# FROM ocaml/opam:ubuntu-lts-ocaml-5.1
# FROM ocaml/opam:alpine-ocaml-5.0 as switch
FROM ocaml/opam:alpine-3.16-ocaml-5.1 as switch
USER root
RUN apk update
RUN apk add pkgconfig linux-headers gmp gmp-dev curl
USER opam
RUN opam switch create --debug 5.0.0~beta1

FROM switch
RUN opam install dune
WORKDIR /app
RUN opam install ocaml-lsp-server eio
COPY ./dune-project ./dune-project
RUN opam install . --deps-only --with-test --with-doc
RUN opam exec dune build --best-effort
