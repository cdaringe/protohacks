# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.245.0/containers/ubuntu/.devcontainer/base.Dockerfile
# FROM ocaml/opam:ubuntu-lts-ocaml-5.1
FROM ocaml/opam:ubuntu-lts-ocaml-5.0
USER root
RUN apt-get purge libappstream3 || true \
  && apt-get update && export DEBIAN_FRONTEND=noninteractive \
  && apt-get -y install --no-install-recommends pkg-config
USER opam
RUN test -r /home/opam/.opam/opam-init/init.sh \
  && . /home/opam/.opam/opam-init/init.sh > /dev/null 2> /dev/null \
  && eval $(opam env) \
  && opam install dune
WORKDIR /app
COPY ./dune-project ./dune-project
RUN opam exec dune build --best-effort \
  && opam install . --deps-only --with-test --with-doc --jobs=1
ENV DENO_INSTALL="/home/opam/.deno"
ENV PATH="$DENO_INSTALL/bin:$PATH"
ENV FOO=BAR
RUN curl -fsSL https://raw.githubusercontent.com/cdaringe/rad/main/assets/install.sh | bash
