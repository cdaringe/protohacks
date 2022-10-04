import type { Task, Tasks } from "https://deno.land/x/rad/src/mod.ts";

const buildDevcontainer: Task =
  `docker buildx build --progress=plain --platform linux/amd64 -t cdaringe/ocaml5-devcontainer .`;
const build: Task = `dune build`;

export const tasks: Tasks = {
  ...{ build, b: build },
  ...{ buildDevcontainer, bd: buildDevcontainer },
};
