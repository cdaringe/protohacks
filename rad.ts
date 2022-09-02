import type { Task, Tasks } from "https://deno.land/x/rad/src/mod.ts";

const buildDevcontainer: Task = `docker buildx build --platform linux/amd64 -t cdaringe/ocaml5-devcontainer .`;
const build: Task = `dune build`
const run: Task = `dune exec bin/echo.exe`
export const tasks: Tasks = {
  ...{ run, r: run },
  ...{ build, b: build},
  ...{ buildDevcontainer, bd: buildDevcontainer }
};
