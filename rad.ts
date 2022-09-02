import type { Task, Tasks } from "https://deno.land/x/rad/src/mod.ts";

const buildProdContainer: Task = `docker build -t cdaringe/ocaml5-devcontainer .`;
const buildDevcontainer: Task = `docker buildx build --platform linux/amd64 -t cdaringe/ocaml5-devcontainer .`;
const build: Task = `dune build`
const run: Task = `dune exec bin/echo.exe`
const sync: Task = `rsync -a --exclude=_build -r $PWD/ $HTTP_SERVER_ADMIN@$HTTP_SERVER_IP:/www/protohacks/`
const deploy: Task = {
  dependsOn: [sync],
  async fn ({ sh }) {
    return sh(`ssh $HTTP_SERVER_ADMIN@$HTTP_SERVER_IP 'cd /www/protohacks/ && ${buildDevcontainer}'`)
  }
}
export const tasks: Tasks = {
  sync,
  deploy,
  ...{ run, r: run },
  ...{ build, b: build},
  ...{ buildDevcontainer, bd: buildDevcontainer }
};
