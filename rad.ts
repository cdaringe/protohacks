import type { Task, Tasks } from "https://deno.land/x/rad/src/mod.ts";

const buildDevcontainer: Task = `docker buildx build --platform linux/amd64 -t cdaringe/ocaml5-devcontainer .`;
const build: Task = `dune build`
const run: Task = `dune exec bin/echo.exe`
const sync: Task = `rsync -a --exclude=_build -r $PWD/ $HTTP_SERVER_ADMIN@$HTTP_SERVER_IP:/www/protohacks/`
const deploy: Task = {
  dependsOn: [sync],
  async fn ({ sh }) {
    const onBoxCmds = [
      "cd /www/protohacks/",
      buildDevcontainer,
      "chown -R cdaringe:webadmins /www/protohacks",
      "chmod -R ugo+rwx /www/protohacks",
      "ls -al /www/protohacks",
       "docker compose up -d --force-recreate"
    ].flatMap(cmd => [`echo "${cmd}"`, cmd]).join(" && ")
    return sh(`ssh $HTTP_SERVER_ADMIN@$HTTP_SERVER_IP '${onBoxCmds}'`)
  }
}
export const tasks: Tasks = {
  sync,
  ...{ deploy, d: deploy},
  ...{ run, r: run },
  ...{ build, b: build},
  ...{ buildDevcontainer, bd: buildDevcontainer }
};
