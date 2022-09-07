var net = require("net");

// Configuration parameters
var HOST = "localhost";
var PORT = 10000;
const malformedMsg = { method: "malformed" };

function isPrime(number) {
  let start = 2;
  const limit = Math.sqrt(number);
  while (start <= limit) {
    if (number % start++ < 1) return false;
  }
  return number > 1;
}

// Create Server instance
var server = net.createServer(function onClientConnected(sock) {
  const send = (msg, cb) => sock.write(`${JSON.stringify(msg)}\n`, cb);
  const terminate = (err, cb) => {
    console.error(`terminating: ${err}`);
    send(malformedMsg, () => sock.end(cb));
  };
  let q = Promise.resolve();
  var remoteAddress = sock.remoteAddress + ":" + sock.remotePort;
  console.log("new client connected: %s", remoteAddress);
  let chars = [];
  const flush = (msg) => {
    q = new Promise((res, rej) => {
      try {
        const parsed = JSON.parse(msg);
        if (parsed.method === "isPrime") {
          send({ method: "isPrime", prime: isPrime(parsed.number) }, () => {
            console.log("msg-flushed");
            res();
          });
        } else {
          terminate(new Error(`bogus req: ${JSON.stringify(msg)}`), rej);
        }
      } catch (err) {
        terminate(err, rej);
      }
    });
  };
  sock.on("data", function (data) {
    console.error(`data`);
    for (const charCode of data) {
      const char = String.fromCharCode(charCode);
      process.stdout.write(char);
      // if (char === '\n') {
      // flush()
      // } else {
      chars.push(char);
      // }
    }
    const full = chars.join("");
    console.log(full);
    const isEvilEnding = full.endsWith("\n");
    const toProcess = full.split("\n");
    const finalI = toProcess.length - 1;
    toProcess.forEach((it, i) => {
      if (i === finalI && isEvilEnding) {
        chars = it.split("");
      } else {
        flush(it);
      }
    });
  });
  sock.on("close", function () {
    console.log("socket closing");
  });
  sock.on("error", function (err) {
    console.error(`error: ${err}`);
    terminate(err);
  });
});

server.listen(
  PORT,
  HOST,
  () => console.log("server listening on %j", server.address()),
);
