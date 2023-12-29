import * as fs from "fs/promises";
import fetch from "node-fetch";

const [url, path] = process.argv.slice(2);
await fs.writeFile(path, Buffer.from(await (await fetch(url)).arrayBuffer()));
