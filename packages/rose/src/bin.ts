import * as fs from "fs/promises";
import * as path from "path";
import yargs from "yargs";
import { hideBin } from "yargs/helpers";

const backtick = (s: string) => `\`${s.replace(/[`\$\\]/g, (x) => `\\${x}`)}\``;

const transform = (source: string): string => {
  return `export const source = ${backtick(source)};\n`;
};

const src = "src";
const dist = "dist";

const walk = async (dir: string) => {
  const distDir = path.join(dist, dir);
  await fs.mkdir(distDir, { recursive: true });
  const srcDir = path.join(src, dir);
  for (const child of await fs.readdir(srcDir)) {
    const dirChild = path.join(dir, child);
    const srcDirChild = path.join(src, dirChild);
    if ((await fs.stat(srcDirChild)).isDirectory()) await walk(dirChild);
    else {
      const ext = path.extname(child);
      if (ext === ".rose") {
        const base = path.basename(child, ext);
        const distDirChild = path.join(distDir, `${base}.js`);
        const contents = await fs.readFile(srcDirChild, "utf8");
        await fs.writeFile(distDirChild, transform(contents));
      }
    }
  }
};

yargs(hideBin(process.argv))
  .command(
    "build",
    "build the current package",
    () => {},
    () => walk(""),
  )
  .demandCommand()
  .strict()
  .version("0.5.0") // TODO: why must this be manually specified?
  .help().argv;
