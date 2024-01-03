import * as fs from "fs/promises";
import moo from "moo";
import * as path from "path";
import yargs from "yargs";
import { hideBin } from "yargs/helpers";
import { lexer } from "./lex.js";
import { filter, forest, parseUse, toplevel } from "./parse.js";

const parse = (lex: moo.Lexer, source: string): string[] => {
  lex.reset(source);
  const tokens = [...lex];
  const trees = filter(tokens, forest(tokens));
  const toplevels = toplevel(tokens, trees);
  return (toplevels.get("use") ?? []).map(
    (id) => tokens[parseUse(tokens, trees, id).module].text,
  );
};

const transform = (lex: moo.Lexer, source: string): string => {
  const uses = parse(lex, source);
  return [
    ...uses.map((use, i) => `import * as _${i} from ${use};`),
    "export const uses = {",
    ...uses.map((use, i) => `  ${use}: _${i},`),
    "};",
    `export const source = ${JSON.stringify(source)};`,
    "",
  ].join("\n");
};

const src = "src";
const dist = "dist";

const walk = async (lex: moo.Lexer, dir: string) => {
  const distDir = path.join(dist, dir);
  await fs.mkdir(distDir, { recursive: true });
  const srcDir = path.join(src, dir);
  for (const child of await fs.readdir(srcDir)) {
    const dirChild = path.join(dir, child);
    const srcDirChild = path.join(src, dirChild);
    if ((await fs.stat(srcDirChild)).isDirectory()) await walk(lex, dirChild);
    else {
      const ext = path.extname(child);
      if (ext === ".rose") {
        const base = path.basename(child, ext);
        const distDirChild = path.join(distDir, `${base}.js`);
        const contents = await fs.readFile(srcDirChild, "utf8");
        await fs.writeFile(distDirChild, transform(lex, contents));
      }
    }
  }
};

yargs(hideBin(process.argv))
  .command(
    "build",
    "build the current package",
    () => {},
    () => walk(lexer(), ""),
  )
  .demandCommand()
  .strict()
  .version("0.5.0") // TODO: why must this be manually specified?
  .help().argv;
