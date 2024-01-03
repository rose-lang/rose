import * as fs from "fs/promises";
import moo from "moo";
import * as path from "path";
import yargs from "yargs";
import { hideBin } from "yargs/helpers";
import { lexer } from "./lex.js";
import {
  ToplevelKind,
  filter,
  forest,
  parseInfix,
  parseUse,
  toplevel,
} from "./parse.js";

const backtick = (s: string) => `\`${s.replace(/[`\$\\]/g, (x) => `\\${x}`)}\``;

const infixes: ToplevelKind[] = ["infix", "infixl", "infixr"];

const parse = (
  lex: moo.Lexer,
  source: string,
): {
  use: { pub: boolean; names: string[]; module: string }[];
  infix: { pub: boolean; assoc: string; prec: string[]; op: string }[];
} => {
  lex.reset(source);
  const tokens = [...lex];
  const trees = filter(tokens, forest(tokens));
  const toplevels = toplevel(tokens, trees);
  return {
    use: (toplevels.get("use") ?? []).map((id) => {
      const { pub, names, module } = parseUse(tokens, trees, id);
      return {
        pub: pub !== undefined,
        names: names.map((id) => tokens[id].text),
        module: tokens[module].text,
      };
    }),
    infix: infixes
      .flatMap((kw) => toplevels.get(kw) ?? [])
      .map((id) => {
        const { pub, assoc, prec, op } = parseInfix(tokens, trees, id);
        return {
          pub: pub !== undefined,
          op: tokens[op].text,
          assoc: tokens[assoc].text,
          prec: prec.map((id) => tokens[id].text),
        };
      }),
  };
};

const transform = (lex: moo.Lexer, source: string): string => {
  const { use, infix } = parse(lex, source);
  return [
    ...use.map(({ module }, i) => `import * as _${i} from ${module};`),
    "export const use = {",
    ...use.map(
      ({ pub, names, module }, i) =>
        `  ${module}: { resolved: _${i}, pub: ${pub}, names: [${names
          .map((name) => JSON.stringify(name))
          .join(", ")}] },`,
    ),
    "};",
    "export const infix = {",
    ...infix.map(
      ({ pub, assoc, prec, op }) =>
        `  ${JSON.stringify(op)}: { pub: ${pub}, assoc: ${JSON.stringify(
          assoc,
        )}, prec: [${prec.map((x) => `${x}`).join(", ")}] },`,
    ),
    "};",
    `export const source = ${backtick(source)};`,
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
