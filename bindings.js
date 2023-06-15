import fs from "fs/promises";
import path from "path";

const start_dir = "crates";
const dest_dir = "packages/wasm/dist/bindings";

const crates = await fs.readdir(start_dir, { withFileTypes: true });
for (const crate of crates) {
  if (!crate.isDirectory()) continue;
  const bindings_dir = path.join(start_dir, crate.name, "bindings");

  const stat = await fs.stat(bindings_dir).catch(() => null);
  if (!stat || !stat.isDirectory()) {
    continue; // bindings directory does not exist for this crate, so skip it
  }

  // Create the destination directory
  const dest_folder = path.join(dest_dir, crate.name);
  await fs.mkdir(dest_folder, { recursive: true });

  // Read all .ts files in the bindings directory
  const files = (await fs.readdir(bindings_dir)).filter((f) =>
    f.endsWith(".ts")
  );
  for (const file of files) {
    const tsfile = path.join(bindings_dir, file);

    // Get the base filename, without the .ts extension
    const base_name = path.basename(tsfile, ".ts");

    // Read the content of the .ts file
    const ts_content = await fs.readFile(tsfile, "utf-8");

    // Add .js to import lines
    const updated_content = ts_content.replace(/^(import .*)";$/gm, '$1.js";');

    // Write the updated content to the new location with .d.ts extension
    await fs.writeFile(
      path.join(dest_folder, `${base_name}.d.ts`),
      updated_content
    );
  }
}
