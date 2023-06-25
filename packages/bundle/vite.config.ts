import * as path from "path";
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

export default defineConfig({
  build: {
    lib: { entry: path.resolve(__dirname, "src/index.ts"), formats: ["es"] },
  },
  plugins: [topLevelAwait()],
});
