import { createWriteStream } from "fs";
import https from "https";
import { pipeline } from "stream/promises";
import { fileURLToPath } from "url";

// extract command line arguments
const [fileUrl, outputPath] = process.argv.slice(2);

https
  .get(fileUrl, async (response) => {
    try {
      await pipeline(
        response,
        createWriteStream(fileURLToPath(new URL(outputPath, import.meta.url))),
      );
    } catch (error) {
      console.error(`Error fetching resource: ${error}`);
      process.exit(1);
    }
  })
  .on("error", (error) => {
    console.error(`Error: ${error}`);
    process.exit(1);
  });
