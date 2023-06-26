import init, { initialize } from "./dist/wbg/rose_web.js";
import bytes from "./dist/wbg/rose_web_bg.wasm";

await init(bytes);
initialize();

export * from "./dist/wbg/rose_web.js";
