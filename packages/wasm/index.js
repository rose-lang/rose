import init, { initialize } from "./wbg/rose_web.js";
import bytes from "./wbg/rose_web_bg.wasm";

await init(bytes);
initialize();

export * from "./wbg/rose_web.js";
