import init from "./wbg/rose_web.js";
import bytes from "./wbg/rose_web_bg.wasm";
await init(bytes);

export * from "./wbg/rose_web.js";
