import { initialize } from "@rose-lang/wasm";
import init from "./wbg/rose_web.js";
import bytes from "./wbg/rose_web_bg.wasm";

initialize();
await init(bytes);

export * from "./wbg/rose_web.js";
