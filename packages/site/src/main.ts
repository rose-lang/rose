import hljs from "highlight.js/lib/core";
import javascript from "highlight.js/lib/languages/javascript";
import "highlight.js/styles/base16/helios.css";

hljs.registerLanguage("javascript", javascript);
hljs.highlightAll();

console.log(await import("rose"));
