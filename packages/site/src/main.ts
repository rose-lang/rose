import * as rose from "rose";

const elem = document.getElementById("code");
if (elem) elem.innerText = JSON.stringify(rose, null, 2);
