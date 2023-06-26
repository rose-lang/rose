import * as rose from "rose";

const elem = document.getElementById("code");
if (elem !== null) elem.innerText = JSON.stringify(rose, null, 2);
