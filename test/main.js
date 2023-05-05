import * as S2J from '../src/smol_to_js.bs.js';
import fs from "fs";
const file = "./test/test_cases/examples/defvar.smol";
try {
    const data = fs.readFileSync(file, 'utf8');
    console.log(data);
    console.log("-----");
    console.log(S2J.translate_program(data));
} catch (err) {
    console.error(err);
}
