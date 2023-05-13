import * as S2J from '../src/smol_to_js.bs.js';
import fs from "fs";

const path = "./test/test_cases/from_tutor";
for (const f of fs.readdirSync(path)) {
    if (f.endsWith(".smol")) {
        const file = `${path}/${f}`;
        try {
            const data = fs.readFileSync(file, 'utf8');
            fs.writeFileSync(`${file}.js`, S2J.translate_program(data));
        } catch (err) {
            console.log(file);
            console.error(err);
        }
    }
}

// const file = "./test/test_cases/examples/defvar.smol";
// try {
//     const data = fs.readFileSync(file, 'utf8');
//     console.log(data);
//     console.log("-----");
//     console.log(S2J.translate_program(data));
// } catch (err) {
//     console.error(err);
// }
