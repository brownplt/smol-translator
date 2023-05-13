// This program translates SMoL programs to JavaScript
//   and will do so for Python in the future.

import * as S2J from '../src/smol_to_js.bs.js';
import fs from "fs";

const path = "./test/test_cases/from_tutor";
const suffix = ".smol";
for (const f of fs.readdirSync(path)) {
    if (f.endsWith(suffix)) {
        const name = f.substring(0, f.length - suffix.length);
        const programFile = `${path}/${name}.smol`;
        const answerFile = `${path}/${name}.txt`;
        try {
            const program = fs.readFileSync(programFile, 'utf8');
            fs.writeFileSync(`${programFile}.js`, S2J.translate_program(program));
            const answer = fs.readFileSync(answerFile, 'utf8');
            fs.writeFileSync(`${answerFile}.js`, S2J.translate_results(answer));
        } catch (err) {
            console.log(programFile);
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
