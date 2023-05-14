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
        const outputsFile = `${path}/${name}.smol.txt`;
        try {
            const program = fs.readFileSync(programFile, 'utf8');
            const outputs = fs.readFileSync(outputsFile, 'utf8');
            if (!outputs.includes("@")) {
                try {
                    fs.writeFileSync(`${path}/${name}.js`, S2J.translate_program(program));
                    fs.writeFileSync(`${path}/${name}.js.txt`, S2J.translate_results(outputs));
                } catch (err) {
                    fs.writeFileSync(`${path}/${name}.js.err`, `An error occurred in translation:\n${JSON.stringify(err)}`);
                }
            } else {
                fs.writeFileSync(`${path}/${name}.js.err`, "Skipped translation because the outputs include `@`.");
            }
        } catch (err) {
            console.log(name);
            console.error(err);
        }
    }
}