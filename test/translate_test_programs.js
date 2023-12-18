// This program translates SMoL programs to JavaScript
//   and will do so for Python in the future.

import * as SMoL from '../src/SMoL.bs.js';
import fs from "fs";

const path = "./test/test_cases";
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
                    fs.writeFileSync(`${path}/${name}.js`, SMoL.JSTranslator.translateProgram(program));
                    fs.writeFileSync(`${path}/${name}.js.txt`, SMoL.JSTranslator.translateTerms(outputs));
                } catch (err) {
                    fs.writeFileSync(`${path}/${name}.js.err`, `An error occurred in translation:\n${JSON.stringify(err)}\n${err.toString()}`);
                }
            } else {
                fs.writeFileSync(`${path}/${name}.js.err`, "Skipped translation because the outputs include `@`.");
            }
        } catch (err) {
            console.log(name);
            console.error(err);
        }
        try {
            const program = fs.readFileSync(programFile, 'utf8');
            const outputs = fs.readFileSync(outputsFile, 'utf8');
            if (!outputs.includes("@")) {
                try {
                    fs.writeFileSync(`${path}/${name}.py`, SMoL.PYTranslator.translateProgram(program));
                    fs.writeFileSync(`${path}/${name}.py.txt`, SMoL.PYTranslator.translateTerms(outputs));
                } catch (err) {
                    fs.writeFileSync(`${path}/${name}.py.err`, `An error occurred in translation:\n${JSON.stringify(err)}\n${err.toString()}`);
                }
            } else {
                fs.writeFileSync(`${path}/${name}.py.err`, "Skipped translation because the outputs include `@`.");
            }
        } catch (err) {
            console.log(name);
            console.error(err);
        }
        try {
            const program = fs.readFileSync(programFile, 'utf8');
            const outputs = fs.readFileSync(outputsFile, 'utf8');
            if (!outputs.includes("@")) {
                try {
                    fs.writeFileSync(`${path}/${name}.py`, SMoL.ScalaTranslator.translateProgram(program));
                    fs.writeFileSync(`${path}/${name}.py.txt`, SMoL.ScalaTranslator.translateTerms(outputs));
                } catch (err) {
                    fs.writeFileSync(`${path}/${name}.py.err`, `An error occurred in translation:\n${JSON.stringify(err)}\n${err.toString()}`);
                }
            } else {
                fs.writeFileSync(`${path}/${name}.py.err`, "Skipped translation because the outputs include `@`.");
            }
        } catch (err) {
            console.log(name);
            console.error(err);
        }
    }
}