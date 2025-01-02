// This program translates SMoL programs to JavaScript
//   and will do so for Python in the future.

import * as SMoL from '../src/SMoL.mjs';
import fs from "fs";

const paths = [
    "./test/style_tests",
    "./test/test_cases",
];
const suffix = ".smol";


const Scala3_source_hard_coded_translation = JSON.parse(fs.readFileSync('../smol-scala-translations/consolidated-fixes.json'));
const Scala3_actual_hard_coded_translation = {};
const incompatibleWithScala3 = new Set([
    "mutvars1.not_aliased_by_funarg_2",
    "post3.post_not_aliased_by_funarg_2",
    "post4.post_not_aliased_by_funarg_2",
]);
const incompatibleWithPY = new Set([
    "mutvars2.update_undefined",
]);
for (const [reason, items] of Object.entries(Scala3_source_hard_coded_translation)) {
    for (const { broken, fix, task, tutorial } of items) {
        Scala3_actual_hard_coded_translation[`${tutorial}.${task}`] = fix.trim();
    }
}

for (const path of paths) {

    for (const f of fs.readdirSync(path)) {
        if (f.endsWith(suffix)) {
            const name = f.substring(0, f.length - suffix.length);
            const programFile = `${path}/${name}.smol`;
            const outputsFile = `${path}/${name}.smol.txt`;
            // PY
            try {
                if (!incompatibleWithPY.has(name.replace(/[.]again$/, ""))) {
                    const program = fs.readFileSync(programFile, 'utf8');
                    const outputs = fs.readFileSync(outputsFile, 'utf8');
                    if (!outputs.includes("@")) {
                        try {
                            fs.writeFileSync(`${path}/${name}.py`, SMoL.PYTranslator.translateProgram(true, program));
                            fs.writeFileSync(`${path}/${name}.py.txt`, SMoL.PYTranslator.translateOutput(outputs));
                        } catch (err) {
                            fs.writeFileSync(`${path}/${name}.py.err`, `An error occurred in translation:\n${SMoL.TranslateError.toString(err._1)}`);
                        }
                    } else {
                        fs.writeFileSync(`${path}/${name}.py.err`, "Skipped translation because the outputs include `@`.");
                    }
                }
            } catch (err) {
                console.log(name);
                console.error(err);
            }
            // JS
            try {
                const program = fs.readFileSync(programFile, 'utf8');
                const outputs = fs.readFileSync(outputsFile, 'utf8');
                if (!outputs.includes("@")) {
                    try {
                        fs.writeFileSync(`${path}/${name}.js`, SMoL.JSTranslator.translateProgram(true, program));
                        fs.writeFileSync(`${path}/${name}.js.txt`, SMoL.JSTranslator.translateOutput(outputs));
                    } catch (err) {
                        fs.writeFileSync(`${path}/${name}.js.err`, `An error occurred in translation:\n${SMoL.TranslateError.toString(err._1)}`);
                    }
                } else {
                    fs.writeFileSync(`${path}/${name}.js.err`, "Skipped translation because the outputs include `@`.");
                }
            } catch (err) {
                console.log(name);
                console.error(err);
            }
            // // Pseudocode
            try {
                const program = fs.readFileSync(programFile, 'utf8');
                const outputs = fs.readFileSync(outputsFile, 'utf8');
                if (!outputs.includes("@")) {
                    try {
                        fs.writeFileSync(`${path}/${name}.cm`, SMoL.PCTranslator.translateProgram(true, program));
                        fs.writeFileSync(`${path}/${name}.cm.txt`, SMoL.PCTranslator.translateOutput(outputs));
                    } catch (err) {
                        fs.writeFileSync(`${path}/${name}.cm.err`, `An error occurred in translation:\n${SMoL.TranslateError.toString(err._1)}`);
                    }
                } else {
                    fs.writeFileSync(`${path}/${name}.cm.err`, "Skipped translation because the outputs include `@`.");
                }
            } catch (err) {
                console.log(name);
                console.error(err);
            }
            // Scala
            try {
                if (!incompatibleWithScala3.has(name.replace(/[.]again$/, ""))) {
                    const program = fs.readFileSync(programFile, 'utf8');
                    const outputs = fs.readFileSync(outputsFile, 'utf8');
                    if (!outputs.includes("@")) {
                        try {
                            fs.writeFileSync(`${path}/${name}.scala`, Scala3_actual_hard_coded_translation[name] || SMoL.SCTranslator.translateProgram(true, program));
                            fs.writeFileSync(`${path}/${name}.scala.txt`, SMoL.SCTranslator.translateOutput(outputs));
                        } catch (err) {
                            fs.writeFileSync(`${path}/${name}.scala.err`, `An error occurred in translation:\n${SMoL.TranslateError.toString(err._1)}`);
                        }
                    } else {
                        fs.writeFileSync(`${path}/${name}.scala.err`, "Skipped translation because the outputs include `@`.");
                    }
                }
            } catch (err) {
                console.log(name);
                console.error(err);
            }
        }
    }
}
