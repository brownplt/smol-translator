import * as SMoL from '../src/SMoL.mjs';
import fs from "fs";

const dirs = [
    "./test/style_tests",
    "./test/test_cases",
];

for (const dir of dirs) {
    for (let file of fs.readdirSync(dir)) {
        if (!file.endsWith(".smol")) {
            continue;
        }
        file = file.substring(0, file.length - ".smol".length);
        const source = fs.readFileSync(`${dir}/${file}.smol`, 'utf8');
        try {
            fs.writeFileSync(`${dir}/${file}.js`, SMoL.JSTranslator.translateProgram(true, source));
        } catch (err) {
            fs.writeFileSync(`${dir}/${file}.js.err`, `An error occurred in translation:\n${SMoL.TranslateError.toString(err._1)}`);
        }
        try {
            fs.writeFileSync(`${dir}/${file}.py`, SMoL.PYTranslator.translateProgram(true, source));
        } catch (err) {
            fs.writeFileSync(`${dir}/${file}.py.err`, `An error occurred in translation:\n${SMoL.TranslateError.toString(err._1)}`);
        }
        try {
            fs.writeFileSync(`${dir}/${file}.pseudo`, SMoL.PCTranslator.translateProgram(true, source));
        } catch (err) {
            fs.writeFileSync(`${dir}/${file}.pseudo.err`, `An error occurred in translation:\n${SMoL.TranslateError.toString(err._1)}`);
        }
        try {
            fs.writeFileSync(`${dir}/${file}.scala`, SMoL.SCTranslator.translateProgram(true, source));
        } catch (err) {
            fs.writeFileSync(`${dir}/${file}.scala.err`, `An error occurred in translation:\n${SMoL.TranslateError.toString(err._1)}`);
        }
        
    }
}