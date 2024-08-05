// This program translates SMoL programs to JavaScript
//   and will do so for Python in the future.

import * as SMoL from '../src/SMoL.bs.js';
import fs from "fs";

const programFile = process.argv[2];
const program = fs.readFileSync(programFile, 'utf8');
console.log("Source program:")
console.log(program)
console.log("===========")
const translatedProgram = SMoL.CommonTranslator.translateProgram(false, program);
console.log("Target program:")
console.log(translatedProgram)
