// This program translates SMoL programs to JavaScript
//   and will do so for Python in the future.

import * as SMoL from '../src/SMoL.bs.js';
import fs from "fs";

const programFile = process.argv[2];
const program = fs.readFileSync(programFile, 'utf8');
console.log("Source program:");
console.log(program);
console.log("===========");
const parsedProgram = SMoL.Parser.parseProgram(program);
console.log("Target program:");
console.log(SMoL.SMoLPrinter.printProgram(false, parsedProgram));
console.log("===========");
console.log(SMoL.CommonPrinter.printProgram(false, parsedProgram));
