// This program translates SMoL programs to JavaScript
//   and will do so for Python in the future.

import * as SMoL from '../src/SMoL.mjs';
import fs from "fs";

const programFile = process.argv[2];
if (! (typeof programFile == "string")) {
    throw new Error("expecting a file path represented by string, given " + programFile)
}
const program = fs.readFileSync(programFile, 'utf8');
console.log("Source program:");
console.log(program);
console.log("===========");
const parsedProgram = SMoL.Parser.parseProgram(program);
console.log("Target program:");
console.log(SMoL.SMoLPrinter.printProgram(false, parsedProgram));
console.log("===========");
console.log(SMoL.PCPrinter.printProgram(false, parsedProgram));
console.log("===========");
console.log(SMoL.SCPrinter.printProgram(false, parsedProgram));
