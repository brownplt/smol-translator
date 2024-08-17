// This program translates SMoL programs to JavaScript
//   and will do so for Python in the future.

import * as SMoL from '../src/SMoL.bs.js';
import fs from "fs";


const program = `
(defvar n 0)
(deffun (f)
  (set! n (+ n 1))
  n)
(deffun (main)
  (defvar m (f))
  (deffun (g)
    (set! m (+ m 1))
    n)
  (g))
(main)
`;

console.log("=== normal usage ===");
console.log(SMoL.JSTranslator.translateProgram(false, program));

console.log("=== simulate REPL ===");
console.log(SMoL.JSTranslator.translateProgram(true, program));

console.log("=== node-to-node translation ===");
const translatedProgram = SMoL.PYTranslator.translateProgramFull(true, program);
console.log(translatedProgram.ann.print);
console.log("== f ==");
console.log(translatedProgram.it.tl.hd.ann.print);
console.log("== f's body ==");
console.log(translatedProgram.it.tl.hd.it._0._2.ann.print);

console.log("=== parse once and print multiple times ===");
const parsedProgram = SMoL.Parser.parseProgram(program);
console.log("== print JS ==")
console.log(SMoL.JSPrinter.printProgram(false, parsedProgram));
console.log("== print PY ==");
console.log(SMoL.PYPrinter.printProgram(false, parsedProgram));
