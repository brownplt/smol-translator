// This file extract programs from `tutorials.js`.

import { tutorials } from "./tutorials.js";
import fs from 'fs';

let skippedCount = 0;

function skip(tutorialName, questionName) {
    skippedCount++;
    console.log(`SKIPPED ${tutorialName}.${questionName}`);
}

function writeProgram(tutorialName, questionName, program, answer) {
    // skip the whole heap and the whole local tutorials
    if (tutorialName == "heap" || tutorialName == "local") {
        skip(tutorialName, questionName);
        return;
    }
    // console.log(`${tutorialName}.${questionName}`);
    // console.log(program);
    // console.log(answer);
    // console.log("-----");
    fs.writeFileSync(`./test/test_cases/from_tutor/${tutorialName}.${questionName}.smol`, program);
    fs.writeFileSync(`./test/test_cases/from_tutor/${tutorialName}.${questionName}.smol.txt`, answer);
}

for (const tutorialName in tutorials) {
    const tutorial = tutorials[tutorialName];
    for (const k in tutorial.questions) {
        const q = tutorial.questions[k];
        if ("program" in q) {
            const { program, answer } = q;
            writeProgram(tutorialName, k, program, answer);
        }
    }
    for (const k in tutorial.questions) {
        const q = tutorial.questions[k];
        if ("again" in q) {
            const { program, answer } = q.again;
            writeProgram(tutorialName, `${k}.again`, program, answer);
        }
    }
}

console.log(`${skippedCount} programs skipped.`);