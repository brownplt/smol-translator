// This file extract programs from `tutorials.js`.

import { tutorials } from "./tutorials.js";
import fs from 'fs';

let skippedCount = 0;

function skip(tutorialName, questionName) {
    skippedCount++;
    console.log(`SKIPPED ${tutorialName}.${questionName}`);
}

function writeProgram(tutorialName, questionName, program, outputs) {
    if (!(typeof program == "string" && typeof outputs == "string")) {
        console.log(tutorialName, questionName, program, outputs);
        throw Error("WTF");
    }
    // skip the whole heap and the whole local tutorials
    if (tutorialName == "heap" || tutorialName == "local" || tutorialName == "begin" || outputs.includes("@") || outputs.includes("=")) {
        skip(tutorialName, questionName);
        return;
    }
    // console.log(`${tutorialName}.${questionName}`);
    // console.log(program);
    // console.log(outputs);
    // console.log("-----");
    fs.writeFileSync(`./test/test_cases/${tutorialName}.${questionName}.smol`, program);
    fs.writeFileSync(`./test/test_cases/${tutorialName}.${questionName}.smol.txt`, outputs);
}

for (const tutorialName in tutorials) {
    const tutorial = tutorials[tutorialName];
    for (const k in tutorial.questions) {
        const q = tutorial.questions[k];
        if ("program" in q) {
            const { program, answer: outputs } = q;
            writeProgram(tutorialName, k, program, outputs);
        }
    }
    for (const k in tutorial.questions) {
        const q = tutorial.questions[k];
        if ("again" in q) {
            const { program, answer: outputs } = q.again;
            writeProgram(tutorialName, `${k}.again`, program, outputs);
        }
    }
}

console.log(`${skippedCount} programs skipped.`);