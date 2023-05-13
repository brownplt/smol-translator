import { tutorials } from "./tutorials.js";
import fs from 'fs';

function writeProgram(tutorialName, questionName, program, answer) {
    // console.log(`${tutorialName}.${questionName}`);
    // console.log(program);
    // console.log(answer);
    // console.log("-----");
    fs.writeFileSync(`./test/test_cases/from_tutor/${tutorialName}.${questionName}.smol`, program);
    fs.writeFileSync(`./test/test_cases/from_tutor/${tutorialName}.${questionName}.txt`, answer);
}

for (const tutorialName in tutorials) {
    const tutorial = tutorials[tutorialName];
    let testText = "";
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
            writeProgram(tutorialName, k, program, answer);
        }
    }
    for (const k in tutorial.questions) {
        if (!tutorial.order.includes(k)) {
            alert(`The question ${k} is not included!`);
        }
    }
}