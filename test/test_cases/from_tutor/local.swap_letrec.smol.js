let a = 1;
let b = 2;
console.log(letrec(a(b * 10)(b(a * 10)), [a, b]));