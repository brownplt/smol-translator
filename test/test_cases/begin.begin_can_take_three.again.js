let a = 2;
let b = a;
let c = (a = a * 3, b = 123, a);
console.log(a);
console.log(b);
console.log(c);