let x = 0;
let y = x;
let z = (x = x + 1, y = 4, x);
console.log(x);
console.log(y);
console.log(z);