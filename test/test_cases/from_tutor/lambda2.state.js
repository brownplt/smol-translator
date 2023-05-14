let x = 1;
function makeF() {
  function addx(y) {
    return x + y;
  }
  return addx;
}
let f = makeF();
x = 2;
console.log(f(x));