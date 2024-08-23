let x = 1;
function f() {
  return function (y) {
    return x + y;
  };
}
let g = f();
x = 2;
console.log(g(0));