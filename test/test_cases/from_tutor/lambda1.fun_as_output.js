function add1(x) {
  return x + 1;
}
function g() {
  return add1;
}
let f = g();
console.log(f(100));