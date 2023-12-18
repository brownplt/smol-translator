function g() {
  function add1(x) {
    return x + 1;
  }
  return add1;
}
let f = g();
console.log(f(10));