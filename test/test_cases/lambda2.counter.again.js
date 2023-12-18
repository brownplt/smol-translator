function f(n) {
  function dbl() {
    n = n * 2;
    return n;
  }
  return dbl;
}
let dbl1 = f(1);
let dbl2 = f(1);
console.log(dbl1());
console.log(dbl2());
console.log(dbl1());