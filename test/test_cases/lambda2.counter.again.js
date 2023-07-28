function buildDbl(n) {
  function dbl() {
    n = n * 2
    return n;
  }
  return dbl;
}
let dbl1 = buildDbl(1);
let dbl2 = buildDbl(1);
console.log(dbl1());
console.log(dbl2());
console.log(dbl1());