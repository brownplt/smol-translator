function buildDbl() {
  let n = 1;
  return () => {
    n = n * 2;
    return n;
  };
}
let dbl1 = buildDbl();
let dbl2 = buildDbl();
console.log(dbl1());
console.log(dbl2());
console.log(dbl1());