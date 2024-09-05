function makeCounter() {
  let n = 0;
  function inc() {
    n = n + 1;
    return n;
  }
  return inc;
}
let f = makeCounter();
console.log(f());
console.log(f());