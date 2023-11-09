function makeCounter(count) {
  function counter() {
    count = count + 1;
    return count;
  }
  return counter;
}
let f = makeCounter(0);
let g = makeCounter(0);
console.log(f());
console.log(f());
console.log(g());