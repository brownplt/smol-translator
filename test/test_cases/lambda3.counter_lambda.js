function makeCounter(count) {
  return function () {
    count = count + 1
    return count;
  };
}
let f = makeCounter(0);
let g = makeCounter(0);
console.log(f());
console.log(f());
console.log(g());