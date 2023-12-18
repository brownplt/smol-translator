function foobar() {
  let n = 0;
  return function () {
    n = n + 1;
    return n;
  };
}
let f = foobar();
let g = foobar();
console.log(f());
console.log(f());
console.log(g());