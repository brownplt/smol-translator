function foobar() {
  let n = 0;
  function counter() {
    n = n + 1;
    return n;
  }
  return counter;
}
let f = foobar();
let g = foobar();
console.log(f());
console.log(f());
console.log(g());