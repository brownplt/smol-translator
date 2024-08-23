function foo() {
  let n = 0;
  function bar() {
    n = n + 1;
    return n;
  }
  return bar;
}
let f = foo();
let g = foo();
console.log(f());
console.log(f());
console.log(g());