let n = 7;
function foo() {
  return n;
}
function bar() {
  n = 3;
  return foo();
}
console.log(bar());
n = 5;
console.log(foo());