let x = 12;
function f() {
  return x;
}
function g() {
  x = 0;
  return f();
}
console.log(g());
x = 1;
console.log(f());