let x = 12;
function f() {
  x = 0;
  return;
}
console.log(f());
console.log(x);