let x = 12;
function f(x) {
  x = 0;
  return;
}
console.log(f(x));
console.log(x);