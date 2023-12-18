let x = [ 71, 86 ];
function f(y) {
  y[0] = 34;
  return;
}
console.log(f(x));
console.log(x);