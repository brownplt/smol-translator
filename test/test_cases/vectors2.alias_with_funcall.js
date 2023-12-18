let x = [ 1, 0 ];
function f(y) {
  y[0] = 173;
  return;
}
console.log(f(x));
console.log(x);