function ffx(f x) {
  return f(f(x));
}
function add1(x) {
  return x + 1;
}
console.log(ffx(add1 1));