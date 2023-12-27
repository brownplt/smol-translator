function ffx(f, x) {
  return f(f(x));
}
function inc(x) {
  return x + 1;
}
console.log(ffx(inc, 1));