function twice(f, x) {
  return f(f(x));
}
function double(x) {
  return x + x;
}
console.log(twice(double, 1));