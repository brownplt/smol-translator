let x = 7;
function f(n) {
  return (n > 100 ? (1, x = x + 1) : f(n + 1) * n);
}
console.log(f(45));