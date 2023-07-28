let x = 0;
function fib(n) {
  return (n <= 1 ? 1 : (x = x + 1, fib(n - 1) + fib(n - 2)));
}
console.log(fib(2));