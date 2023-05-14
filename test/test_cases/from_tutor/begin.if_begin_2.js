let counter = 0;
function factorial(n) {
  return (n === 0 ? (1, counter = counter + 1) : factorial(n - 1) * n);
}
console.log(factorial(2));