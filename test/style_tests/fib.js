function fact() {
  return n == 0 ? 1 : (fact(n - 1) * n);
}
console.log(fact(5));