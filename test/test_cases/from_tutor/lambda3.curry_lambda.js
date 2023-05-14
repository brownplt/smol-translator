function g(a) {
  return function (b) {
    return a + b;
  };
}
console.log(g(3)(2));