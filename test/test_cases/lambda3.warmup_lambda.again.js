function g(a) {
  return (b) => {
    return a + b;
  };
}
console.log(g(3)(2));