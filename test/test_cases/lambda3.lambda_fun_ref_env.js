function bar(y) {
  return (x) => {
    return x + y;
  };
}
let f = bar(2);
let g = bar(4);
console.log(f(2));
console.log(g(2));