function f(x) {
  return (y) => {
    return x + y;
  };
}
let x = 0;
console.log(f(2)(1));