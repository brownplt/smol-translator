function f(n) {
  function g(m) {
    return m * n;
  }
  return g;
}
let fun1 = f(10);
let fun2 = f(1);
console.log(fun1(4));
console.log(fun2(4));