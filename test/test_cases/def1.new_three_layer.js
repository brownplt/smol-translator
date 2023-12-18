let x = 1;
function f(y) {
  function g() {
    let z = 2;
    return x + y + z;
  }
  return g();
}
console.log(f(3) + 4);