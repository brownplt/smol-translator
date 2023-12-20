let x = 1;
function f() {
  let y = 2;
  function g() {
    return x + y;
  }
  return g();
}
console.log(f());