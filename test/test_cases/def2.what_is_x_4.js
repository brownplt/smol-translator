let x = 1;
function f() {
  function g() {
    return x;
  }
  let x = 2;
  return g();
}
console.log(f());