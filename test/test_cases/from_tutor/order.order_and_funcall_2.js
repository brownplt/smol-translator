let a = 1;
function f() {
  function g() {
    return a;
  }
  let b = g();
  let a = 2;
  return b;
}
console.log(f());