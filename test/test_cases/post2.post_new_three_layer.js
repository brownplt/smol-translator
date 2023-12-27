let n = 5;
function f1(m) {
  function f2() {
    let l = 4;
    return n + m + l;
  }
  return f2();
}
console.log(f1(1) + 3);