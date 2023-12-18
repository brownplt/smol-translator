let f = 6;
function x() {
  function y() {
    return f;
  }
  let f = 3;
  return y();
}
console.log(x());