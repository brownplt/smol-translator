function add1(n) {
  return n + 1;
}
function sub1(n) {
  return n - 1;
}
let v = mpair(add1, sub1);
console.log(right(v)(2));