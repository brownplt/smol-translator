function add1(n) {
  return n + 1;
}
function sub1(n) {
  return n - 1;
}
let v = [ add1, sub1 ];
console.log(v[1](2));