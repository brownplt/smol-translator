function inc(n) {
  return n + 1
}
function dec(n) {
  return n - 1
}
let v = [ inc, dec ]
console.log(v[1](2))