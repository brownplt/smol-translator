function inc(n) {
  return n + 1
}
let v = [ inc, inc ]
console.log(v[0](2))