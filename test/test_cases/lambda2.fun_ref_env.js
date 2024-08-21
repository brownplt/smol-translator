function bar(y) {
  function addy(x) {
    return x + y
  }
  return addy
}
let f = bar(2)
let g = bar(4)
console.log(f(2))
console.log(g(2))