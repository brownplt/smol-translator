function f() {
  let n = 1
  function dbl() {
    n = n * 2
    return n
  }
  return dbl
}
let dbl1 = f()
let dbl2 = f()
console.log(dbl1())
console.log(dbl2())
console.log(dbl1())