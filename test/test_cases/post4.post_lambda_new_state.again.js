let a = 2
function make() {
  return function (b) {
    return a + b
  }
}
let fun = make()
a = 1
console.log(fun(1))