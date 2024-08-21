let a = 3
function foo(b) {
  function bar() {
    let c = 6
    return a + b + c
  }
  return bar()
}
console.log(foo(4) + 2)