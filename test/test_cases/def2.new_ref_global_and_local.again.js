let a = 9
function fun() {
  let b = 2
  function prod() {
    return a * b
  }
  return prod()
}
console.log(fun())