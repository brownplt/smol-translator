let a = 1
function foobar(b) {
  a = 2
  return b
}
console.log(foobar(a))
console.log(a)