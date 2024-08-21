let x = 1
function f() {
  return x
}
function g() {
  let x = 2
  return f()
}
console.log(g())