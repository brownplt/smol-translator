let a = 4
function h() {
  return a
}
function k() {
  a = 2
  return h()
}
console.log(k())
a = 6
console.log(h())