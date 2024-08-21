let s = 21
function i() {
  return s
}
function j() {
  let s = 76
  return i()
}
console.log(j())