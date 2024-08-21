let aa = 3
function abc(bb) {
  function h() {
    let cc = 2
    return aa * bb * cc
  }
  return h()
}
console.log(abc(4) * 1)