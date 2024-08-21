function fun1() {
  function average(x, y) {
    return (x + y) / 2
  }
  return average
}
let x = fun1()
console.log(x(20, 40))