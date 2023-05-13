function average(x y) {
  return (x + y) / 2;
}
function fun1() {
  return average;
}
let x = fun1();
console.log(x(200 400));