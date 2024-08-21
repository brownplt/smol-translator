function f1() {
  return 5
}
let f2 = f1
let f3 = f2
console.log(f3() * 10)