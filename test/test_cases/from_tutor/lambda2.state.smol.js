let a = 2;
function makeFun() {
  function addA(b) {
    return a + b;
  }
  return addA;
}
let fun = makeFun();
a = 100;
console.log(fun(a));