function fun(A) {
  function whatIsA() {
    return A;
  }
  return whatIsA;
}
let whatIsA = fun(22);
let whatIsB = fun(33);
console.log(whatIsA());
console.log(whatIsB());