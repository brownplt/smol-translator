function fun() {
  let it = 32;
  function whatIsIt() {
    return it;
  }
  return whatIsIt;
}
let whatIsIt = fun();
console.log(whatIsIt());