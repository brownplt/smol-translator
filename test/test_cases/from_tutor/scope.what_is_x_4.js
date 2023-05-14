let it = 100;
function fun() {
  function whatIsIt() {
    return it;
  }
  let it = 1;
  return whatIsIt();
}
console.log(fun());