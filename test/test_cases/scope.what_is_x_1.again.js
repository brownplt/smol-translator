let it = 100;
function fun() {
  let it = 1;
  function whatIsIt() {
    return it;
  }
  return whatIsIt();
}
console.log(fun());