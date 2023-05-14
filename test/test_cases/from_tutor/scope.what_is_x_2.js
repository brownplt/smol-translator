let it = 100;
function whatIsIt() {
  return it;
}
function fun() {
  let it = 1;
  return whatIsIt();
}
console.log(fun());