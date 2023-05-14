let it = 100;
function fun() {
  let it = 1;
  return whatIsIt();
}
function whatIsIt() {
  return it;
}
console.log(fun());