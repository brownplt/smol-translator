function makeGetter() {
  let x = 1;
  function getX() {
    return x;
  }
  return getX;
}
let getX = makeGetter();
console.log(getX());