function makeGetter(x) {
  function getX() {
    return x;
  }
  return getX;
}
let getA = makeGetter(1);
let getB = makeGetter(2);
console.log(getA());
console.log(getB());