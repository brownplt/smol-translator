let x = 12;
function setAndReturn(y) {
  x = 0;
  return y;
}
console.log(setAndReturn(x));
console.log(x);