let x = 12;
function setAndReturn(y) {
  y = 0;
  return x;
}
console.log(setAndReturn(x));