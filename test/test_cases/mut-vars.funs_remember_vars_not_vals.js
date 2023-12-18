let x = 1;
function getx() {
  return x;
}
function setx(newVal) {
  x = newVal;
  return;
}
console.log(getx());
console.log(setx(2));
console.log(getx());