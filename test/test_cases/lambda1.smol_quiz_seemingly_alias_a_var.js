let x = 5;
function set1(x, y) {
  x = y;
  return;
}
function set2(a, y) {
  x = y;
  return;
}
console.log(set1(x, 6));
console.log(x);
console.log(set2(x, 7));
console.log(x);