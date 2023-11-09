let x = 5;
function set1(x, y) {
  return x = y;
}
function set2(a, y) {
  return x = y;
}
console.log(set1(x, 6));
console.log(x);
console.log(set2(x, 7));
console.log(x);