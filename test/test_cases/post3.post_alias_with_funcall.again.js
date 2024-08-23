let a = [ 55, 17 ];
function foobar(b) {
  b[0] = 52;
  return;
}
console.log(foobar(a));
console.log(a);