let a = [ 66, 54 ];
function h(b) {
  a[0] = 42;
  return b;
}
console.log(h(a));