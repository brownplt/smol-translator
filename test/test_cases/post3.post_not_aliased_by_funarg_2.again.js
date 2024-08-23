let s = 1;
function foobar(t) {
  t = 2;
  return s;
}
console.log(foobar(s));