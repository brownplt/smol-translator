let s = 1;
function updateVar(t) {
  t = 2;
  return s;
}
console.log(updateVar(s));