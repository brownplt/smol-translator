let m1 = [ 77, 77 ];
function f(m2) {
  m2[0] = 43;
  return m2[0];
}
let foo = f(m1);
console.log(m1);