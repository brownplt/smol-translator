let x = 1;
function gety() {
  let y = x;
  let x = 2;
  return y;
}
console.log(gety());