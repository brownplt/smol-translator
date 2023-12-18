let var1 = [ 10, 17 ];
function updateVar(var2) {
  var2[0] = 20;
  return;
}
console.log(updateVar(var1));
console.log(var1);