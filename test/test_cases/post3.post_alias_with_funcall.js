"use strict";

let zz = [ 88, 88 ];
function f(aa) {
  aa[0] = 97;
  return aa[0];
}
let oo = f(zz);
console.log(zz);