"use strict";

let a = [ 55, 17 ];
function foobar(b) {
  b[0] = 52;
  return b[0];
}
let m = foobar(a);
console.log(a);