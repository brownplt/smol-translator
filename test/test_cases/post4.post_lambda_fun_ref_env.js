"use strict";

function k(b) {
  return function (a) {
    return a + b;
  };
}
let foo = k(3);
let bar = k(2);
console.log(foo(3));
console.log(bar(3));