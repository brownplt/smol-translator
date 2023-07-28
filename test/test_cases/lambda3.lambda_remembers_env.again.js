let a = 2;
let fun = function (b) {
  return a + b;
};
a = 3;
console.log(fun(a));