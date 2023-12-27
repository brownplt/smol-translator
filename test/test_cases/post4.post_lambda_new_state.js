let a = 1;
function foo() {
  return function (b) {
    return a + b;
  };
}
let bar = foo();
a = 3;
console.log(bar(0));