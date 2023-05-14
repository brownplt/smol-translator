function makeAddy(y) {
  function addy(x) {
    return x + y;
  }
  return addy;
}
let f = makeAddy(10);
let g = makeAddy(50);
console.log(f(2));
console.log(g(2));