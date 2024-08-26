function f(x) {
  return x + 1;
}
function* g() {
  yield f(1);
  yield f(2);
  return yield f(3);
}
let h = g();
console.log(h.next());
console.log(h.next());
console.log(h.next());
console.log(h.next());