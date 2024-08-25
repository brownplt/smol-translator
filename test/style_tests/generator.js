function f(x) {
  return x + 1;
}
function* g() {
  console.log("started");
  yield f(1);
  console.log("yielded once");
  yield f(2);
  console.log("yielded twice");
  yield f(3);
  console.log("yielded third times and end");
  return;
}
let h = g();
console.log(h.next());
console.log(h.next());
console.log(h.next());
console.log(h.next());