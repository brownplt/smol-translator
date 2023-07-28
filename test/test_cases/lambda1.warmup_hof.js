function get() {
  return 42;
}
let f = get;
let g = f;
console.log(g());