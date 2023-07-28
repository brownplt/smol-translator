let tmp = 4;
function lookup() {
  return tmp;
}
function update(val) {
  return tmp = val
}
console.log(lookup());
console.log(update(3));
console.log(lookup());