let x = 1;
function getX() {
  return x;
}
function main() {
  let x = 2;
  return getX();
}
console.log(main());