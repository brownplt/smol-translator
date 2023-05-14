let x = 1;
function main() {
  let x = 2;
  return getX();
}
function getX() {
  return x;
}
console.log(main());