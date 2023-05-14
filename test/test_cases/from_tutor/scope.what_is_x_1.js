let x = 1;
function main() {
  let x = 2;
  function getX() {
    return x;
  }
  return getX();
}
console.log(main());