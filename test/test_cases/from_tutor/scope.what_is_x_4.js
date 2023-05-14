let x = 1;
function main() {
  function getX() {
    return x;
  }
  let x = 2;
  return getX();
}
console.log(main());