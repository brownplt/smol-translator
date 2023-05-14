let x = 2;
function main() {
  function getx() {
    return x;
  }
  let y = getx();
  let x = 3;
  return y;
}
console.log(main());