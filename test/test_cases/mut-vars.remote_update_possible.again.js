let $var = 1;
function proc() {
  $var = 2;
  return;
}
console.log(proc());
console.log($var);