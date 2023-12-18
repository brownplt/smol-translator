let $var = 1;
function proc($var) {
  $var = 2;
  return;
}
console.log(proc($var));
console.log($var);