let $var = 1;
function proc($var) {
  return $var = 2;
}
console.log(proc($var));
console.log($var);