let fact = function (n) {
  if (n == 0) {
    return 1;
  } else {
    return fact(n - 1) * n;
  }
};
console.log(fact(5));