let x = [ 76, 18 ];
x[1] = x;
x[1][1][0] = 2;
console.log(x[0]);