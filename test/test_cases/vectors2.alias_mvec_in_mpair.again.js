let mv = [ 63 ];
let mv2 = [ mv, mv ];
mv2[0][0] = 42;
console.log(mv2[1]);