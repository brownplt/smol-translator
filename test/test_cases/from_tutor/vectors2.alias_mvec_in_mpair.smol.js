let mv = [13];
let mv2 = mpair(mv, mv);
left(mv2)[0] = 42;
console.log(right(mv2));