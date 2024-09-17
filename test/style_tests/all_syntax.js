console.log(42);
console.log(true);
console.log(false);
console.log("a string with numbers (e.g., 42) and quotes (e.g., '' and \"\")");
let x = 1;
console.log(x);
x = 2;
console.log(3 + 4);
console.log(3 - 4);
console.log(3 * 4);
console.log(3 / 4);
console.log(5 < 6);
console.log(5 > 6);
console.log(5 == 6);
console.log(5 != 6);
console.log([ 7, 8 ]);
let mv = [ 7, 8 ];
console.log(mv[0]);
console.log(mv[1]);
console.log(mv[0]);
mv[0] = 9;
mv[0] = 10;
mv[1] = 11;
console.log(mv.length);
console.log(! true);
console.log(false);
let f = () => {
  return 3;
};
let g = (x,y) => {
  return x + y;
};
console.log(f());
console.log(g(4, 5));
if ("apple" === "orange") {
  console.log("yes");
} else {
  console.log("no");
}
throw "bad thing happens";