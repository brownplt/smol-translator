console.log(42);
let s1 = "foobar";
let s2 = "a string with numbers (e.g., 42) and quotes (e.g., '' and \"\")";
console.log(true);
console.log(false);
console.log(3 + 4);
console.log(3 - 4);
console.log(3 * 4);
console.log(3 / 4);
console.log(5 == 6);
console.log(5 < 6);
console.log(5 <= 6);
console.log(5 > 6);
console.log(5 >= 6);
console.log(7 == 0);
let x = 123;
function f(y) {
  return y + 1;
}
x = 456;
console.log([  ]);
console.log([ 1, [  ], [ 2, 3 ], 4 ]);
console.log([ 1, 2 ]);
console.log([ 1, 2 ]);
console.log([ 1, 2, 3 ].length);
console.log([ 1, 2, 3 ][2]);
[ 1, 2, 3 ][2] = 0;
console.log([ 3, 4 ]);
console.log([ 3, 4 ]);
console.log([ 3, 4 ][0]);
console.log([ 3, 4 ][1]);
[ 3, 4 ][0] = 5;
[ 3, 4 ][1] = 5;
console.log([ 1, 2 ] === [ 1, 2 ]);
if ("apple" === "orange") {
  console.log("yes");
} else {
  console.log("no");
}
if ("apple" === "orange") {
  console.log("yes");
} else {
  console.log("no");
}
console.log(true);
console.log(true && false && true);
console.log(false);
console.log(true || false || true);
console.log(! true);
let g = () => {
  return 3;
};
let h = (x,y) => {
  return x + y;
};
console.log(g());
console.log(h(4, 5));
console.log("foobar");
throw "bad thing happens";