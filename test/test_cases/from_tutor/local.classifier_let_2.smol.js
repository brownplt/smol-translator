console.log(((foo, getFoo)=>{return getFoo();})(123, function () {
  return foo;
}));