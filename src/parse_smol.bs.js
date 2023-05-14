// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$String from "rescript/lib/es6/string.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Float from "rescript/lib/es6/belt_Float.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as S_expression from "@lukc1024/s-expression/src/s_expression.bs.js";
import * as Stringify_smol from "./stringify_smol.bs.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

var ParseError = /* @__PURE__ */Caml_exceptions.create("Parse_smol.ParseError");

function stringOfExprs(es) {
  if (es) {
    if (es.tl) {
      return "" + String(Belt_List.length(es)) + " terms: " + $$String.concat(", ", Belt_List.map(es, S_expression.stringOfSExpr)) + "";
    } else {
      return "one term: " + S_expression.stringOfSExpr(es.hd) + "";
    }
  } else {
    return "no term";
  }
}

function stringOfParseError(err) {
  switch (err.TAG | 0) {
    case /* SExprKindError */0 :
        return "expecting a " + err._1 + ", given " + S_expression.stringOfSExpr(err._2) + "";
    case /* SExprArityError */1 :
        return "expecting " + err._1 + ", given " + stringOfExprs(err._2) + "";
    case /* LiteralSymbolError */2 :
        return "expecting a literal value, given a symbol " + err._0 + "";
    case /* LiteralListError */3 :
        return "expecting a constant or a vector, given " + S_expression.stringOfSExpr(err._0) + "";
    case /* TermKindError */4 :
        return "expecting " + err._1 + ", given " + Stringify_smol.string_of_term(err._2) + "";
    
  }
}

function as_id(context, e) {
  var match = e.it;
  if (match.TAG === /* Atom */0) {
    var x = match._0;
    if (x.TAG !== /* Str */0) {
      return {
              it: x._0,
              ann: e.ann
            };
    }
    throw {
          RE_EXN_ID: ParseError,
          _1: {
            TAG: /* SExprKindError */0,
            _0: /* Symbol */0,
            _1: context,
            _2: e
          },
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: ParseError,
        _1: {
          TAG: /* SExprKindError */0,
          _0: /* Symbol */0,
          _1: context,
          _2: e
        },
        Error: new Error()
      };
}

function as_list(context, e) {
  var match = e.it;
  if (match.TAG === /* Atom */0) {
    throw {
          RE_EXN_ID: ParseError,
          _1: {
            TAG: /* SExprKindError */0,
            _0: /* List */1,
            _1: context,
            _2: e
          },
          Error: new Error()
        };
  }
  if (match._0) {
    throw {
          RE_EXN_ID: ParseError,
          _1: {
            TAG: /* SExprKindError */0,
            _0: /* List */1,
            _1: context,
            _2: e
          },
          Error: new Error()
        };
  }
  return match._2;
}

function as_one_then_many(context, es) {
  if (es) {
    return [
            es.hd,
            es.tl
          ];
  }
  throw {
        RE_EXN_ID: ParseError,
        _1: {
          TAG: /* SExprArityError */1,
          _0: /* OneThenMany */3,
          _1: context,
          _2: es
        },
        Error: new Error()
      };
}

function as_many_then_one(context, es) {
  if (es) {
    var e1 = es.hd;
    var match = Belt_List.reverse(es.tl);
    if (match) {
      return [
              {
                hd: e1,
                tl: Belt_List.reverse(match.tl)
              },
              match.hd
            ];
    } else {
      return [
              /* [] */0,
              e1
            ];
    }
  }
  throw {
        RE_EXN_ID: ParseError,
        _1: {
          TAG: /* SExprArityError */1,
          _0: /* ManyThenOne */4,
          _1: context,
          _2: es
        },
        Error: new Error()
      };
}

function as_one(context, es) {
  if (es) {
    if (es.tl) {
      throw {
            RE_EXN_ID: ParseError,
            _1: {
              TAG: /* SExprArityError */1,
              _0: /* ExactlyOne */0,
              _1: context,
              _2: es
            },
            Error: new Error()
          };
    }
    return es.hd;
  }
  throw {
        RE_EXN_ID: ParseError,
        _1: {
          TAG: /* SExprArityError */1,
          _0: /* ExactlyOne */0,
          _1: context,
          _2: es
        },
        Error: new Error()
      };
}

function as_two(context, es) {
  if (es) {
    var match = es.tl;
    if (match && !match.tl) {
      return [
              es.hd,
              match.hd
            ];
    }
    
  }
  throw {
        RE_EXN_ID: ParseError,
        _1: {
          TAG: /* SExprArityError */1,
          _0: /* ExactlyTwo */1,
          _1: context,
          _2: es
        },
        Error: new Error()
      };
}

function as_three(context, es) {
  if (es) {
    var match = es.tl;
    if (match) {
      var match$1 = match.tl;
      if (match$1 && !match$1.tl) {
        return [
                es.hd,
                match.hd,
                match$1.hd
              ];
      }
      
    }
    
  }
  throw {
        RE_EXN_ID: ParseError,
        _1: {
          TAG: /* SExprArityError */1,
          _0: /* ExactlyThree */2,
          _1: context,
          _2: es
        },
        Error: new Error()
      };
}

function as_one_then_many_then_one(context, es) {
  if (es) {
    var match = es.tl;
    if (match) {
      var e2 = match.hd;
      var e1 = es.hd;
      var match$1 = Belt_List.reverse(match.tl);
      if (match$1) {
        return [
                e1,
                {
                  hd: e2,
                  tl: Belt_List.reverse(match$1.tl)
                },
                match$1.hd
              ];
      } else {
        return [
                e1,
                /* [] */0,
                e2
              ];
      }
    }
    throw {
          RE_EXN_ID: ParseError,
          _1: {
            TAG: /* SExprArityError */1,
            _0: /* OneThenManyThenOne */5,
            _1: context,
            _2: es
          },
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: ParseError,
        _1: {
          TAG: /* SExprArityError */1,
          _0: /* OneThenManyThenOne */5,
          _1: context,
          _2: es
        },
        Error: new Error()
      };
}

var ExpectingExpression = /* @__PURE__ */Caml_exceptions.create("Parse_smol.ExpectingExpression");

function as_expr(context, e) {
  if (e.TAG !== /* Def */0) {
    return e._0;
  }
  throw {
        RE_EXN_ID: ParseError,
        _1: {
          TAG: /* TermKindError */4,
          _0: /* Expression */1,
          _1: context,
          _2: e
        },
        Error: new Error()
      };
}

function constant_of_atom(_ann, atom) {
  if (atom.TAG === /* Str */0) {
    return {
            TAG: /* Con */0,
            _0: {
              TAG: /* Str */2,
              _0: atom._0
            }
          };
  }
  var x = atom._0;
  switch (x) {
    case "#f" :
        return {
                TAG: /* Con */0,
                _0: {
                  TAG: /* Lgc */1,
                  _0: false
                }
              };
    case "#t" :
        return {
                TAG: /* Con */0,
                _0: {
                  TAG: /* Lgc */1,
                  _0: true
                }
              };
    default:
      var tryNum = Belt_Option.map(Belt_Float.fromString(x), (function (n) {
              return {
                      TAG: /* Con */0,
                      _0: {
                        TAG: /* Num */0,
                        _0: n
                      }
                    };
            }));
      if (tryNum !== undefined) {
        return tryNum;
      }
      throw {
            RE_EXN_ID: ParseError,
            _1: {
              TAG: /* LiteralSymbolError */2,
              _0: x
            },
            Error: new Error()
          };
  }
}

function value_of_sexpr(e) {
  var ann = e.ann;
  var atom = e.it;
  if (atom.TAG === /* Atom */0) {
    return {
            it: constant_of_atom(ann, atom._0),
            ann: ann
          };
  }
  if (atom._0) {
    var es = Belt_List.map(atom._2, value_of_sexpr);
    return {
            it: {
              TAG: /* AppPrm */5,
              _0: /* VecNew */15,
              _1: es
            },
            ann: ann
          };
  }
  throw {
        RE_EXN_ID: ParseError,
        _1: {
          TAG: /* LiteralListError */3,
          _0: e
        },
        Error: new Error()
      };
}

function expr_of_atom(ann, atom) {
  if (atom.TAG === /* Str */0) {
    return {
            TAG: /* Con */0,
            _0: {
              TAG: /* Str */2,
              _0: atom._0
            }
          };
  }
  var x = atom._0;
  switch (x) {
    case "#f" :
        return {
                TAG: /* Con */0,
                _0: {
                  TAG: /* Lgc */1,
                  _0: false
                }
              };
    case "#t" :
        return {
                TAG: /* Con */0,
                _0: {
                  TAG: /* Lgc */1,
                  _0: true
                }
              };
    default:
      var tryNum = Belt_Option.map(Belt_Float.fromString(x), (function (n) {
              return {
                      TAG: /* Con */0,
                      _0: {
                        TAG: /* Num */0,
                        _0: n
                      }
                    };
            }));
      return Belt_Option.getWithDefault(tryNum, {
                  TAG: /* Ref */1,
                  _0: {
                    it: x,
                    ann: ann
                  }
                });
  }
}

function term_of_sexpr(e) {
  var ann = e.ann;
  var atom = e.it;
  if (atom.TAG === /* Atom */0) {
    return {
            TAG: /* Exp */1,
            _0: {
              it: expr_of_atom(ann, atom._0),
              ann: ann
            }
          };
  }
  if (atom._0) {
    var es = Belt_List.map(atom._2, value_of_sexpr);
    return {
            TAG: /* Exp */1,
            _0: {
              it: {
                TAG: /* AppPrm */5,
                _0: /* VecNew */15,
                _1: es
              },
              ann: ann
            }
          };
  }
  var es$1 = atom._2;
  if (es$1) {
    var match = es$1.hd.it;
    if (match.TAG === /* Atom */0) {
      var match$1 = match._0;
      if (match$1.TAG !== /* Str */0) {
        var exit = 0;
        switch (match$1._0) {
          case "!=" :
              return app_prm(ann, /* Ne */9, es$1.tl);
          case "*" :
              return app_prm(ann, /* Mul */2, es$1.tl);
          case "+" :
              return app_prm(ann, /* Add */0, es$1.tl);
          case "-" :
              return app_prm(ann, /* Sub */1, es$1.tl);
          case "/" :
              return app_prm(ann, /* Div */3, es$1.tl);
          case "<" :
              return app_prm(ann, /* Lt */4, es$1.tl);
          case "<=" :
              return app_prm(ann, /* Le */7, es$1.tl);
          case "=" :
              return app_prm(ann, /* Eq */5, es$1.tl);
          case ">" :
              return app_prm(ann, /* Gt */6, es$1.tl);
          case ">=" :
              return app_prm(ann, /* Ge */8, es$1.tl);
          case "begin" :
              var match$2 = as_many_then_one("one or more expressions", es$1.tl);
              var terms = Belt_List.map(Belt_List.map(match$2[0], term_of_sexpr), (function (param) {
                      return as_expr("an expression", param);
                    }));
              var result = as_expr("an expression", term_of_sexpr(match$2[1]));
              return {
                      TAG: /* Exp */1,
                      _0: {
                        it: {
                          TAG: /* Bgn */7,
                          _0: terms,
                          _1: result
                        },
                        ann: ann
                      }
                    };
          case "cond" :
              var branches = Belt_List.map(Belt_List.map(es$1.tl, (function (param) {
                          return as_list("a `cond` branch", param);
                        })), (function (param) {
                      return as_one_then_many_then_one("the condition followed by the branch", param);
                    }));
              var _parsed = /* [] */0;
              var _branches = branches;
              while(true) {
                var branches$1 = _branches;
                var parsed = _parsed;
                if (!branches$1) {
                  return {
                          TAG: /* Exp */1,
                          _0: {
                            it: {
                              TAG: /* Cnd */9,
                              _0: Belt_List.reverse(parsed),
                              _1: undefined
                            },
                            ann: ann
                          }
                        };
                }
                var match$3 = branches$1.hd;
                var $$case = match$3[0];
                var match$4 = $$case.it;
                if (match$4.TAG === /* Atom */0) {
                  var match$5 = match$4._0;
                  if (match$5.TAG !== /* Str */0 && match$5._0 === "else" && !branches$1.tl) {
                    var terms$1 = Belt_List.map(match$3[1], term_of_sexpr);
                    var result$1 = as_expr("an expression", term_of_sexpr(match$3[2]));
                    return {
                            TAG: /* Exp */1,
                            _0: {
                              it: {
                                TAG: /* Cnd */9,
                                _0: Belt_List.reverse(parsed),
                                _1: [
                                  terms$1,
                                  result$1
                                ]
                              },
                              ann: ann
                            }
                          };
                  }
                  
                }
                var $$case$1 = as_expr("a (conditional) expression", term_of_sexpr($$case));
                var terms$2 = Belt_List.map(match$3[1], term_of_sexpr);
                var result$2 = as_expr("an expression", term_of_sexpr(match$3[2]));
                _branches = branches$1.tl;
                _parsed = {
                  hd: [
                    $$case$1,
                    [
                      terms$2,
                      result$2
                    ]
                  ],
                  tl: parsed
                };
                continue ;
              };
          case "deffun" :
              var match$6 = as_one_then_many_then_one("", es$1.tl);
              var match$7 = as_one_then_many("function name followed by parameters", as_list("function name and parameters", match$6[0]));
              var fun = as_id("a function name", match$7[0]);
              var args = Belt_List.map(match$7[1], (function (param) {
                      return as_id("a parameter", param);
                    }));
              var terms$3 = Belt_List.map(match$6[1], term_of_sexpr);
              var result$3 = as_expr("an expression to be returned", term_of_sexpr(match$6[2]));
              return {
                      TAG: /* Def */0,
                      _0: {
                        it: {
                          TAG: /* Fun */1,
                          _0: fun,
                          _1: args,
                          _2: [
                            terms$3,
                            result$3
                          ]
                        },
                        ann: ann
                      }
                    };
          case "defvar" :
              var match$8 = as_two("a variable and an expression", es$1.tl);
              var x = as_id("a variable name", match$8[0]);
              var e$1 = as_expr("an expression", term_of_sexpr(match$8[1]));
              return {
                      TAG: /* Def */0,
                      _0: {
                        it: {
                          TAG: /* Var */0,
                          _0: x,
                          _1: e$1
                        },
                        ann: ann
                      }
                    };
          case "eq?" :
          case "equal?" :
          case "eqv?" :
              exit = 2;
              break;
          case "error" :
              return app_prm(ann, /* Error */20, es$1.tl);
          case "if" :
              var match$9 = as_three("three expressions (i.e., a condition, the \"then\" branch, and the \"else\" branch)", es$1.tl);
              var e_cnd = as_expr("a (conditional) expression", term_of_sexpr(match$9[0]));
              var e_thn = as_expr("an expression", term_of_sexpr(match$9[1]));
              var e_els = as_expr("an expression", term_of_sexpr(match$9[2]));
              return {
                      TAG: /* Exp */1,
                      _0: {
                        it: {
                          TAG: /* If */8,
                          _0: e_cnd,
                          _1: e_thn,
                          _2: e_els
                        },
                        ann: ann
                      }
                    };
          case "lambda" :
              var match$10 = as_one_then_many_then_one("the function signature followed by the function body", es$1.tl);
              var args$1 = Belt_List.map(as_list("function parameters", match$10[0]), (function (param) {
                      return as_id("a parameter", param);
                    }));
              var terms$4 = Belt_List.map(match$10[1], term_of_sexpr);
              var result$4 = as_expr("an expression to be returned", term_of_sexpr(match$10[2]));
              return {
                      TAG: /* Exp */1,
                      _0: {
                        it: {
                          TAG: /* Lam */3,
                          _0: args$1,
                          _1: [
                            terms$4,
                            result$4
                          ]
                        },
                        ann: ann
                      }
                    };
          case "left" :
              return app_prm(ann, /* PairRefLeft */12, es$1.tl);
          case "let" :
              var match$11 = as_one_then_many_then_one("the bindings followed by the body", es$1.tl);
              var xes = Belt_List.map(Belt_List.map(as_list("variable-expression pairs", match$11[0]), (function (param) {
                          return as_list("a variable and an expression", param);
                        })), (function (param) {
                      return as_two("a variable and an expression", param);
                    }));
              var xes$1 = Belt_List.map(xes, (function (param) {
                      var x = as_id("a variable to be bound", param[0]);
                      var e = as_expr("an expression", term_of_sexpr(param[1]));
                      return [
                              x,
                              e
                            ];
                    }));
              var ts = Belt_List.map(match$11[1], term_of_sexpr);
              var result$5 = as_expr("an expression to be return", term_of_sexpr(match$11[2]));
              return {
                      TAG: /* Exp */1,
                      _0: {
                        it: {
                          TAG: /* Let */4,
                          _0: xes$1,
                          _1: [
                            ts,
                            result$5
                          ]
                        },
                        ann: ann
                      }
                    };
          case "mpair" :
          case "pair" :
              return app_prm(ann, /* PairNew */10, es$1.tl);
          case "quote" :
              var e$2 = as_one("a quoted value", es$1.tl);
              return {
                      TAG: /* Exp */1,
                      _0: value_of_sexpr(e$2)
                    };
          case "right" :
              return app_prm(ann, /* PairRefRight */11, es$1.tl);
          case "set!" :
              var match$12 = as_two("a variable and an expression", es$1.tl);
              var x$1 = as_id("a variable to be set", match$12[0]);
              var e$3 = as_expr("an expression", term_of_sexpr(match$12[1]));
              return {
                      TAG: /* Exp */1,
                      _0: {
                        it: {
                          TAG: /* Set */2,
                          _0: x$1,
                          _1: e$3
                        },
                        ann: ann
                      }
                    };
          case "set-left!" :
              return app_prm(ann, /* PairSetLeft */14, es$1.tl);
          case "set-right!" :
              return app_prm(ann, /* PairSetRight */13, es$1.tl);
          case "mvec" :
          case "vec" :
              return app_prm(ann, /* VecNew */15, es$1.tl);
          case "vec-len" :
          case "vlen" :
              return app_prm(ann, /* VecLen */18, es$1.tl);
          case "vec-ref" :
          case "vref" :
              return app_prm(ann, /* VecRef */16, es$1.tl);
          case "vec-set!" :
          case "vset!" :
              return app_prm(ann, /* VecSet */17, es$1.tl);
          default:
            
        }
        if (exit === 2) {
          return app_prm(ann, /* Eqv */19, es$1.tl);
        }
        
      }
      
    }
    
  }
  var match$13 = as_one_then_many("a function call/application, which includes a function and then one ore more arguments", es$1);
  var e$4 = as_expr("a function", term_of_sexpr(match$13[0]));
  var es$2 = Belt_List.map(Belt_List.map(match$13[1], term_of_sexpr), (function (param) {
          return as_expr("an argument", param);
        }));
  return {
          TAG: /* Exp */1,
          _0: {
            it: {
              TAG: /* App */6,
              _0: e$4,
              _1: es$2
            },
            ann: ann
          }
        };
}

function app_prm(ann, p, es) {
  var es$1 = Belt_List.map(Belt_List.map(es, term_of_sexpr), (function (param) {
          return as_expr("an argument", param);
        }));
  return {
          TAG: /* Exp */1,
          _0: {
            it: {
              TAG: /* AppPrm */5,
              _0: p,
              _1: es$1
            },
            ann: ann
          }
        };
}

function terms_of_sexprs(es) {
  return Belt_List.map(es, term_of_sexpr);
}

function parse_terms(src) {
  return Belt_List.map(S_expression.parseMany(S_expression.stringAsSource(src)), term_of_sexpr);
}

export {
  ParseError ,
  stringOfExprs ,
  stringOfParseError ,
  as_id ,
  as_list ,
  as_one_then_many ,
  as_many_then_one ,
  as_one ,
  as_two ,
  as_three ,
  as_one_then_many_then_one ,
  ExpectingExpression ,
  as_expr ,
  constant_of_atom ,
  value_of_sexpr ,
  expr_of_atom ,
  term_of_sexpr ,
  app_prm ,
  terms_of_sexprs ,
  parse_terms ,
}
/* No side effect */
