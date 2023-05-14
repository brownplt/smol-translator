// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Format from "./format.bs.js";
import * as $$String from "rescript/lib/es6/string.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Parse_smol from "./parse_smol.bs.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function string_of_constant(c) {
  if (typeof c === "number") {
    return "None";
  }
  switch (c.TAG | 0) {
    case /* Num */0 :
        return String(c._0);
    case /* Lgc */1 :
        if (c._0) {
          return "True";
        } else {
          return "False";
        }
    case /* Str */2 :
        return "\"" + $$String.escaped(c._0) + "\"";
    
  }
}

function string_of_list(ss) {
  return "(" + $$String.concat(" ", ss) + ")";
}

function string_of_identifier(x) {
  if (x === "-") {
    return x;
  }
  var re = /-/g;
  var matchFn = function (_matchPart, _offset, _wholeString) {
    return "_";
  };
  return x.replace(re, matchFn);
}

function string_of_def_var(x, e) {
  return "" + string_of_identifier(x.it) + " = " + e + "";
}

function string_of_def_fun(f, xs, b) {
  return "def " + f + "" + string_of_list(xs) + ":\n    " + Format.indent(b, 4) + "";
}

function string_of_expr_set(x, e) {
  return "" + x + " := " + e + "";
}

function string_of_expr_lam(xs, b) {
  if (xs === /* [] */0) {
    return "lambda: " + b + "";
  } else {
    return "lambda " + $$String.concat(",", xs) + ": " + b + "";
  }
}

function string_of_expr_app_prm(p, es) {
  switch (p) {
    case /* Add */0 :
        return "" + $$String.concat(" + ", es) + "";
    case /* Sub */1 :
        return "" + $$String.concat(" - ", es) + "";
    case /* Mul */2 :
        return "" + $$String.concat(" * ", es) + "";
    case /* Div */3 :
        return "" + $$String.concat(" / ", es) + "";
    case /* Lt */4 :
        if (!es) {
          return "/* a primitive operation not supported yet */";
        }
        var match = es.tl;
        if (match && !match.tl) {
          return "" + es.hd + " < " + match.hd + "";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* Eq */5 :
        if (!es) {
          return "/* a primitive operation not supported yet */";
        }
        var match$1 = es.tl;
        if (match$1 && !match$1.tl) {
          return "" + es.hd + " is " + match$1.hd + "";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* Gt */6 :
        if (!es) {
          return "/* a primitive operation not supported yet */";
        }
        var match$2 = es.tl;
        if (match$2 && !match$2.tl) {
          return "" + es.hd + " > " + match$2.hd + "";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* Le */7 :
        if (!es) {
          return "/* a primitive operation not supported yet */";
        }
        var match$3 = es.tl;
        if (match$3 && !match$3.tl) {
          return "" + es.hd + " <= " + match$3.hd + "";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* Ge */8 :
        if (!es) {
          return "/* a primitive operation not supported yet */";
        }
        var match$4 = es.tl;
        if (match$4 && !match$4.tl) {
          return "" + es.hd + " >= " + match$4.hd + "";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* Ne */9 :
        if (!es) {
          return "/* a primitive operation not supported yet */";
        }
        var match$5 = es.tl;
        if (match$5 && !match$5.tl) {
          return "" + es.hd + " != " + match$5.hd + "";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* PairNew */10 :
    case /* PairRefRight */11 :
    case /* PairRefLeft */12 :
    case /* PairSetRight */13 :
    case /* PairSetLeft */14 :
        return "/* a primitive operation not supported yet */";
    case /* VecNew */15 :
        return "[" + $$String.concat(", ", es) + "]";
    case /* VecRef */16 :
        if (!es) {
          return "/* a primitive operation not supported yet */";
        }
        var match$6 = es.tl;
        if (match$6 && !match$6.tl) {
          return "" + es.hd + "[" + match$6.hd + "]";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* VecSet */17 :
        if (!es) {
          return "/* a primitive operation not supported yet */";
        }
        var match$7 = es.tl;
        if (!match$7) {
          return "/* a primitive operation not supported yet */";
        }
        var match$8 = match$7.tl;
        if (match$8 && !match$8.tl) {
          return "" + es.hd + "[" + match$7.hd + "] = " + match$8.hd + "";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* VecLen */18 :
        if (es && !es.tl) {
          return "" + es.hd + ".length";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* Eqv */19 :
        if (!es) {
          return "/* a primitive operation not supported yet */";
        }
        var match$9 = es.tl;
        if (match$9 && !match$9.tl) {
          return "" + es.hd + " is " + match$9.hd + "";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    case /* Error */20 :
        if (es && !es.tl) {
          return "raise " + es.hd + "";
        } else {
          return "/* a primitive operation not supported yet */";
        }
    
  }
}

function string_of_expr_app(e, es) {
  return "" + e + "" + string_of_list(es) + "";
}

function string_of_expr_bgn(es, e) {
  return "(" + $$String.concat(", ", Belt_List.concatMany([
                  es,
                  {
                    hd: e,
                    tl: /* [] */0
                  }
                ])) + ")[-1]";
}

function string_of_expr_cnd(ebs, ob) {
  var ob$1 = ob !== undefined ? "else:\n    " + Format.indent(ob, 4) + "" : "";
  var ebs$1 = Belt_List.map(ebs, (function (param) {
          return "if " + param[0] + ":\n    " + Format.indent(param[1], 2) + "\n";
        }));
  var ebs$2 = $$String.concat("el", ebs$1);
  return ebs$2 + ob$1;
}

function string_of_expr_if(e_cnd, e_thn, e_els) {
  return "" + e_thn + " if " + e_cnd + " else " + e_els + "";
}

function string_of_expr_let(_xes, _b) {
  return "\"...a let-expression...\"";
}

function maybe_wrap(ctx, code) {
  if (typeof ctx === "number" || !ctx._0) {
    return code;
  } else {
    return "(" + code + ")";
  }
}

function consider_context(ctx, code) {
  if (typeof ctx === "number" && ctx >= 2) {
    return "return " + code + "";
  } else {
    return code;
  }
}

function string_of_expr(ctx, e) {
  var c = e.it;
  switch (c.TAG | 0) {
    case /* Con */0 :
        return consider_context(ctx, string_of_constant(c._0));
    case /* Ref */1 :
        return consider_context(ctx, string_of_identifier(c._0.it));
    case /* Set */2 :
        return consider_context(ctx, string_of_expr_set(string_of_identifier(Format.unannotate(c._0)), string_of_expr(/* Expr */{
                            _0: false
                          }, c._1)));
    case /* Lam */3 :
        var b = c._1;
        var b$1 = b[0] ? "..." : string_of_expr(/* Expr */{
                _0: false
              }, b[1]);
        return consider_context(ctx, string_of_expr_lam(Belt_List.map(Belt_List.map(c._0, Format.unannotate), string_of_identifier), b$1));
    case /* Let */4 :
        return consider_context(ctx, (string_of_block(/* Return */2, c._1), Belt_List.map(c._0, string_of_xe), "\"...a let-expression...\""));
    case /* AppPrm */5 :
        var p = c._0;
        var partial_arg = /* Expr */{
          _0: true
        };
        var o = maybe_wrap(ctx, string_of_expr_app_prm(p, Belt_List.map(c._1, (function (param) {
                        return string_of_expr(partial_arg, param);
                      }))));
        if (p !== /* Error */20) {
          return consider_context(ctx, o);
        } else {
          return o;
        }
    case /* App */6 :
        var partial_arg$1 = /* Expr */{
          _0: false
        };
        return consider_context(ctx, string_of_expr_app(string_of_expr(/* Expr */{
                            _0: false
                          }, c._0), Belt_List.map(c._1, (function (param) {
                              return string_of_expr(partial_arg$1, param);
                            }))));
    case /* Bgn */7 :
        var partial_arg$2 = /* Expr */{
          _0: false
        };
        return consider_context(ctx, string_of_expr_bgn(Belt_List.map(c._0, (function (param) {
                              return string_of_expr(partial_arg$2, param);
                            })), string_of_expr(/* Expr */{
                            _0: false
                          }, c._1)));
    case /* If */8 :
        return consider_context(ctx, string_of_expr_if(string_of_expr(/* Expr */{
                            _0: false
                          }, c._0), string_of_expr(/* Expr */{
                            _0: false
                          }, c._1), string_of_expr(/* Expr */{
                            _0: false
                          }, c._2)));
    case /* Cnd */9 :
        if (typeof ctx === "number") {
          return string_of_expr_cnd(Belt_List.map(c._0, (function (param) {
                            return string_of_eb(ctx, param);
                          })), Belt_Option.map(c._1, (function (param) {
                            return string_of_block(ctx, param);
                          })));
        } else {
          return "if...";
        }
    
  }
}

function string_of_def(d) {
  var match = d.it;
  if (match.TAG === /* Var */0) {
    return string_of_def_var(match._0, string_of_expr(/* Expr */{
                    _0: false
                  }, match._1));
  } else {
    return string_of_def_fun(string_of_identifier(Format.unannotate(match._0)), Belt_List.map(Belt_List.map(match._1, Format.unannotate), string_of_identifier), string_of_block(/* Return */2, match._2));
  }
}

function string_of_block(ctx, b) {
  return $$String.concat("\n", Belt_List.concatMany([
                  Belt_List.map(b[0], string_of_term),
                  {
                    hd: string_of_expr(ctx, b[1]),
                    tl: /* [] */0
                  }
                ]));
}

function string_of_term(t) {
  if (t.TAG === /* Def */0) {
    return string_of_def(t._0);
  } else {
    return string_of_expr(/* Stat */1, t._0);
  }
}

function string_of_xe(xe) {
  return [
          string_of_identifier(xe[0].it),
          string_of_expr(/* Expr */{
                _0: false
              }, xe[1])
        ];
}

function string_of_eb(ctx, eb) {
  return [
          string_of_expr(/* Expr */{
                _0: false
              }, eb[0]),
          string_of_block(ctx, eb[1])
        ];
}

function string_of_ob(ctx, ob) {
  return Belt_Option.map(ob, (function (param) {
                return string_of_block(ctx, param);
              }));
}

function string_of_top_level(ts) {
  return $$String.concat("\n", Belt_List.map(ts, string_of_term));
}

function as_many_then_one(es) {
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
        RE_EXN_ID: Format.Impossible,
        _1: "unsafe",
        Error: new Error()
      };
}

function smol_to_py(ctx, smol_program) {
  var ts = Parse_smol.parse_terms(smol_program);
  if (typeof ctx !== "number") {
    if (ts && !ts.tl) {
      return string_of_expr(ctx, Parse_smol.as_expr("", ts.hd));
    } else {
      return "...expecting exactly one expression...";
    }
  }
  switch (ctx) {
    case /* Term */0 :
        if (ts && !ts.tl) {
          return string_of_term(ts.hd);
        } else {
          return "...expecting exactly one term...";
        }
    case /* Stat */1 :
        return string_of_top_level(ts);
    case /* Return */2 :
        var match = as_many_then_one(ts);
        var e = Parse_smol.as_expr("", match[1]);
        return string_of_block(/* Return */2, [
                    match[0],
                    e
                  ]);
    
  }
}

function translate_program(program) {
  var ts = Parse_smol.parse_terms(program);
  return $$String.concat("\n", Belt_List.map(ts, (function (t) {
                    if (t.TAG === /* Def */0) {
                      return string_of_term(t);
                    } else {
                      return "console.log(" + string_of_expr(/* Expr */{
                                  _0: false
                                }, t._0) + ")";
                    }
                  })));
}

export {
  string_of_constant ,
  string_of_list ,
  string_of_identifier ,
  string_of_def_var ,
  string_of_def_fun ,
  string_of_expr_set ,
  string_of_expr_lam ,
  string_of_expr_app_prm ,
  string_of_expr_app ,
  string_of_expr_bgn ,
  string_of_expr_cnd ,
  string_of_expr_if ,
  string_of_expr_let ,
  maybe_wrap ,
  consider_context ,
  string_of_expr ,
  string_of_def ,
  string_of_xe ,
  string_of_eb ,
  string_of_ob ,
  string_of_block ,
  string_of_term ,
  string_of_top_level ,
  as_many_then_one ,
  smol_to_py ,
  translate_program ,
}
/* No side effect */
