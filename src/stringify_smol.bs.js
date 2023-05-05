// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$String from "rescript/lib/es6/string.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Js_string from "rescript/lib/es6/js_string.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function unannotate(x) {
  return x.it;
}

function indent(s, i) {
  var pad = Js_string.repeat(i, " ");
  return Js_string.replaceByRe(/\n/g, "\n" + pad, s);
}

function string_of_constant(c) {
  if (typeof c === "number") {
    return "#<void>";
  }
  switch (c.TAG | 0) {
    case /* Num */0 :
        return String(c._0);
    case /* Lgc */1 :
        if (c._0) {
          return "#t";
        } else {
          return "#f";
        }
    case /* Str */2 :
        return "\"" + $$String.escaped(c._0) + "\"";
    
  }
}

function string_of_prm(o) {
  switch (o) {
    case /* Add */0 :
        return "+";
    case /* Sub */1 :
        return "-";
    case /* Mul */2 :
        return "*";
    case /* Div */3 :
        return "/";
    case /* Lt */4 :
        return "<";
    case /* Eq */5 :
        return "=";
    case /* Gt */6 :
        return ">";
    case /* Le */7 :
        return "<=";
    case /* Ge */8 :
        return ">=";
    case /* Ne */9 :
        return "!=";
    case /* VecNew */10 :
        return "vec";
    case /* VecRef */11 :
        return "vec-ref";
    case /* VecSet */12 :
        return "vec-set!";
    case /* VecLen */13 :
        return "vec-len";
    case /* Eqv */14 :
        return "eq?";
    case /* Error */15 :
        return "error";
    
  }
}

function string_of_list(ss) {
  return "(" + $$String.concat(" ", ss) + ")";
}

function string_of_def_var(x, e) {
  return string_of_list({
              hd: "defvar",
              tl: {
                hd: x.it,
                tl: {
                  hd: e,
                  tl: /* [] */0
                }
              }
            });
}

function string_of_def_for(x, e_from, e_to, body) {
  return "(for " + x.it + " " + e_from + " " + e_to + "\n" + indent(body, 2) + ")";
}

function string_of_def_fun(f, xs, b) {
  return "(deffun " + string_of_list({
              hd: f,
              tl: xs
            }) + "\n  " + indent(b, 2) + ")";
}

function string_of_expr_set(x, e) {
  return string_of_list({
              hd: "set!",
              tl: {
                hd: x,
                tl: {
                  hd: e,
                  tl: /* [] */0
                }
              }
            });
}

function string_of_expr_lam(xs, b) {
  return "(lambda " + string_of_list(xs) + "\n  " + indent(b, 2) + ")";
}

function string_of_expr_app(e, es) {
  return string_of_list({
              hd: e,
              tl: es
            });
}

function string_of_expr_bgn(es, e) {
  var b = $$String.concat("\n", Belt_List.concatMany([
            es,
            {
              hd: e,
              tl: /* [] */0
            }
          ]));
  return "(begin\n  " + indent(b, 2) + ")";
}

function string_of_expr_whl(e, b) {
  return "(while " + e + "\n  " + indent(b, 2) + ")";
}

function string_of_expr_cnd(ebs, ob) {
  var ebs$1 = ob !== undefined ? Belt_List.concatMany([
          ebs,
          {
            hd: [
              "else",
              ob
            ],
            tl: /* [] */0
          }
        ]) : ebs;
  var ebs$2 = Belt_List.map(ebs$1, (function (param) {
          return "[" + param[0] + "\n " + indent(param[1], 1) + "]";
        }));
  var ebs$3 = $$String.concat("\n", ebs$2);
  return "(cond\n  " + indent(ebs$3, 2) + ")";
}

function string_of_expr_if(e_cnd, e_thn, e_els) {
  return "(if " + indent(e_cnd, 4) + "\n    " + indent(e_thn, 4) + "\n    " + indent(e_els, 4) + ")";
}

function string_of_expr_let(xes, b) {
  var xes$1 = Belt_List.map(xes, (function (param) {
          var x = param[0].it;
          return "[" + x + " " + indent(param[1], 2 + x.length | 0) + "]";
        }));
  var xes$2 = $$String.concat("\n", xes$1);
  return "(let " + indent(xes$2, 5) + "\n" + indent(b, 2) + ")";
}

function string_of_expr(e) {
  var c = e.it;
  switch (c.TAG | 0) {
    case /* Con */0 :
        return string_of_constant(c._0);
    case /* Ref */1 :
        return c._0.it;
    case /* Set */2 :
        return string_of_expr_set(c._0.it, string_of_expr(c._1));
    case /* Lam */3 :
        return string_of_expr_lam(Belt_List.map(c._0, unannotate), string_of_block(c._1));
    case /* Let */4 :
        return string_of_expr_let(Belt_List.map(c._0, string_of_xe), string_of_block(c._1));
    case /* AppPrm */5 :
        var es = Belt_List.map(c._1, string_of_expr);
        var e$1 = string_of_prm(c._0);
        return string_of_list({
                    hd: e$1,
                    tl: es
                  });
    case /* App */6 :
        var es$1 = Belt_List.map(c._1, string_of_expr);
        var e$2 = string_of_expr(c._0);
        return string_of_list({
                    hd: e$2,
                    tl: es$1
                  });
    case /* Bgn */7 :
        return string_of_expr_bgn(Belt_List.map(c._0, string_of_expr), string_of_expr(c._1));
    case /* If */8 :
        return string_of_expr_if(string_of_expr(c._0), string_of_expr(c._1), string_of_expr(c._2));
    case /* Cnd */9 :
        return string_of_expr_cnd(Belt_List.map(c._0, string_of_eb), Belt_Option.map(c._1, string_of_block));
    
  }
}

function string_of_def(d) {
  var match = d.it;
  if (match.TAG === /* Var */0) {
    return string_of_def_var(match._0, string_of_expr(match._1));
  } else {
    return string_of_def_fun(match._0.it, Belt_List.map(match._1, unannotate), string_of_block(match._2));
  }
}

function string_of_term(t) {
  if (t.TAG === /* Def */0) {
    return string_of_def(t._0);
  } else {
    return string_of_expr(t._0);
  }
}

function string_of_block(b) {
  return $$String.concat("\n", Belt_List.concatMany([
                  Belt_List.map(b[0], string_of_term),
                  {
                    hd: string_of_expr(b[1]),
                    tl: /* [] */0
                  }
                ]));
}

function string_of_xe(xe) {
  return [
          xe[0],
          string_of_expr(xe[1])
        ];
}

function string_of_eb(eb) {
  return [
          string_of_expr(eb[0]),
          string_of_block(eb[1])
        ];
}

function string_of_ob(ob) {
  return Belt_Option.map(ob, string_of_block);
}

export {
  unannotate ,
  indent ,
  string_of_constant ,
  string_of_prm ,
  string_of_list ,
  string_of_def_var ,
  string_of_def_for ,
  string_of_def_fun ,
  string_of_expr_set ,
  string_of_expr_lam ,
  string_of_expr_app ,
  string_of_expr_bgn ,
  string_of_expr_whl ,
  string_of_expr_cnd ,
  string_of_expr_if ,
  string_of_expr_let ,
  string_of_expr ,
  string_of_def ,
  string_of_xe ,
  string_of_eb ,
  string_of_ob ,
  string_of_block ,
  string_of_term ,
}
/* No side effect */
