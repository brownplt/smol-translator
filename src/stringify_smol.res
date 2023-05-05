open Utilities
open Smol
open Belt
open List

let unannotate = x => x.it

let indent = (s, i) => {
  let pad = Js.String.repeat(i, " ")
  Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s)
}

let string_of_constant = c => {
  switch c {
  | Uni => "#<void>"
  | Num(n) => Float.toString(n)
  | Lgc(l) =>
    if l {
      "#t"
    } else {
      "#f"
    }
  | Str(s) => "\"" ++ String.escaped(s) ++ "\""
  }
}

let string_of_prm = (o: primitive) => {
  switch o {
  | Add => "+"
  | Sub => "-"
  | Mul => "*"
  | Div => "/"
  | Lt => "<"
  | Eq => "="
  | Gt => ">"
  | Le => "<="
  | Ge => ">="
  | Ne => "!="
  | VecNew => "vec"
  | VecRef => "vec-ref"
  | VecSet => "vec-set!"
  | VecLen => "vec-len"
  | Eqv => "eq?"
  | Error => "error"
  }
}

let string_of_list = ss => {
  "(" ++ String.concat(" ", ss) ++ ")"
}

let string_of_def_var = (x, e) => {
  string_of_list(list{"defvar", x.it, e})
}

let string_of_def_for = (x, e_from, e_to, body) => {
  `(for ${x.it} ${e_from} ${e_to}\n${indent(body, 2)})`
}

let string_of_def_fun = (f, xs, b) => {
  // string_of_list(list{"deffun", string_of_list(list{f, ...xs}), b})
  "(" ++ "deffun" ++ " " ++ string_of_list(list{f, ...xs}) ++ "\n  " ++ indent(b, 2) ++ ")"
}

let string_of_expr_set = (x, e) => {
  string_of_list(list{"set!", x, e})
}

let string_of_expr_lam = (xs, b) => {
  // if String.contains(b, '\n') {
  "(" ++ "lambda" ++ " " ++ string_of_list(xs) ++ "\n  " ++ indent(b, 2) ++ ")"
  // } else {
  // "(" ++ "lambda" ++ " " ++ string_of_list(xs) ++ " " ++ b ++ ")"
  // }
}

let string_of_expr_app = (e, es) => {
  string_of_list(list{e, ...es})
}

let string_of_expr_bgn = (es, e) => {
  let b = String.concat("\n", list{...es, e})
  "(begin\n  " ++ indent(b, 2) ++ ")"
}

let string_of_expr_whl = (e, b) => {
  "(while " ++ e ++ "\n  " ++ indent(b, 2) ++ ")"
}

let string_of_expr_cnd = (ebs: list<(string, string)>, ob) => {
  let ebs = {
    switch ob {
    | None => ebs
    | Some(b) => list{...ebs, ("else", b)}
    }
  }
  let ebs = ebs->map(((e, b)) => `[${e}\n ${indent(b, 1)}]`)
  let ebs = String.concat("\n", ebs)
  "(" ++ "cond\n  " ++ indent(ebs, 2) ++ ")"
}

let string_of_expr_if = (e_cnd: string, e_thn: string, e_els: string) => {
  `(if ${indent(e_cnd, 4)}\n    ${indent(e_thn, 4)}\n    ${indent(e_els, 4)})`
}

let string_of_expr_let = (xes, b) => {
  let xes = xes->map(((x, e)) => {
    let x = unannotate(x)
    `[${x} ${indent(e, 2 + String.length(x))}]`
  })
  let xes = String.concat("\n", xes)
  `(let ${indent(xes, 5)}\n${indent(b, 2)})`
}

let rec string_of_expr = (e: annotated<expression>): string => {
  switch e.it {
  | Con(c) => string_of_constant(c)
  | Ref(x) => x.it
  | Set(x, e) => string_of_expr_set(x->unannotate, string_of_expr(e))
  | Lam(xs, b) => string_of_expr_lam(xs->map(unannotate), string_of_block(b))
  | AppPrm(p, es) => string_of_expr_app(string_of_prm(p), es->map(string_of_expr))
  | App(e, es) => string_of_expr_app(string_of_expr(e), es->map(string_of_expr))
  | Let(xes, b) => string_of_expr_let(xes->map(string_of_xe), string_of_block(b))
  | Cnd(ebs, ob) => string_of_expr_cnd(ebs->map(string_of_eb), string_of_ob(ob))
  | If(e_cnd, e_thn, e_els) =>
    string_of_expr_if(string_of_expr(e_cnd), string_of_expr(e_thn), string_of_expr(e_els))
  // | Whl(e, b) => string_of_expr_whl(string_of_expr(e), string_of_block(b))
  | Bgn(es, e) => string_of_expr_bgn(es->map(string_of_expr), string_of_expr(e))
  }
}
and string_of_def = (d: annotated<definition>): string => {
  switch d.it {
  | Var(x, e) => string_of_def_var(x, string_of_expr(e))
  | Fun(f, xs, b) => string_of_def_fun(f->unannotate, xs->map(unannotate), string_of_block(b))
  // | For(x, e_from, e_to, b) =>
  //   string_of_def_for(x, string_of_expr(e_from), string_of_expr(e_to), string_of_block(b))
  }
}
and string_of_xe = xe => {
  let (x, e) = xe
  (x, string_of_expr(e))
}
and string_of_eb = eb => {
  let (e, b) = eb
  (string_of_expr(e), string_of_block(b))
}
and string_of_ob = ob => {
  ob->Option.map(string_of_block)
}
and string_of_block = b => {
  let (ts, e) = b
  String.concat("\n", list{...ts->map(string_of_term), string_of_expr(e)})
}
and string_of_term = t => {
  switch t {
  | Exp(e) => string_of_expr(e)
  | Def(d) => string_of_def(d)
  }
}
