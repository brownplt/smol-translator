open Belt
open SExpression

type primitive =
  | Add
  | Sub
  | Mul
  | Div
  | Lt
  | Eq
  | Gt
  | Le
  | Ge
  | Ne
  | PairNew
  | PairRefRight
  | PairRefLeft
  | PairSetRight
  | PairSetLeft
  | VecNew
  | VecRef
  | VecSet
  | VecLen
  | Eqv
  | Err
type constant =
  | Uni
  | Num(float)
  | Lgc(bool)
  | Str(string)

type symbol = string
type rec expression =
  | Con(constant)
  | Ref(annotated<symbol>)
  | Set(annotated<symbol>, annotated<expression>)
  | Lam(list<annotated<symbol>>, block)
  | Let(list<(annotated<symbol>, annotated<expression>)>, block)
  | AppPrm(primitive, list<annotated<expression>>)
  | App(annotated<expression>, list<annotated<expression>>)
  | Bgn(list<annotated<expression>>, annotated<expression>)
  | If(annotated<expression>, annotated<expression>, annotated<expression>)
  | Cnd(list<(annotated<expression>, block)>, option<block>)
and block = (list<term>, annotated<expression>)
and definition =
  | Var(annotated<symbol>, annotated<expression>)
  | Fun(annotated<symbol>, list<annotated<symbol>>, block)
and term =
  | Def(annotated<definition>)
  | Exp(annotated<expression>)
and program = list<term>

let string_of_primitive = (o: primitive) => {
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
  | PairNew => "pair"
  | PairRefLeft => "left"
  | PairRefRight => "right"
  | PairSetLeft => "set-left!"
  | PairSetRight => "set-right!"
  | VecNew => "vec"
  | VecRef => "vec-ref"
  | VecSet => "vec-set!"
  | VecLen => "vec-len"
  | Eqv => "eq?"
  | Err => "error"
  }
}

let unannotate = x => x.it

let indent = (s, i) => {
  let pad = Js.String.repeat(i, " ")
  Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s)
}

type result =
  | Con(constant)
  | Vec(int) // the int is the address
  | Fun(int) // the int is the address
  | PrmFun(primitive)

type stringifier = {
  string_of_result: result => string,
  string_of_expr: annotated<expression> => string,
  string_of_def: annotated<definition> => string,
  string_of_term: term => string,
  string_of_block: block => string,
  string_of_program: program => string,
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

let string_of_result = r => {
  switch r {
  | Con(c) => string_of_constant(c)
  | Vec(i) => `@${i |> Int.toString}`
  | Fun(i) => `@${i |> Int.toString}`
  | PrmFun(p) => string_of_primitive(p)
  }
}

let string_of_list = ss => {
  "(" ++ String.concat(" ", ss) ++ ")"
}

let string_of_def_var = (x, e) => {
  string_of_list(list{"defvar", x.it, e})
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

let string_of_expr_cnd = (ebs: list<(string, string)>, ob) => {
  let ebs = {
    switch ob {
    | None => ebs
    | Some(b) => list{...ebs, ("else", b)}
    }
  }
  let ebs = ebs->List.map(((e, b)) => `[${e}\n ${indent(b, 1)}]`)
  let ebs = String.concat("\n", ebs)
  "(" ++ "cond\n  " ++ indent(ebs, 2) ++ ")"
}

let string_of_expr_if = (e_cnd: string, e_thn: string, e_els: string) => {
  `(if ${indent(e_cnd, 4)}\n    ${indent(e_thn, 4)}\n    ${indent(e_els, 4)})`
}

let string_of_expr_let = (xes, b) => {
  let xes = xes->List.map(((x, e)) => {
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
  | Lam(xs, b) => string_of_expr_lam(xs->List.map(unannotate), string_of_block(b))
  | AppPrm(p, es) => string_of_expr_app(string_of_primitive(p), es->List.map(string_of_expr))
  | App(e, es) => string_of_expr_app(string_of_expr(e), es->List.map(string_of_expr))
  | Let(xes, b) => string_of_expr_let(xes->List.map(string_of_xe), string_of_block(b))
  | Cnd(ebs, ob) => string_of_expr_cnd(ebs->List.map(string_of_eb), string_of_ob(ob))
  | If(e_cnd, e_thn, e_els) =>
    string_of_expr_if(string_of_expr(e_cnd), string_of_expr(e_thn), string_of_expr(e_els))
  // | Whl(e, b) => string_of_expr_whl(string_of_expr(e), string_of_block(b))
  | Bgn(es, e) => string_of_expr_bgn(es->List.map(string_of_expr), string_of_expr(e))
  }
}
and string_of_def = (d: annotated<definition>): string => {
  switch d.it {
  | Var(x, e) => string_of_def_var(x, string_of_expr(e))
  | Fun(f, xs, b) => string_of_def_fun(f->unannotate, xs->List.map(unannotate), string_of_block(b))
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
  String.concat("\n", list{...ts->List.map(string_of_term), string_of_expr(e)})
}
and string_of_term = t => {
  switch t {
  | Exp(e) => string_of_expr(e)
  | Def(d) => string_of_def(d)
  }
}

let string_of_program = ts => {
  String.concat("\n", ts->List.map(string_of_term))
}

let toString = string_of_term

type s_expr = SExpression.t

type kind_expectation =
  | Atom
  | List

type arity_expectation =
  | ExactlyOne
  | ExactlyTwo
  | ExactlyThree
  | OneThenMany
  | ManyThenOne
  | OneThenManyThenOne

type term_kind =
  | Definition
  | Expression

type parse_error =
  | SExprKindError(kind_expectation, string, annotated<s_expr>)
  | SExprArityError(arity_expectation, string, list<annotated<s_expr>>)
  | LiteralSymbolError(string)
  | LiteralListError(annotated<s_expr>)
  | TermKindError(term_kind, string, term)
exception ParseError(parse_error)

let stringOfExprs = es => {
  switch es {
  | list{} => "no term"
  | list{e} => `one term: ${SExpression.toString(e)}`
  | es =>
    `${List.length(es)->Int.toString} terms: ${String.concat(
        ", ",
        es->List.map(SExpression.toString),
      )}`
  }
}

let stringOfParseError: parse_error => string = err => {
  switch err {
  | SExprKindError(_kind, context, sexpr) =>
    `expecting a ${context}, given ${SExpression.toString(sexpr)}`
  | LiteralSymbolError(x) => `expecting a literal value, given a symbol ${x}`
  | LiteralListError(sexpr) =>
    `expecting a constant or a vector, given ${SExpression.toString(sexpr)}`
  | SExprArityError(_arity_expectation, context, es) =>
    `expecting ${context}, given ${stringOfExprs(es)}`
  | TermKindError(_term_kind, context, term) => `expecting ${context}, given ${toString(term)}`
  }
}

let as_id = (context, e: annotated<s_expr>) => {
  switch e.it {
  | Atom(Sym(x)) => {it: x, ann: e.ann}
  | _ => raise(ParseError(SExprKindError(Atom, context, e)))
  }
}

let as_list = (context, e: annotated<s_expr>) => {
  switch e.it {
  | Sequence(List, _b, ls) => ls
  | _ => raise(ParseError(SExprKindError(List, context, e)))
  }
}

let as_one_then_many = (context, es) => {
  switch es {
  | list{e1, ...es} => (e1, es)
  | _ => raise(ParseError(SExprArityError(OneThenMany, context, es)))
  }
}

let as_many_then_one = (context, es) => {
  switch es {
  | list{e1, ...rest} =>
    switch List.reverse(rest) {
    | list{} => (list{}, e1)
    | list{x, ...xs} => (list{e1, ...List.reverse(xs)}, x)
    }
  | _ => raise(ParseError(SExprArityError(ManyThenOne, context, es)))
  }
}

let as_one = (context, es) => {
  switch es {
  | list{e} => e
  | _ => raise(ParseError(SExprArityError(ExactlyOne, context, es)))
  }
}

let as_two = (context, es) => {
  switch es {
  | list{e1, e2} => (e1, e2)
  | _ => raise(ParseError(SExprArityError(ExactlyTwo, context, es)))
  }
}
let as_three = (context, es) => {
  switch es {
  | list{e1, e2, e3} => (e1, e2, e3)
  | _ => raise(ParseError(SExprArityError(ExactlyThree, context, es)))
  }
}
let as_one_then_many_then_one = (context, es) => {
  switch es {
  | list{e1, e2, ...rest} =>
    switch List.reverse(rest) {
    | list{} => (e1, list{}, e2)
    | list{x, ...xs} => (e1, list{e2, ...List.reverse(xs)}, x)
    }
  | _ => raise(ParseError(SExprArityError(OneThenManyThenOne, context, es)))
  }
}

exception ExpectingExpression
let as_expr = (context, e) => {
  switch e {
  | Exp(e) => e
  | _ => raise(ParseError(TermKindError(Expression, context, e)))
  }
}

let constant_of_atom = (_ann, atom) => {
  switch atom {
  | SExpression.Atom.Str(s) => (Con(Str(s)): expression)
  | Sym("#t") => Con(Lgc(true))
  | Sym("#f") => Con(Lgc(false))
  | Sym(x) => {
      let tryNum = x->Float.fromString->Option.map((n): expression => Con(Num(n)))
      switch tryNum {
      | None => raise(ParseError(LiteralSymbolError(x)))
      | Some(n) => n
      }
    }
  }
}
let rec value_of_sexpr = (e: annotated<s_expr>) => {
  let ann = e.ann
  switch e.it {
  | Atom(atom) => {ann, it: constant_of_atom(ann, atom)}
  | Sequence(Vector, _b, es) => {
      let es = es->List.map(value_of_sexpr)
      {ann, it: AppPrm(VecNew, es)}
    }
  | Sequence(List, _, _) => raise(ParseError(LiteralListError(e)))
  }
}

let expr_of_atom = (ann, atom) => {
  switch atom {
  | SExpression.Atom.Str(s) => (Con(Str(s)): expression)
  | Sym("#t") => Con(Lgc(true))
  | Sym("#f") => Con(Lgc(false))
  | Sym(x) =>
    let e: expression = {
      let tryNum = x->Float.fromString->Option.map((n): expression => Con(Num(n)))
      tryNum->Option.getWithDefault(Ref({ann, it: x}))
    }
    e
  }
}

let rec term_of_sexpr = (e: annotated<s_expr>) => {
  let ann = e.ann
  switch e.it {
  | Sequence(Vector, _b, es) => {
      let es = es->List.map(value_of_sexpr)
      Exp({ann, it: AppPrm(VecNew, es)})
    }
  | Sequence(List, _b, list{{it: Atom(Sym("quote")), ann: _}, ...rest}) => {
      let e = as_one("a quoted value", rest)
      Exp(value_of_sexpr(e))
    }
  | Sequence(List, _b, list{{it: Atom(Sym("defvar")), ann: _}, ...rest}) => {
      let (x, e) = as_two("a variable and an expression", rest)
      let x = as_id("a variable name", x)
      let e = as_expr("an expression", term_of_sexpr(e))
      Def({ann, it: Var(x, e)})
    }

  | Sequence(List, _b, list{{it: Atom(Sym("deffun")), ann: _}, ...rest}) => {
      let (head, terms, result) = as_one_then_many_then_one("", rest)
      let (fun, args) = as_one_then_many(
        "function name followed by parameters",
        as_list("function name and parameters", head),
      )
      let fun = as_id("a function name", fun)
      let args = List.map(args, as_id("a parameter"))
      let terms = Belt.List.map(terms, term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr("an expression to be returned")
      Def({ann, it: Fun(fun, args, (terms, result))})
    }

  | Sequence(List, _b, list{{it: Atom(Sym("lambda")), ann: _}, ...rest}) => {
      let (args, terms, result) = as_one_then_many_then_one(
        "the function signature followed by the function body",
        rest,
      )
      let args = as_list("function parameters", args)->List.map(as_id("a parameter"))
      let terms = terms->List.map(term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr("an expression to be returned")
      Exp({ann, it: Lam(args, (terms, result))})
    }

  | Sequence(List, _b, list{{it: Atom(Sym("begin")), ann: _}, ...rest}) => {
      let (terms, result) = as_many_then_one("one or more expressions", rest)
      let terms = terms->List.map(term_of_sexpr)->List.map(as_expr("an expression"))
      let result = result->term_of_sexpr |> as_expr("an expression")
      Exp({ann, it: Bgn(terms, result)})
    }

  | Sequence(List, _b, list{{it: Atom(Sym("set!")), ann: _}, ...rest}) => {
      let (x, e) = as_two("a variable and an expression", rest)
      let x = as_id("a variable to be set", x)
      let e = as_expr("an expression", term_of_sexpr(e))
      Exp({ann, it: Set(x, e)})
    }

  | Sequence(List, _b, list{{it: Atom(Sym("if")), ann: _}, ...rest}) => {
      let (e_cnd, e_thn, e_els) = as_three(
        "three expressions (i.e., a condition, the \"then\" branch, and the \"else\" branch)",
        rest,
      )
      let e_cnd = as_expr("a (conditional) expression", term_of_sexpr(e_cnd))
      let e_thn = as_expr("an expression", term_of_sexpr(e_thn))
      let e_els = as_expr("an expression", term_of_sexpr(e_els))
      Exp({
        ann,
        it: If(e_cnd, e_thn, e_els),
      })
    }

  | Sequence(List, _b, list{{it: Atom(Sym("cond")), ann: _}, ...branches}) => {
      let branches =
        branches
        ->List.map(as_list("a `cond` branch"))
        ->List.map(as_one_then_many_then_one("the condition followed by the branch"))
      let rec loop = (parsed, branches) => {
        switch branches {
        | list{} => Exp({ann, it: Cnd(List.reverse(parsed), None)})
        | list{({it: Atom(Sym("else")), ann: _}: annotated<s_expr>, terms, result)} => {
            let terms = terms->List.map(term_of_sexpr)
            let result = result |> term_of_sexpr |> as_expr("an expression")
            Exp({ann, it: Cnd(List.reverse(parsed), Some((terms, result)))})
          }

        | list{(case, terms, result), ...branches} => {
            let case = case->term_of_sexpr |> as_expr("a (conditional) expression")
            let terms = terms->List.map(term_of_sexpr)
            let result = result |> term_of_sexpr |> as_expr("an expression")
            loop(list{(case, (terms, result)), ...parsed}, branches)
          }
        }
      }
      loop(list{}, branches)
    }

  | Sequence(List, _b, list{{it: Atom(Sym("let")), ann: _}, ...rest}) => {
      let (xes, ts, result) = as_one_then_many_then_one("the bindings followed by the body", rest)
      let xes =
        as_list("variable-expression pairs", xes)
        ->List.map(as_list("a variable and an expression"))
        ->List.map(as_two("a variable and an expression"))
      let xes = xes->List.map(((x, e)) => {
        let x = as_id("a variable to be bound", x)
        let e = term_of_sexpr(e) |> as_expr("an expression")
        (x, e)
      })
      let ts = ts->List.map(term_of_sexpr)
      let result = term_of_sexpr(result) |> as_expr("an expression to be return")
      Exp({ann, it: Let(xes, (ts, result))})
    }

  | Atom(atom) => Exp({ann, it: expr_of_atom(ann, atom)})
  | Sequence(List, _b, list{{it: Atom(Sym("+")), ann: _}, ...es}) => app_prm(ann, Add, es)
  | Sequence(List, _b, list{{it: Atom(Sym("-")), ann: _}, ...es}) => app_prm(ann, Sub, es)
  | Sequence(List, _b, list{{it: Atom(Sym("*")), ann: _}, ...es}) => app_prm(ann, Mul, es)
  | Sequence(List, _b, list{{it: Atom(Sym("/")), ann: _}, ...es}) => app_prm(ann, Div, es)
  | Sequence(List, _b, list{{it: Atom(Sym("<")), ann: _}, ...es}) => app_prm(ann, Lt, es)
  | Sequence(List, _b, list{{it: Atom(Sym("=")), ann: _}, ...es}) => app_prm(ann, Eq, es)
  | Sequence(List, _b, list{{it: Atom(Sym(">")), ann: _}, ...es}) => app_prm(ann, Gt, es)
  | Sequence(List, _b, list{{it: Atom(Sym("<=")), ann: _}, ...es}) => app_prm(ann, Le, es)
  | Sequence(List, _b, list{{it: Atom(Sym(">=")), ann: _}, ...es}) => app_prm(ann, Ge, es)
  | Sequence(List, _b, list{{it: Atom(Sym("!=")), ann: _}, ...es}) => app_prm(ann, Ne, es)
  | Sequence(List, _b, list{{it: Atom(Sym("pair")), ann: _}, ...es}) => app_prm(ann, PairNew, es)
  | Sequence(List, _b, list{{it: Atom(Sym("mpair")), ann: _}, ...es}) => app_prm(ann, PairNew, es)
  | Sequence(List, _b, list{{it: Atom(Sym("left")), ann: _}, ...es}) =>
    app_prm(ann, PairRefLeft, es)
  | Sequence(List, _b, list{{it: Atom(Sym("right")), ann: _}, ...es}) =>
    app_prm(ann, PairRefRight, es)
  | Sequence(List, _b, list{{it: Atom(Sym("set-left!")), ann: _}, ...es}) =>
    app_prm(ann, PairSetLeft, es)
  | Sequence(List, _b, list{{it: Atom(Sym("set-right!")), ann: _}, ...es}) =>
    app_prm(ann, PairSetRight, es)
  | Sequence(List, _b, list{{it: Atom(Sym("vec")), ann: _}, ...es}) => app_prm(ann, VecNew, es)
  | Sequence(List, _b, list{{it: Atom(Sym("mvec")), ann: _}, ...es}) => app_prm(ann, VecNew, es)
  | Sequence(List, _b, list{{it: Atom(Sym("vec-ref")), ann: _}, ...es}) => app_prm(ann, VecRef, es)
  | Sequence(List, _b, list{{it: Atom(Sym("vref")), ann: _}, ...es}) => app_prm(ann, VecRef, es)
  | Sequence(List, _b, list{{it: Atom(Sym("vec-set!")), ann: _}, ...es}) => app_prm(ann, VecSet, es)
  | Sequence(List, _b, list{{it: Atom(Sym("vset!")), ann: _}, ...es}) => app_prm(ann, VecSet, es)
  | Sequence(List, _b, list{{it: Atom(Sym("vec-len")), ann: _}, ...es}) => app_prm(ann, VecLen, es)
  | Sequence(List, _b, list{{it: Atom(Sym("vlen")), ann: _}, ...es}) => app_prm(ann, VecLen, es)
  | Sequence(List, _b, list{{it: Atom(Sym("eq?")), ann: _}, ...es}) => app_prm(ann, Eqv, es)
  | Sequence(List, _b, list{{it: Atom(Sym("eqv?")), ann: _}, ...es}) => app_prm(ann, Eqv, es)
  | Sequence(List, _b, list{{it: Atom(Sym("equal?")), ann: _}, ...es}) => app_prm(ann, Eqv, es)
  | Sequence(List, _b, list{{it: Atom(Sym("error")), ann: _}, ...es}) => app_prm(ann, Err, es)
  | Sequence(List, _b, es) => {
      let (e, es) = as_one_then_many(
        "a function call/application, which includes a function and then one ore more arguments",
        es,
      )
      let e = e->term_of_sexpr |> as_expr("a function")
      let es = es->List.map(term_of_sexpr)->List.map(as_expr("an argument"))
      Exp({ann, it: App(e, es)})
    }
  }
}
and app_prm = (ann, p, es) => {
  let es = es->List.map(term_of_sexpr)->List.map(as_expr("an argument"))
  Exp({ann, it: AppPrm(p, es)})
}
and terms_of_sexprs = es => {
  es->List.map(term_of_sexpr)
}

let terms_of_string = src => {
  src->SExpression.fromString->terms_of_sexprs
}

exception Impossible(string)

let as_many_then_one = es => {
  switch es {
  | list{e1, ...rest} =>
    switch List.reverse(rest) {
    | list{} => (list{}, e1)
    | list{x, ...xs} => (list{e1, ...List.reverse(xs)}, x)
    }
  | _ => raise(Impossible("unsafe"))
  }
}

type js_ctx =
  | Expr(bool) // the bool indicates whether we are in an infix operation
  | Stat
  | Return

let maybe_wrap = (ctx, p, code) => {
  switch (ctx, p) {
  | (Expr(true), Add) => `(${code})`
  | (Expr(true), Sub) => `(${code})`
  | (Expr(true), Mul) => `(${code})`
  | (Expr(true), Div) => `(${code})`
  | _ => code
  }
}

module type Translator = {
  let translate_program: string => string
  let translate_block: string => string
  let translate_expressions: string => string
}

module SMoLToJS = {
  open List

  let string_of_constant = c => {
    switch c {
    | Uni => "null"
    | Num(n) => Float.toString(n)
    | Lgc(l) =>
      if l {
        "true"
      } else {
        "false"
      }
    | Str(s) => "\"" ++ String.escaped(s) ++ "\""
    }
  }

  let string_of_list = ss => {
    "(" ++ String.concat(", ", ss) ++ ")"
  }

  let string_of_identifier = x => {
    let re = %re("/-./g")
    let matchFn = (matchPart, _offset, _wholeString) => {
      Js.String2.toUpperCase(Js.String2.substringToEnd(matchPart, ~from=1))
    }
    let x = Js.String2.unsafeReplaceBy0(x, re, matchFn)

    // add `$` to the beginning of reserved words
    if x == "var" {
      "$var"
    } else if x == "+" {
      "(function(x, y) { return x + y; })"
    } else if x == "-" {
      "(function(x, y) { return x - y; })"
    } else if x == "*" {
      "(function(x, y) { return x * y; })"
    } else if x == "/" {
      "(function(x, y) { return x / y; })"
    } else {
      x
    }
  }

  let string_of_def_var = (x, e) => {
    `let ${string_of_identifier(x.it)} = ${e};`
  }

  // let string_of_def_for = (_x, _e_from, _e_to, _body) => {
  //   raise(TranslationNotSupportedYet("`for`-loop"))
  // }

  let string_of_def_fun = (f, xs, b) => {
    `function ${f}${string_of_list(xs)} {\n  ${indent(b, 2)}\n}`
  }

  let string_of_expr_set = (ctx, x, e) => {
    let itself = `${x} = ${e}`;
    switch ctx {
    | Expr(false) => itself
    | Expr(true) => `(${itself})`
    | Stat => `${itself};`
    | Return => `return ${itself};`
    }
  }

  let string_of_expr_lam = (xs, b) => {
    `function ${string_of_list(xs)} {\n  ${indent(b, 2)}\n}`
  }

  let string_of_expr_app_prm = (p, es) => {
    switch (p, es) {
    | (Add, es) => `${String.concat(" + ", es)}`
    | (Sub, es) => `${String.concat(" - ", es)}`
    | (Mul, es) => `${String.concat(" * ", es)}`
    | (Div, es) => `${String.concat(" / ", es)}`
    | (Lt, list{e1, e2}) => `${e1} < ${e2}`
    | (Eq, list{e1, e2}) => `${e1} === ${e2}`
    | (Gt, list{e1, e2}) => `${e1} > ${e2}`
    | (Le, list{e1, e2}) => `${e1} <= ${e2}`
    | (Ge, list{e1, e2}) => `${e1} >= ${e2}`
    | (Ne, list{e1, e2}) => `${e1} != ${e2}`
    | (PairRefLeft, list{e1}) => `${e1}[0]`
    | (PairRefRight, list{e1}) => `${e1}[1]`
    | (PairSetLeft, list{e1, e2}) => `${e1}[0]=${e2}`
    | (PairSetRight, list{e1, e2}) => `${e1}[1]=${e2}`
    | (PairNew, list{e1, e2}) => `[ ${e1}, ${e2} ]`
    | (VecNew, es) => `[ ${String.concat(", ", es)} ]`
    | (VecSet, list{e1, e2, e3}) => `${e1}[${e2}] = ${e3}`
    | (VecRef, list{e1, e2}) => `${e1}[${e2}]`
    | (VecLen, list{e}) => `${e}.length`
    | (Eqv, list{e1, e2}) => `${e1} === ${e2}`
    | (Err, list{e}) => `throw ${e}`
    | _ => "/* a primitive operation not supported yet */"
    }
  }

  let string_of_expr_app = (e, es) => {
    `${e}${string_of_list(es)}`
  }

  let string_of_expr_bgn = (es, e) => {
    `(${String.concat(", ", list{...es, e})})`
  }

  // let string_of_expr_whl = (_e, _b) => {
  //   raise(TranslationNotSupportedYet("`while`-loop"))
  // }

  let string_of_expr_cnd = (ebs: list<(string, string)>, ob) => {
    let ob = {
      switch ob {
      | None => ""
      | Some(b) => ` else {\n  ${indent(b, 2)}\n}`
      }
    }
    let ebs = ebs->List.map(((e, b)) => `if (${e}) {\n  ${indent(b, 2)}\n}`)
    let ebs = String.concat(" else ", ebs)
    ebs ++ ob
  }

  let string_of_expr_if = (e_cnd: string, e_thn: string, e_els: string) => {
    `(${e_cnd} ? ${e_thn} : ${e_els})`
  }

  let string_of_expr_let = (xes, b) => {
    `((${xes->List.map(((x, _e)) => x) |> String.concat(", ")})=>{${b}})(${xes->List.map(((_x, e)) => e)
        |> String.concat(", ")})`
  }

  let consider_context = (ctx: js_ctx, code: string) => {
    switch ctx {
    | Expr(_) => code
    | Stat => `${code};`
    | Return => `return ${code};`
    }
  }

  let rec string_of_expr = (ctx: js_ctx, e: annotated<expression>): string => {
    switch e.it {
    | Con(c) => string_of_constant(c) |> consider_context(ctx)
    | Ref(x) => string_of_identifier(x.it) |> consider_context(ctx)
    | Set(x, e) =>
      string_of_expr_set(
        ctx,
        x->unannotate->string_of_identifier,
        string_of_expr(Expr(false), e),
      )
    | Lam(xs, b) =>
      string_of_expr_lam(
        xs->List.map(unannotate)->List.map(string_of_identifier),
        string_of_block(Return, b),
      ) |> consider_context(ctx)
    | AppPrm(p, es) =>
      let o = string_of_expr_app_prm(p, es->List.map(string_of_expr(Expr(true)))) |> maybe_wrap(ctx, p)
      if p != Err {
        o |> consider_context(ctx)
      } else {
        o
      }
    | App(e, es) =>
      string_of_expr_app(
        string_of_expr(Expr(false), e),
        es->List.map(string_of_expr(Expr(false))),
      ) |> consider_context(ctx)
    | Let(xes, b) =>
      string_of_expr_let(xes->List.map(string_of_xe), string_of_block(Return, b)) |> consider_context(
        ctx,
      )
    | Cnd(ebs, ob) => string_of_expr_cnd(ebs->List.map(string_of_eb(ctx)), string_of_ob(ctx, ob))
    | If(e_cnd, e_thn, e_els) =>
      string_of_expr_if(
        string_of_expr(Expr(false), e_cnd),
        string_of_expr(Expr(false), e_thn),
        string_of_expr(Expr(false), e_els),
      ) |> consider_context(ctx)
    | Bgn(es, e) =>
      string_of_expr_bgn(
        es->List.map(string_of_expr(Expr(false))),
        string_of_expr(Expr(false), e),
      ) |> consider_context(ctx)
    }
  }
  and string_of_def = (d: annotated<definition>): string => {
    switch d.it {
    | Var(x, e) => string_of_def_var(x, string_of_expr(Expr(false), e))
    | Fun(f, xs, b) =>
      string_of_def_fun(
        f->unannotate->string_of_identifier,
        xs->List.map(unannotate)->List.map(string_of_identifier),
        string_of_block(Return, b),
      )
    }
  }
  and string_of_xe = xe => {
    let (x, e) = xe
    (string_of_identifier(x.it), string_of_expr(Expr(false), e))
  }
  and string_of_eb = (ctx, eb) => {
    let (e, b) = eb
    (string_of_expr(Expr(false), e), string_of_block(ctx, b))
  }
  and string_of_ob = (ctx, ob) => {
    ob->Option.map(string_of_block(ctx))
  }
  and string_of_block = (ctx, b) => {
    let (ts, e) = b
    String.concat("\n", list{...ts->List.map(string_of_term), string_of_expr(ctx, e)})
  }
  and string_of_term = t => {
    switch t {
    | Exp(e) => string_of_expr(Stat, e)
    | Def(d) => string_of_def(d)
    }
  }

  let as_many_then_one = es => {
    switch es {
    | list{e1, ...rest} =>
      switch reverse(rest) {
      | list{} => (list{}, e1)
      | list{x, ...xs} => (list{e1, ...reverse(xs)}, x)
      }
    | _ => raise(Impossible("unsafe"))
    }
  }

  let translate_expressions: string => string = results => {
    let ts = results->terms_of_string
    String.concat(" ", ts->List.map(as_expr("expr"))->List.map(string_of_expr(Expr(true))))
  }

  let translate_program: string => string = program => {
    let ts = program->terms_of_string
    String.concat(
      "\n",
      ts->List.map(t => {
        switch t {
        | Def(_) => string_of_term(t)
        | Exp(e) =>
          switch e.it {
          | Set(_x, _e) => `${string_of_expr(Expr(false), e)};`
          | AppPrm(VecSet, _args) => `${string_of_expr(Expr(false), e)};`
          | _ => `console.log(${string_of_expr(Expr(false), e)});`
          }
        }
      }),
    )
  }

  let translate_block: string => string = program => {
    let ts = program->terms_of_string
    let (ts, t) = as_many_then_one(ts)
    let e = as_expr("result", t)
    string_of_block(Return, (ts, e))
  }
}

module SMoLToPY = {
  let string_of_constant = c => {
    switch c {
    | Uni => "None"
    | Num(n) => Float.toString(n)
    | Lgc(l) =>
      if l {
        "True"
      } else {
        "False"
      }
    | Str(s) => "\"" ++ String.escaped(s) ++ "\""
    }
  }

  let string_of_list = ss => {
    "(" ++ String.concat(", ", ss) ++ ")"
  }

  let string_of_identifier = x => {
    if x != "-" {
      let re = %re("/-/g")
      let matchFn = (_matchPart, _offset, _wholeString) => {
        "_"
      }
      Js.String2.unsafeReplaceBy0(x, re, matchFn)
    } else {
      x
    }
  }

  let string_of_def_var = (x, e) => {
    `${string_of_identifier(x.it)} = ${e}`
  }

  // let string_of_def_for = (_x, _e_from, _e_to, _body) => {
  //   raise(TranslationNotSupportedYet("`for`-loop"))
  // }

  let string_of_def_fun = (f, xs, b) => {
    `def ${f}${string_of_list(xs)}:\n    ${indent(b, 4)}`
  }

  let string_of_expr_set = (ctx, x, e) => {
    switch ctx {
    | Expr(true) => `(${x} := ${e})`
    | Expr(false) => `${x} := ${e}`
    | Stat => `${x} = ${e}`
    | Return => `return (${x} := ${e})`
    }
  }

  let string_of_expr_lam = (xs, b) => {
    if xs == list{} {
      `lambda: ${b}`
    } else {
      `lambda ${String.concat(",", xs)}: ${b}`
    }
  }

  let wrap = (ctx, code) => {
    switch ctx {
    | Expr(true) => `(${code})`
    | Return => `return (${code})`
    | _ => code
    }
  }

  let ret = (ctx, code) => {
    switch ctx {
    | Return => `return ${code}`
    | _ => code
    }
  }

  let string_of_expr_app_prm = (ctx, p, es) => {
    switch (p, es) {
    | (Add, es) => `${String.concat(" + ", es)}` |> wrap(ctx)
    | (Sub, es) => `${String.concat(" - ", es)}` |> wrap(ctx)
    | (Mul, es) => `${String.concat(" * ", es)}` |> wrap(ctx)
    | (Div, es) => `${String.concat(" / ", es)}` |> wrap(ctx)
    | (Lt, list{e1, e2}) => `${e1} < ${e2}` |> wrap(ctx)
    | (Eq, list{e1, e2}) => `${e1} is ${e2}` |> wrap(ctx)
    | (Gt, list{e1, e2}) => `${e1} > ${e2}` |> wrap(ctx)
    | (Le, list{e1, e2}) => `${e1} <= ${e2}` |> wrap(ctx)
    | (Ge, list{e1, e2}) => `${e1} >= ${e2}` |> wrap(ctx)
    | (Ne, list{e1, e2}) => `${e1} != ${e2}` |> wrap(ctx)
    | (PairRefLeft, list{e1}) => `${e1}[0]` |> wrap(ctx)
    | (PairRefRight, list{e1}) => `${e1}[1]` |> wrap(ctx)
    | (PairSetLeft, list{e1, e2}) =>
      switch ctx {
      | Stat => `${e1}[0] = ${e2}`
      | Expr(true) => `${e1}.__setitem__(0, ${e2})`
      | Expr(false) => `${e1}.__setitem__(0, ${e2})`
      | Return => `return ${e1}.__setitem__(0, ${e2})`
      }
    | (PairSetRight, list{e1, e2}) =>
      switch ctx {
      | Stat => `${e1}[1] = ${e2}`
      | Expr(true) => `${e1}.__setitem__(1, ${e2})`
      | Expr(false) => `${e1}.__setitem__(1, ${e2})`
      | Return => `return ${e1}.__setitem__(1, ${e2})`
      }
    | (PairNew, list{e1, e2}) => `[ ${e1}, ${e2} ]` |> ret(ctx)
    | (VecNew, es) => `[${String.concat(", ", es)}]` |> ret(ctx)
    | (VecSet, list{e1, e2, e3}) =>
      switch ctx {
      | Stat => `${e1}[${e2}] = ${e3}`
      | Expr(true) => `${e1}.__setitem__(${e2}, ${e3})`
      | Expr(false) => `${e1}.__setitem__(${e2}, ${e3})`
      | Return => `return ${e1}.__setitem__(${e2}, ${e3})`
      }
    | (VecRef, list{e1, e2}) => `${e1}[${e2}]` |> ret(ctx)
    | (VecLen, list{e}) => `${e}.length` |> ret(ctx)
    | (Eqv, list{e1, e2}) => `${e1} is ${e2}` |> wrap(ctx)
    | (Err, list{e}) => `raise ${e}`
    | _ => "/* a primitive operation not supported yet */"
    }
  }

  let string_of_expr_app = (e, es) => {
    `${e}${string_of_list(es)}`
  }

  let string_of_expr_bgn = (es, e) => {
    `[${String.concat(", ", list{...es, e})}][-1]`
  }

  let string_of_expr_cnd = (ebs: list<(string, string)>, ob) => {
    let ob = {
      switch ob {
      | None => ""
      | Some(b) => `else:\n    ${indent(b, 4)}`
      }
    }
    let ebs = ebs->List.map(((e, b)) => `if ${e}:\n    ${indent(b, 2)}\n`)
    let ebs = String.concat("el", ebs)
    ebs ++ ob
  }

  let string_of_expr_if = (e_cnd: string, e_thn: string, e_els: string) => {
    `${e_thn} if ${e_cnd} else ${e_els}`
  }

  let string_of_expr_let = (_xes, _b) => {
    `"...a let-expression..."`
  }

  let consider_context = (ctx: js_ctx, code: string) => {
    switch ctx {
    | Return => `return ${code}`
    | _ => code
    }
  }

  let rec string_of_expr = (ctx: js_ctx, e: annotated<expression>): string => {
    switch e.it {
    | Con(c) => string_of_constant(c) |> consider_context(ctx)
    | Ref(x) => string_of_identifier(x.it) |> consider_context(ctx)
    | Set(x, e) =>
      string_of_expr_set(ctx, x->unannotate->string_of_identifier, string_of_expr(Expr(false), e))
    | Lam(xs, b) =>
      let b = {
        let (ts, e) = b
        switch ts {
        | list{} => string_of_expr(Expr(false), e)
        | _ => `\n${string_of_block(Return, b)}\nend`
        }
      }
      string_of_expr_lam(xs->List.map(unannotate)->List.map(string_of_identifier), b) |> consider_context(ctx)
    | AppPrm(VecSet, es) =>
      string_of_expr_app_prm(ctx, VecSet, es->List.map(string_of_expr(Expr(false))))
    | AppPrm(p, es) => string_of_expr_app_prm(ctx, p, es->List.map(string_of_expr(Expr(true))))
    | App(e, es) =>
      string_of_expr_app(
        string_of_expr(Expr(false), e),
        es->List.map(string_of_expr(Expr(false))),
      ) |> consider_context(ctx)
    | Let(xes, b) =>
      string_of_expr_let(xes->List.map(string_of_xe), string_of_block(Return, b)) |> consider_context(
        ctx,
      )
    | Cnd(ebs, ob) =>
      switch ctx {
      | Expr(_) => "if..."
      | _ => string_of_expr_cnd(ebs->List.map(string_of_eb(ctx)), string_of_ob(ctx, ob))
      }
    | If(e_cnd, e_thn, e_els) =>
      string_of_expr_if(
        string_of_expr(Expr(false), e_cnd),
        string_of_expr(Expr(false), e_thn),
        string_of_expr(Expr(false), e_els),
      ) |> consider_context(ctx)
    // | Whl(e, b) =>
    //   string_of_expr_whl(string_of_expr(Expr(false), e), string_of_block(Expr(false), b)) |> consider_context(ctx)
    | Bgn(es, e) =>
      string_of_expr_bgn(
        es->List.map(string_of_expr(Expr(false))),
        string_of_expr(Expr(false), e),
      ) |> consider_context(ctx)
    }
  }
  and string_of_def = (d: annotated<definition>): string => {
    switch d.it {
    | Var(x, e) => string_of_def_var(x, string_of_expr(Expr(false), e))
    | Fun(f, xs, b) =>
      string_of_def_fun(
        f->unannotate->string_of_identifier,
        xs->List.map(unannotate)->List.map(string_of_identifier),
        string_of_block(Return, b),
      )
    // | For(x, e_from, e_to, b) =>
    //   string_of_def_for(
    //     x,
    //     string_of_expr(Expr, e_from),
    //     string_of_expr(Expr, e_to),
    //     string_of_block(Expr, b),
    //   )
    }
  }
  and string_of_xe = xe => {
    let (x, e) = xe
    (string_of_identifier(x.it), string_of_expr(Expr(false), e))
  }
  and string_of_eb = (ctx, eb) => {
    let (e, b) = eb
    (string_of_expr(Expr(false), e), string_of_block(ctx, b))
  }
  and string_of_ob = (ctx, ob) => {
    ob->Option.map(string_of_block(ctx))
  }
  and string_of_block = (ctx, b) => {
    let (ts, e) = b
    String.concat("\n", list{...ts->List.map(string_of_term), string_of_expr(ctx, e)})
  }
  and string_of_term = t => {
    switch t {
    | Exp(e) => string_of_expr(Stat, e)
    | Def(d) => string_of_def(d)
    }
  }

  let translate_program: string => string = program => {
    let ts = program->terms_of_string
    String.concat(
      "\n",
      ts->List.map(t => {
        switch t {
        | Def(_) => string_of_term(t)
        | Exp(e) => `print(${string_of_expr(Expr(false), e)})`
        }
      }),
    )
  }

  let translate_block: string => string = program => {
    let ts = program->terms_of_string
    let (ts, t) = as_many_then_one(ts)
    let e = as_expr("result", t)
    string_of_block(Return, (ts, e))
  }

  let translate_expressions: string => string = results => {
    let ts = results->terms_of_string
    String.concat(" ", ts->List.map(as_expr("expr"))->List.map(string_of_expr(Expr(true))))
  }
}

let stringify = {
  string_of_result,
  string_of_expr,
  string_of_def,
  string_of_term,
  string_of_block,
  string_of_program
}

let stringifyAsJS: stringifier = {
  string_of_result: r => {
    switch r {
    | Con(c) => SMoLToJS.string_of_constant(c)
    | PrmFun(_p) => SMoLToJS.string_of_identifier(string_of_result(r))
    | _ => string_of_result(r)
    }
  },
  string_of_def: SMoLToJS.string_of_def,
  string_of_expr: SMoLToJS.string_of_expr(Expr(false)),
  string_of_term: SMoLToJS.string_of_term,
  string_of_block: SMoLToJS.string_of_block(Return),
  string_of_program: ts => String.concat("\n", ts->List.map(SMoLToJS.string_of_term))
}

let stringifyAsPY: stringifier = {
  string_of_result: r => {
    switch r {
    | Con(c) => SMoLToJS.string_of_constant(c)
    | PrmFun(_p) => SMoLToPY.string_of_identifier(string_of_result(r))
    | _ => string_of_result(r)
    }
  },
  string_of_def: SMoLToPY.string_of_def,
  string_of_expr: SMoLToPY.string_of_expr(Expr(false)),
  string_of_term: SMoLToPY.string_of_term,
  string_of_block: SMoLToPY.string_of_block(Return),
  string_of_program: ts => String.concat("\n", ts->List.map(SMoLToPY.string_of_term)),
}
