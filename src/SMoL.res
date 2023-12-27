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
  | Not
let all_primitives = [
  Add,
  Sub,
  Mul,
  Div,
  Lt,
  Eq,
  Gt,
  Le,
  Ge,
  Ne,
  PairNew,
  PairRefRight,
  PairRefLeft,
  PairSetRight,
  PairSetLeft,
  VecNew,
  VecRef,
  VecSet,
  VecLen,
  Eqv,
  Err,
  Not,
]
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
  | Letrec(list<(annotated<symbol>, annotated<expression>)>, block)
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

type exn += SMoLPrintError(string)
module type Printer = {
  let printProgram: program => string
  let printBlock: block => string
  let printTerm: term => string
}

let unannotate = x => x.it

let indent = (s, i) => {
  let pad = Js.String.repeat(i, " ")
  Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s)
}
let indentBlock = (s, i) => indent("\n" ++ s, i)

let hcat = (s1, s2) => {
  `${s1}${indent(s2, String.length(s1))}`
}

module SMoLPrinter = {
  let primitiveToString = (o: primitive) => {
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
    | Not => "not"
    }
  }

  let constantToString = c => {
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

  let listToString = ss => {
    "(" ++ String.concat(" ", ss) ++ ")"
  }

  let defvarLike = (op, x, e) => {
    if String.contains(e, '\n') {
      hcat(`(${op} ${x} `, `${e})`)
    } else {
      listToString(list{op, x, e})
    }
  }

  let defvarToString = (x, e) => {
    defvarLike("defvar", x, e)
  }

  let deffunToString = (f, xs, b) => {
    defvarLike("deffun", listToString(list{f, ...xs}), b)
  }

  let exprSetToString = (x, e) => {
    defvarLike("set!", x, e)
  }

  let exprLamToString = (xs, b) => {
    defvarLike("lambda", listToString(xs), b)
  }

  let exprAppToString = (e, es) => {
    listToString(list{e, ...es})
  }

  let beginLike = (op, ts) => {
    `(${op}${indentBlock(String.concat("\n", ts), 2)})`
  }
  let exprBgnToString = (es, e) => {
    beginLike("begin", list{...es, e})
  }

  let exprCndToString = (ebs: list<(string, string)>, ob) => {
    let ebs = {
      switch ob {
      | None => ebs
      | Some(b) => list{...ebs, ("else", b)}
      }
    }
    let ebs = ebs->List.map(((e, b)) => `[${e}${indentBlock(b, 1)}]`)
    beginLike("cond", ebs)
  }

  let exprIfToString = (e_cnd: string, e_thn: string, e_els: string) => {
    `(if ${indent(e_cnd, 4)}${indentBlock(e_thn, 4)}${indentBlock(e_els, 4)})`
  }

  let letLike = (op, xes, b) => {
    let xes = xes->List.map(((x, e)) => {
      let x = unannotate(x)
      hcat(`[${x} `, `${e}]`)
    })
    let xes = String.concat("\n", xes)
    let xes = `(${indent(xes, 1)})`
    hcat(`(${op} `, `${xes}`) ++ `${indentBlock(b, 2)})`
  }
  let exprLetToString = (xes, b) => {
    letLike("let", xes, b)
  }
  let exprLetrecToString = (xes, b) => {
    letLike("letrec", xes, b)
  }

  let rec expToString = (e: expression): string => {
    switch e {
    | Con(c) => constantToString(c)
    | Ref(x) => x.it
    | Set(x, e) => exprSetToString(x->unannotate, expToString(e.it))
    | Lam(xs, b) => exprLamToString(xs->List.map(unannotate), printBlock(b))
    | AppPrm(p, es) => exprAppToString(primitiveToString(p), expsToString(es))
    | App(e, es) => exprAppToString(expToString(e.it), expsToString(es))
    | Let(xes, b) => exprLetToString(xes->List.map(xeToString), printBlock(b))
    | Letrec(xes, b) => exprLetrecToString(xes->List.map(xeToString), printBlock(b))
    | Cnd(ebs, ob) => exprCndToString(ebs->List.map(ebToString), obToString(ob))
    | If(e_cnd, e_thn, e_els) =>
      exprIfToString(expToString(e_cnd.it), expToString(e_thn.it), expToString(e_els.it))
    | Bgn(es, e) => exprBgnToString(expsToString(es), expToString(e.it))
    }
  }
  and defToString = (d: definition): string => {
    switch d {
    | Var(x, e) => defvarToString(x.it, expToString(e.it))
    | Fun(f, xs, b) => deffunToString(f->unannotate, xs->List.map(unannotate), printBlock(b))
    }
  }
  and expsToString = es => {
    es->List.map(unannotate)->List.map(expToString)
  }
  and xeToString = xe => {
    let (x, e) = xe
    (x, expToString(e.it))
  }
  and ebToString = eb => {
    let (e, b) = eb
    (expToString(e.it), printBlock(b))
  }
  and obToString = ob => {
    ob->Option.map(printBlock)
  }
  and printBlock = b => {
    let (ts, e) = b
    String.concat("\n", list{...ts->List.map(printTerm), expToString(e.it)})
  }
  and printTerm = t => {
    switch t {
    | Exp(e) => expToString(e.it)
    | Def(d) => defToString(d.it)
    }
  }

  let termsToString = ts => {
    String.concat("\n", ts->List.map(printTerm))
  }

  let printProgram = termsToString
}

module SExprKind = {
  type t = Atom | List
  let toString = t => {
    switch t {
    | Atom => "atom"
    | List => "list"
    }
  }
}
module Arity = {
  type t =
    | ExactlyOne
    | ExactlyTwo
    | ExactlyThree
    | OneThenMany
    | ManyThenOne
    | OneThenManyThenOne
  let toString = t => {
    switch t {
    | ExactlyOne => "exactly one"
    | ExactlyTwo => "exactly two"
    | ExactlyThree => "exactly three"
    | OneThenMany => "one followed by many"
    | ManyThenOne => "many followed by one"
    | OneThenManyThenOne => "one followed by several followed by one"
    }
  }
}
module TermKind = {
  type t = Definition | Expression
  let toString = t => {
    switch t {
    | Definition => "definition"
    | Expression => "expression"
    }
  }
}
module ParseError = {
  type t =
    | SExprParseError(string)
    | SExprKindError(SExprKind.t, string, annotated<SExpression.t>)
    | SExprArityError(Arity.t, string, list<annotated<SExpression.t>>)
    | LiteralSymbolError(string)
    | LiteralListError(annotated<SExpression.t>)
    | TermKindError(TermKind.t, string, term)
  let toString = t => {
    switch t {
    | SExprParseError(msg) => `expecting a (valid) s-expression, but the input is not: ${msg}`
    | SExprKindError(_kind, context, sexpr) =>
      `expecting a ${context}, given ${SExpression.toString(sexpr)}`
    | SExprArityError(_arity_expectation, context, es) =>
      `expecting ${context}, given ${String.concat(" ", es->List.map(SExpression.toString))}`
    | LiteralSymbolError(x) => `expecting a literal value, given a symbol ${x}`
    | LiteralListError(sexpr) =>
      `expecting a constant or a vector, given ${SExpression.toString(sexpr)}`
    | TermKindError(_term_kind, context, term) =>
      `expecting ${context}, given ${SMoLPrinter.printTerm(term)}`
    }
  }
}
exception SMoLParseError(ParseError.t)
let raiseParseError = err => raise(SMoLParseError(err))

module Parser = {
  let constant_of_atom = (_ann, atom) => {
    switch atom {
    | SExpression.Atom.Str(s) => (Con(Str(s)): expression)
    | Sym("#t") => Con(Lgc(true))
    | Sym("#f") => Con(Lgc(false))
    | Sym(x) => {
        let tryNum = x->Float.fromString->Option.map((n): expression => Con(Num(n)))
        switch tryNum {
        | None => raiseParseError(LiteralSymbolError(x))
        | Some(n) => n
        }
      }
    }
  }
  let rec value_of_sexpr = (e: annotated<SExpression.t>) => {
    let ann = e.ann
    switch e.it {
    | Atom(atom) => {ann, it: constant_of_atom(ann, atom)}
    | Sequence(Vector, _b, es) => {
        let es = es->List.map(value_of_sexpr)
        {ann, it: AppPrm(VecNew, es)}
      }
    | Sequence(List, _, _) => raiseParseError(LiteralListError(e))
    }
  }

  let as_id = (context, e: annotated<SExpression.t>) => {
    switch e.it {
    | Atom(Sym(x)) => {it: x, ann: e.ann}
    | _ => raiseParseError(SExprKindError(Atom, context, e))
    }
  }

  let as_list = (context, e: annotated<SExpression.t>) => {
    switch e.it {
    | Sequence(List, _b, ls) => ls
    | _ => raiseParseError(SExprKindError(List, context, e))
    }
  }

  let as_one_then_many = (context, es) => {
    switch es {
    | list{e1, ...es} => (e1, es)
    | _ => raiseParseError(SExprArityError(OneThenMany, context, es))
    }
  }

  let as_many_then_one = (context, es) => {
    switch es {
    | list{e1, ...rest} =>
      switch List.reverse(rest) {
      | list{} => (list{}, e1)
      | list{x, ...xs} => (list{e1, ...List.reverse(xs)}, x)
      }
    | _ => raiseParseError(SExprArityError(ManyThenOne, context, es))
    }
  }

  let as_one = (context, es) => {
    switch es {
    | list{e} => e
    | _ => raiseParseError(SExprArityError(ExactlyOne, context, es))
    }
  }

  let as_two = (context, es) => {
    switch es {
    | list{e1, e2} => (e1, e2)
    | _ => raiseParseError(SExprArityError(ExactlyTwo, context, es))
    }
  }
  let as_three = (context, es) => {
    switch es {
    | list{e1, e2, e3} => (e1, e2, e3)
    | _ => raiseParseError(SExprArityError(ExactlyThree, context, es))
    }
  }
  let as_one_then_many_then_one = (context, es) => {
    switch es {
    | list{e1, e2, ...rest} =>
      switch List.reverse(rest) {
      | list{} => (e1, list{}, e2)
      | list{x, ...xs} => (e1, list{e2, ...List.reverse(xs)}, x)
      }
    | _ => raiseParseError(SExprArityError(OneThenManyThenOne, context, es))
    }
  }

  let as_expr = (context, e) => {
    switch e {
    | Exp(e) => e
    | _ => raiseParseError(TermKindError(Expression, context, e))
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

  let rec letstar = (ann, xes, body): annotated<expression> => {
    switch xes {
    | list{} =>
      switch body {
      | (list{}, e) => e
      | body => {ann, it: Let(list{}, body)}
      }
    | list{(x, e)} => {ann, it: Let(list{(x, e)}, body)}
    | list{(x, e), ...xes} => {ann, it: Let(list{(x, e)}, (list{}, letstar(ann, xes, body)))}
    }
  }

  let rec termOfSExpr = (e: annotated<SExpression.t>) => {
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
        let e = as_expr("an expression", termOfSExpr(e))
        Def({ann, it: Var(x, e)})
      }

    | Sequence(List, _b, list{{it: Atom(Sym("deffun")), ann: _}, ...rest}) => {
        let (head, terms, result) = as_one_then_many_then_one("a function header and a body", rest)
        let (fun, args) = as_one_then_many(
          "function name followed by parameters",
          as_list("function name and parameters", head),
        )
        let fun = as_id("a function name", fun)
        let args = List.map(args, as_id("a parameter"))
        let terms = Belt.List.map(terms, termOfSExpr)
        let result = result |> termOfSExpr |> as_expr("an expression to be returned")
        Def({ann, it: Fun(fun, args, (terms, result))})
      }

    | Sequence(List, _b, list{{it: Atom(Sym("lambda")), ann: _}, ...rest}) => {
        let (args, terms, result) = as_one_then_many_then_one(
          "the function signature followed by the function body",
          rest,
        )
        let args = as_list("function parameters", args)->List.map(as_id("a parameter"))
        let terms = terms->List.map(termOfSExpr)
        let result = result |> termOfSExpr |> as_expr("an expression to be returned")
        Exp({ann, it: Lam(args, (terms, result))})
      }
    | Sequence(List, _b, list{{it: Atom(Sym("λ")), ann: _}, ...rest}) => {
        let (args, terms, result) = as_one_then_many_then_one(
          "the function signature followed by the function body",
          rest,
        )
        let args = as_list("function parameters", args)->List.map(as_id("a parameter"))
        let terms = terms->List.map(termOfSExpr)
        let result = result |> termOfSExpr |> as_expr("an expression to be returned")
        Exp({ann, it: Lam(args, (terms, result))})
      }

    | Sequence(List, _b, list{{it: Atom(Sym("begin")), ann: _}, ...rest}) => {
        let (terms, result) = as_many_then_one("one or more expressions", rest)
        let terms = terms->List.map(termOfSExpr)->List.map(as_expr("an expression"))
        let result = result->termOfSExpr |> as_expr("an expression")
        Exp({ann, it: Bgn(terms, result)})
      }

    | Sequence(List, _b, list{{it: Atom(Sym("set!")), ann: _}, ...rest}) => {
        let (x, e) = as_two("a variable and an expression", rest)
        let x = as_id("a variable to be set", x)
        let e = as_expr("an expression", termOfSExpr(e))
        Exp({ann, it: Set(x, e)})
      }

    | Sequence(List, _b, list{{it: Atom(Sym("if")), ann: _}, ...rest}) => {
        let (e_cnd, e_thn, e_els) = as_three(
          "three expressions (i.e., a condition, the \"then\" branch, and the \"else\" branch)",
          rest,
        )
        let e_cnd = as_expr("a (conditional) expression", termOfSExpr(e_cnd))
        let e_thn = as_expr("an expression", termOfSExpr(e_thn))
        let e_els = as_expr("an expression", termOfSExpr(e_els))
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
          | list{({it: Atom(Sym("else")), ann: _}: annotated<SExpression.t>, terms, result)} => {
              let terms = terms->List.map(termOfSExpr)
              let result = result |> termOfSExpr |> as_expr("an expression")
              Exp({ann, it: Cnd(List.reverse(parsed), Some((terms, result)))})
            }

          | list{(case, terms, result), ...branches} => {
              let case = case->termOfSExpr |> as_expr("a (conditional) expression")
              let terms = terms->List.map(termOfSExpr)
              let result = result |> termOfSExpr |> as_expr("an expression")
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
          let e = termOfSExpr(e) |> as_expr("an expression")
          (x, e)
        })
        let ts = ts->List.map(termOfSExpr)
        let result = termOfSExpr(result) |> as_expr("an expression to be return")
        Exp({ann, it: Let(xes, (ts, result))})
      }

    | Sequence(List, _b, list{{it: Atom(Sym("let*")), ann: _}, ...rest}) => {
        let (xes, ts, result) = as_one_then_many_then_one("the bindings followed by the body", rest)
        let xes =
          as_list("variable-expression pairs", xes)
          ->List.map(as_list("a variable and an expression"))
          ->List.map(as_two("a variable and an expression"))
        let xes = xes->List.map(((x, e)) => {
          let x = as_id("a variable to be bound", x)
          let e = termOfSExpr(e) |> as_expr("an expression")
          (x, e)
        })
        let ts = ts->List.map(termOfSExpr)
        let result = termOfSExpr(result) |> as_expr("an expression to be return")
        Exp(letstar(ann, xes, (ts, result)))
      }

    | Sequence(List, _b, list{{it: Atom(Sym("letrec")), ann: _}, ...rest}) => {
        let (xes, ts, result) = as_one_then_many_then_one("the bindings followed by the body", rest)
        let xes =
          as_list("variable-expression pairs", xes)
          ->List.map(as_list("a variable and an expression"))
          ->List.map(as_two("a variable and an expression"))
        let xes = xes->List.map(((x, e)) => {
          let x = as_id("a variable to be bound", x)
          let e = termOfSExpr(e) |> as_expr("an expression")
          (x, e)
        })
        let ts = ts->List.map(termOfSExpr)
        let result = termOfSExpr(result) |> as_expr("an expression to be return")
        Exp({ann, it: Letrec(xes, (ts, result))})
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
    | Sequence(List, _b, list{{it: Atom(Sym("vec-ref")), ann: _}, ...es}) =>
      app_prm(ann, VecRef, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vref")), ann: _}, ...es}) => app_prm(ann, VecRef, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vec-set!")), ann: _}, ...es}) =>
      app_prm(ann, VecSet, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vset!")), ann: _}, ...es}) => app_prm(ann, VecSet, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vec-len")), ann: _}, ...es}) =>
      app_prm(ann, VecLen, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vlen")), ann: _}, ...es}) => app_prm(ann, VecLen, es)
    | Sequence(List, _b, list{{it: Atom(Sym("eq?")), ann: _}, ...es}) => app_prm(ann, Eqv, es)
    | Sequence(List, _b, list{{it: Atom(Sym("eqv?")), ann: _}, ...es}) => app_prm(ann, Eqv, es)
    | Sequence(List, _b, list{{it: Atom(Sym("equal?")), ann: _}, ...es}) => app_prm(ann, Eqv, es)
    | Sequence(List, _b, list{{it: Atom(Sym("error")), ann: _}, ...es}) => app_prm(ann, Err, es)
    | Sequence(List, _b, list{{it: Atom(Sym("not")), ann: _}, ...es}) => app_prm(ann, Not, es)
    | Sequence(List, _b, es) => {
        let (e, es) = as_one_then_many(
          "a function call/application, which includes a function and then zero or more arguments",
          es,
        )
        let e = e->termOfSExpr |> as_expr("a function")
        let es = es->List.map(termOfSExpr)->List.map(as_expr("an argument"))
        Exp({ann, it: App(e, es)})
      }
    }
  }
  and app_prm = (ann, p, es) => {
    let es = es->List.map(termOfSExpr)->List.map(as_expr("an argument"))
    Exp({ann, it: AppPrm(p, es)})
  }

  let parseTerms = (src: string) => {
    switch src->SExpression.fromString->List.map(termOfSExpr) {
    | terms => terms
    | exception SExpression.SExpressionError(err) =>
      raiseParseError(SExprParseError(SExpression.Error.toString(err)))
    }
  }

  let parseProgram = parseTerms
}

type context =
  | Expr(bool) // the bool indicates whether we are in an infix operation
  | Stat
  | Return
  | TopLevel

module JSPrinter = {
  let consider_context = (e, ctx) => {
    switch ctx {
    | Expr(_) => `${e}`
    | Stat => `${e};`
    | Return => `return ${e};`
    | TopLevel => `console.log(${e});`
    }
  }

  let constantToString = c => {
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

  let listToString = ss => {
    "(" ++ String.concat(", ", ss) ++ ")"
  }

  let xToString = x => {
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

  let defvarToString = (x: string, e) => {
    `let ${xToString(x)} = ${e};`
  }

  let deffunToString = (f, xs, b) => {
    `function ${f}${listToString(xs)} {${indentBlock(b, 2)}\n}`
  }

  let exprLamToString = (xs, b) => {
    `function ${listToString(xs)} {\n  ${indent(b, 2)}\n}`
  }

  let infix_consider_context = (e, ctx) => {
    switch ctx {
    | Expr(true) => `(${e})`
    | _ => consider_context(e, ctx)
    }
  }

  let assign_consider_context = (e, ctx) => {
    switch ctx {
    | Expr(true) => `(${e})`
    | TopLevel => `${e};`
    | Return => `${e};\nreturn;`
    | _ => consider_context(e, ctx)
    }
  }

  let error_consider_context = (e, _ctx) => {
    e
  }

  let exprSetToString = (ctx, x, e) => {
    `${x} = ${e}`->assign_consider_context(ctx)
  }

  let exprApp_prmToString = (ctx, p, es) => {
    switch (p, es) {
    | (Add, es) => `${String.concat(" + ", es)}`->infix_consider_context(ctx)
    | (Sub, es) => `${String.concat(" - ", es)}`->infix_consider_context(ctx)
    | (Mul, es) => `${String.concat(" * ", es)}`->infix_consider_context(ctx)
    | (Div, es) => `${String.concat(" / ", es)}`->infix_consider_context(ctx)
    | (Lt, list{e1, e2}) => `${e1} < ${e2}`->infix_consider_context(ctx)
    | (Eq, list{e1, e2}) => `${e1} === ${e2}`->infix_consider_context(ctx)
    | (Gt, list{e1, e2}) => `${e1} > ${e2}`->infix_consider_context(ctx)
    | (Le, list{e1, e2}) => `${e1} <= ${e2}`->infix_consider_context(ctx)
    | (Ge, list{e1, e2}) => `${e1} >= ${e2}`->infix_consider_context(ctx)
    | (Ne, list{e1, e2}) => `${e1} != ${e2}`->infix_consider_context(ctx)
    | (PairRefLeft, list{e1}) => `${e1}[0]`->consider_context(ctx)
    | (PairRefRight, list{e1}) => `${e1}[1]`->consider_context(ctx)
    | (PairSetLeft, list{e1, e2}) => `${e1}[0] = ${e2}`->assign_consider_context(ctx)
    | (PairSetRight, list{e1, e2}) => `${e1}[1] = ${e2}`->assign_consider_context(ctx)
    | (PairNew, list{e1, e2}) => `[ ${e1}, ${e2} ]`->consider_context(ctx)
    | (VecNew, es) => `[ ${String.concat(", ", es)} ]`->consider_context(ctx)
    | (VecSet, list{e1, e2, e3}) => `${e1}[${e2}] = ${e3}`->assign_consider_context(ctx)
    | (VecRef, list{e1, e2}) => `${e1}[${e2}]`->consider_context(ctx)
    | (VecLen, list{e}) => `${e}.length`->consider_context(ctx)
    | (Eqv, list{e1, e2}) => `${e1} === ${e2}`->infix_consider_context(ctx)
    | (Err, list{e}) => `throw ${e}`->error_consider_context(ctx)
    | (Not, list{e}) => `! ${e}`->infix_consider_context(ctx)
    | _ => "/* a primitive operation not supported yet */"
    }
  }

  let exprAppToString = (e, es) => {
    `${e}${listToString(es)}`
  }

  let exprBgnToString = (es, e) => {
    `(${String.concat(", ", list{...es, e})})`
  }

  let exprCndToString = (ebs: list<(string, string)>, ob) => {
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

  let exprIfToString = (e_cnd: string, e_thn: string, e_els: string) => {
    `(${e_cnd} ? ${e_thn} : ${e_els})`
  }

  let exprLetToString = (xes, b) => {
    `((${xes->List.map(((x, _e)) => x) |> String.concat(", ")})=>{${b}})(${xes->List.map(((
        _x,
        e,
      )) => e) |> String.concat(", ")})`
  }

  let exprLetrecToString = (xes: list<(string, string)>, b) => {
    let b = String.concat(
      ";\n",
      list{...xes->List.map(((x: string, e: string)) => defvarToString(x, e)), b},
    )
    `()=>{${b}})()`
  }

  let rec expToString = (ctx: context, e: annotated<expression>): string => {
    switch e.it {
    | Con(c) => constantToString(c)->consider_context(ctx)
    | Ref(x) => xToString(x.it)->consider_context(ctx)
    | Set(x, e) => exprSetToString(ctx, x->unannotate->xToString, expToString(Expr(false), e))
    | Lam(xs, b) =>
      exprLamToString(
        xs->List.map(unannotate)->List.map(xToString),
        printBlock(Return, b),
      )->consider_context(ctx)
    | AppPrm(p, es) => exprApp_prmToString(ctx, p, es->List.map(expToString(Expr(true))))
    | App(e, es) =>
      exprAppToString(
        expToString(Expr(false), e),
        es->List.map(expToString(Expr(false))),
      )->consider_context(ctx)
    | Let(xes, b) =>
      exprLetToString(xes->List.map(xeToString), printBlock(Return, b))->consider_context(ctx)
    | Letrec(xes, b) =>
      exprLetrecToString(xes->List.map(xeToString), printBlock(Return, b))->consider_context(ctx)
    | Cnd(ebs, ob) => exprCndToString(ebs->List.map(ebToString(ctx)), obToString(ctx, ob))
    | If(e_cnd, e_thn, e_els) =>
      exprIfToString(
        expToString(Expr(true), e_cnd),
        expToString(Expr(true), e_thn),
        expToString(Expr(true), e_els),
      )->consider_context(ctx)
    | Bgn(es, e) =>
      exprBgnToString(
        es->List.map(expToString(Expr(false))),
        expToString(Expr(false), e),
      )->consider_context(ctx)
    }
  }
  and defToString = (d: annotated<definition>): string => {
    switch d.it {
    | Var(x, e) => defvarToString(x.it, expToString(Expr(false), e))
    | Fun(f, xs, b) =>
      deffunToString(
        f->unannotate->xToString,
        xs->List.map(unannotate)->List.map(xToString),
        printBlock(Return, b),
      )
    }
  }
  and xeToString = xe => {
    let (x, e) = xe
    (xToString(x.it), expToString(Expr(false), e))
  }
  and ebToString = (ctx, eb) => {
    let (e, b) = eb
    (expToString(Expr(false), e), printBlock(ctx, b))
  }
  and obToString = (ctx, ob) => {
    ob->Option.map(printBlock(ctx))
  }
  and termAsStat = t => {
    switch t {
    | Exp(e) => expToString(Stat, e)
    | Def(d) => defToString(d)
    }
  }
  and printBlock = (ctx, b) => {
    let (ts, e) = b
    String.concat("\n", list{...ts->List.map(termAsStat), expToString(ctx, e)})
  }
  and printTerm = t => {
    switch t {
    | Exp(e) => expToString(Expr(false), e)
    | Def(d) => defToString(d)
    }
  }

  let printProgram = p => {
    let tts = t => {
      switch t {
      | Exp(e) => expToString(TopLevel, e)
      | Def(d) => defToString(d)
      }
    }
    String.concat("\n", p->List.map(tts))
  }

  let printBlock = ((ts, e)) => {
    String.concat("\n", list{...ts->List.map(termAsStat), expToString(Return, e)})
  }
}

module ScalaPrinter = {
  let mutatingVariable = ref(false)
  let usingBuffer = ref(false)

  let consider_context = (e, ctx) => {
    switch ctx {
    | Expr(_) => `${e}`
    | Stat => `${e}`
    | Return => `${e}`
    | TopLevel => `println(${e})`
    }
  }

  let constantToString = c => {
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

  let listToString = ss => {
    "(" ++ String.concat(", ", ss) ++ ")"
  }

  let paraListToString = ss => {
    if ss == list{} {
      ""
    } else {
      listToString(ss)
    }
  }

  let xToString = x => {
    let re = %re("/-./g")
    let matchFn = (matchPart, _offset, _wholeString) => {
      Js.String2.toUpperCase(Js.String2.substringToEnd(matchPart, ~from=1))
    }
    let x = Js.String2.unsafeReplaceBy0(x, re, matchFn)

    // add `$` to the beginning of reserved words
    if x == "var" {
      "$var"
    } else if x == "+" {
      "(x, y) => (x + y)"
    } else if x == "-" {
      "(x, y) => (x - y)"
    } else if x == "*" {
      "(x, y) => (x * y)"
    } else if x == "/" {
      "(x, y) => (x / y)"
    } else {
      x
    }
  }

  let parameterToString = x => {
    `${xToString(x)} : Int`
  }

  let defvarToString = (x: string, e) => {
    `${(mutatingVariable.contents) ? "var" : "val"} ${x} = ${e}`
  }

  let deffunToString = (f, xs, b) => {
    `def ${f}${paraListToString(xs)} =${indentBlock(b, 2)}`
  }

  let exprLamToString = (xs, b) => {
    `${listToString(xs)} =>${indentBlock(b, 2)}`
  }

  let infix_consider_context = (e, ctx) => {
    switch ctx {
    | Expr(true) => `(${e})`
    | _ => consider_context(e, ctx)
    }
  }

  let assign_consider_context = (e, ctx) => {
    switch ctx {
    | Expr(true) => `(${e})`
    | TopLevel => `${e}`
    | Return => `${e}`
    | _ => consider_context(e, ctx)
    }
  }

  let error_consider_context = (e, _ctx) => {
    e
  }

  let exprSetToString = (ctx, x, e) => {
    `${x} = ${e}`->assign_consider_context(ctx)
  }

  let exprApp_prmToString = (ctx, p, es) => {
    switch (p, es) {
    | (Add, es) => `${String.concat(" + ", es)}`->infix_consider_context(ctx)
    | (Sub, es) => `${String.concat(" - ", es)}`->infix_consider_context(ctx)
    | (Mul, es) => `${String.concat(" * ", es)}`->infix_consider_context(ctx)
    | (Div, es) => `${String.concat(" / ", es)}`->infix_consider_context(ctx)
    | (Lt, list{e1, e2}) => `${e1} < ${e2}`->infix_consider_context(ctx)
    | (Eq, list{e1, e2}) => `${e1} === ${e2}`->infix_consider_context(ctx)
    | (Gt, list{e1, e2}) => `${e1} > ${e2}`->infix_consider_context(ctx)
    | (Le, list{e1, e2}) => `${e1} <= ${e2}`->infix_consider_context(ctx)
    | (Ge, list{e1, e2}) => `${e1} >= ${e2}`->infix_consider_context(ctx)
    | (Ne, list{e1, e2}) => `${e1} != ${e2}`->infix_consider_context(ctx)
    | (PairRefLeft, list{e1}) => `${e1}(0)`->consider_context(ctx)
    | (PairRefRight, list{e1}) => `${e1}(1)`->consider_context(ctx)
    | (PairSetLeft, list{e1, e2}) => `${e1}(0) = ${e2}`->assign_consider_context(ctx)
    | (PairSetRight, list{e1, e2}) => `${e1}(1) = ${e2}`->assign_consider_context(ctx)
    | (PairNew, list{e1, e2}) =>
      `${if usingBuffer.contents {
          "Buffer"
        } else {
          ""
        }}(${e1}, ${e2})`->consider_context(ctx)
    | (VecNew, es) =>
      `${if usingBuffer.contents {
          "Buffer"
        } else {
          ""
        }}(${String.concat(", ", es)})`->consider_context(ctx)
    | (VecSet, list{e1, e2, e3}) => `${e1}(${e2}) = ${e3}`->assign_consider_context(ctx)
    | (VecRef, list{e1, e2}) => `${e1}(${e2})`->consider_context(ctx)
    | (VecLen, list{e}) => `${e}.length`->consider_context(ctx)
    | (Eqv, list{e1, e2}) => `${e1} === ${e2}`->infix_consider_context(ctx)
    | (Err, list{e}) => `throw ${e}`->error_consider_context(ctx)
    | (Not, list{e}) => `! ${e}`->infix_consider_context(ctx)
    | _ => "/* a primitive operation not supported yet */"
    }
  }

  let exprAppToString = (e, es) => {
    `${e}${paraListToString(es)}`
  }

  let exprBgnToString = (es, e) => {
    `(${String.concat(", ", list{...es, e})})`
  }

  let exprCndToString = (ebs: list<(string, string)>, ob) => {
    let ob = {
      switch ob {
      | None => ""
      | Some(b) => ` else {\n  ${indent(b, 2)}\n}`
      }
    }
    let ebs = ebs->List.map(((e, b)) => `if (${e})${indentBlock(b, 2)}\n`)
    let ebs = String.concat(" else ", ebs)
    ebs ++ ob
  }

  let exprIfToString = (e_cnd: string, e_thn: string, e_els: string) => {
    `(${e_cnd} ? ${e_thn} : ${e_els})`
  }

  let exprLetToString = (xes, b) => {
    `((${xes->List.map(((x, _e)) => x) |> String.concat(", ")})=>{${b}})(${xes->List.map(((
        _x,
        e,
      )) => e) |> String.concat(", ")})`
  }

  let exprLetrecToString = (xes: list<(string, string)>, b) => {
    let b = String.concat(
      "\n",
      list{...xes->List.map(((x: string, e: string)) => defvarToString(x, e)), b},
    )
    `()=>{${b}})()`
  }

  let rec expToString = (ctx: context, e: annotated<expression>): string => {
    switch e.it {
    | Con(c) => constantToString(c)->consider_context(ctx)
    | Ref(x) => xToString(x.it)->consider_context(ctx)
    | Set(x, e) => exprSetToString(ctx, x->unannotate->xToString, expToString(Expr(false), e))
    | Lam(xs, b) =>
      exprLamToString(
        xs->List.map(unannotate)->List.map(parameterToString),
        printBlock(Return, b),
      )->consider_context(ctx)
    | AppPrm(p, es) => exprApp_prmToString(ctx, p, es->List.map(expToString(Expr(true))))
    | App(e, es) =>
      exprAppToString(
        expToString(Expr(false), e),
        es->List.map(expToString(Expr(false))),
      )->consider_context(ctx)
    | Let(xes, b) =>
      exprLetToString(xes->List.map(xeToString), printBlock(Return, b))->consider_context(ctx)
    | Letrec(xes, b) =>
      exprLetrecToString(xes->List.map(xeToString), printBlock(Return, b))->consider_context(ctx)
    | Cnd(ebs, ob) => exprCndToString(ebs->List.map(ebToString(ctx)), obToString(ctx, ob))
    | If(e_cnd, e_thn, e_els) =>
      exprIfToString(
        expToString(Expr(true), e_cnd),
        expToString(Expr(true), e_thn),
        expToString(Expr(true), e_els),
      )->consider_context(ctx)
    | Bgn(es, e) =>
      exprBgnToString(
        es->List.map(expToString(Expr(false))),
        expToString(Expr(false), e),
      )->consider_context(ctx)
    }
  }
  and defToString = (d: annotated<definition>): string => {
    switch d.it {
    | Var(x, e) => defvarToString(x.it, expToString(Expr(false), e))
    | Fun(f, xs, b) =>
      deffunToString(
        f->unannotate->xToString,
        xs->List.map(unannotate)->List.map(parameterToString),
        printBlock(Return, b),
      )
    }
  }
  and xeToString = xe => {
    let (x, e) = xe
    (xToString(x.it), expToString(Expr(false), e))
  }
  and ebToString = (ctx, eb) => {
    let (e, b) = eb
    (expToString(Expr(false), e), printBlock(ctx, b))
  }
  and obToString = (ctx, ob) => {
    ob->Option.map(printBlock(ctx))
  }
  and termAsStat = t => {
    switch t {
    | Exp(e) => expToString(Stat, e)
    | Def(d) => defToString(d)
    }
  }
  and printBlock = (ctx, b) => {
    let (ts, e) = b
    String.concat("\n", list{...ts->List.map(termAsStat), expToString(ctx, e)})
  }
  and printTerm = t => {
    usingBuffer := true
    switch t {
    | Exp(e) => expToString(Expr(false), e)
    | Def(d) => defToString(d)
    }
  }

  let printProgram = p => {
    // when no variable mutation
    mutatingVariable := Js.String.match_(%re("/[(]set!/"), SMoLPrinter.printProgram(p)) != None
    // when no mutation at all
    // usingBuffer := true
    // usingBuffer := Js.String.match_(%re("/vec-set!/"), SMoLPrinter.printProgram(p)) != None
    usingBuffer := Js.String.match_(%re("/set!/"), SMoLPrinter.printProgram(p)) != None
    let tts = t => {
      switch t {
      | Exp(e) => expToString(TopLevel, e)
      | Def(d) => defToString(d)
      }
    }
    String.concat("\n", p->List.map(tts))
  }

  let printBlock = ((ts, e)) => {
    String.concat("\n", list{...ts->List.map(termAsStat), expToString(Return, e)})
  }
}

type js_context = context
exception Impossible(string)
module PYPrinter = {
  // Python translation is tricky because we need to know whether a variable
  // reference is pointing to non-local variable.
  // - If the variable is local, we proceed normally.
  // - If the variable is external but neither global nor built-in, we need to declare it local in the local scope
  // - If the variable is global, we need to declare it global
  // - If the variable is built-in, we proceed normally

  type placeOfDef =
    | BuiltIn
    | Global
    | NonLocal
    | Local

  type environment = Js.Dict.t<placeOfDef>

  // When generating Python code, we need to think about where we are in
  // an AST tree
  type context = {
    node: js_context,
    block: placeOfDef,
    refs: Js.Array.t<symbol>,
    env: environment,
  }

  let base_env = Js.Dict.fromArray(
    all_primitives->Array.map(p => (SMoLPrinter.primitiveToString(p), BuiltIn)),
  )
  let make_global_env = xs => {
    let env = Js.Dict.entries(base_env)
    Js.Dict.fromArray(Array.concat(env, xs->List.map(x => (x, Global))->List.toArray))
  }
  let make_local_env = (env, xs) => {
    let env = Js.Dict.entries(env)
    let env = env->Array.map(((x, p)) => {
      let p = switch p {
      | Local => NonLocal
      | p => p
      }
      (x, p)
    })
    Js.Dict.fromArray(Array.concat(env, xs->List.map(x => (x, Local))->List.toArray))
  }

  let constantToString = c => {
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

  let listToString = ss => {
    "(" ++ String.concat(", ", ss) ++ ")"
  }

  let xToString = x => {
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

  let defvarToString = (x, e) => {
    `${xToString(x)} = ${e}`
  }

  let deffunToString = (f, xs, b) => {
    `def ${f}${listToString(xs)}:\n    ${indent(b, 4)}`
  }

  let exprSetToString = (ctx, x, e) => {
    switch ctx.node {
    | Expr(true) => `(${x} := ${e})`
    | Expr(false) => `${x} := ${e}`
    | Stat => `${x} = ${e}`
    | Return => `return (${x} := ${e})`
    | TopLevel => `${x} = ${e}`
    }
  }

  let exprLamToString = (xs, b) => {
    if xs == list{} {
      `lambda: ${b}`
    } else {
      `lambda ${String.concat(",", xs)}: ${b}`
    }
  }

  let ret = (ctx, code) => {
    switch ctx.node {
    | Return => `return ${code}`
    | TopLevel => `print(${code})`
    | _ => code
    }
  }
  let wrap = (ctx, code) => {
    switch ctx.node {
    | Expr(true) => `(${code})`
    | _ => ret(ctx, code)
    }
  }

  let exprApp_prmToString = (ctx, p, es) => {
    switch (p, es) {
    | (Add, es) => `${String.concat(" + ", es)}` |> wrap(ctx)
    | (Sub, es) => `${String.concat(" - ", es)}` |> wrap(ctx)
    | (Mul, es) => `${String.concat(" * ", es)}` |> wrap(ctx)
    | (Div, es) => `${String.concat(" / ", es)}` |> wrap(ctx)
    | (Lt, list{e1, e2}) => `${e1} < ${e2}` |> wrap(ctx)
    | (Eq, list{e1, e2}) => `${e1} == ${e2}` |> wrap(ctx)
    | (Gt, list{e1, e2}) => `${e1} > ${e2}` |> wrap(ctx)
    | (Le, list{e1, e2}) => `${e1} <= ${e2}` |> wrap(ctx)
    | (Ge, list{e1, e2}) => `${e1} >= ${e2}` |> wrap(ctx)
    | (Ne, list{e1, e2}) => `${e1} != ${e2}` |> wrap(ctx)
    | (PairRefLeft, list{e1}) => `${e1}[0]` |> wrap(ctx)
    | (PairRefRight, list{e1}) => `${e1}[1]` |> wrap(ctx)
    | (PairSetLeft, list{e1, e2}) =>
      switch ctx.node {
      | Stat => `${e1}[0] = ${e2}`
      | TopLevel => `${e1}[0] = ${e2}`
      | Expr(true) => `${e1}.__setitem__(0, ${e2})`
      | Expr(false) => `${e1}.__setitem__(0, ${e2})`
      | Return => `return ${e1}.__setitem__(0, ${e2})`
      }
    | (PairSetRight, list{e1, e2}) =>
      switch ctx.node {
      | Stat => `${e1}[1] = ${e2}`
      | TopLevel => `${e1}[1] = ${e2}`
      | Expr(true) => `${e1}.__setitem__(1, ${e2})`
      | Expr(false) => `${e1}.__setitem__(1, ${e2})`
      | Return => `return ${e1}.__setitem__(1, ${e2})`
      }
    | (PairNew, list{e1, e2}) => `[ ${e1}, ${e2} ]` |> ret(ctx)
    | (VecNew, es) => `[${String.concat(", ", es)}]` |> ret(ctx)
    | (VecSet, list{e1, e2, e3}) =>
      switch ctx.node {
      | Stat => `${e1}[${e2}] = ${e3}`
      | TopLevel => `${e1}[${e2}] = ${e3}`
      | Expr(true) => `${e1}.__setitem__(${e2}, ${e3})`
      | Expr(false) => `${e1}.__setitem__(${e2}, ${e3})`
      | Return => `return ${e1}.__setitem__(${e2}, ${e3})`
      }
    | (VecRef, list{e1, e2}) => `${e1}[${e2}]` |> ret(ctx)
    | (VecLen, list{e}) => `len(${e})` |> ret(ctx)
    | (Eqv, list{e1, e2}) => `${e1} == ${e2}` |> wrap(ctx)
    | (Err, list{e}) => `raise ${e}`
    | (Not, list{e}) => `not ${e}` |> wrap(ctx)
    | (p, _) =>
      raise(
        SMoLPrintError(
          `found a primitive operation (${SMoLPrinter.primitiveToString(p)}) not supported yet.`,
        ),
      )
    }
  }

  let exprAppToString = (e, es) => {
    `${e}${listToString(es)}`
  }

  let exprCndToString = (ebs: list<(string, string)>, ob) => {
    let ob = {
      switch ob {
      | None => ""
      | Some(b) => `else:${indentBlock(b, 4)}`
      }
    }
    let ebs = ebs->List.map(((e, b)) => `if ${e}:${indentBlock(b, 4)}\n`)
    let ebs = String.concat("el", ebs)
    ebs ++ ob
  }

  let exprIfToString = (ctx, e_cnd: string, e_thn: string, e_els: string) => {
    switch ctx.node {
    | Expr(_) => `${e_thn} if ${e_cnd} else ${e_els}`
    | _ => `if ${e_cnd}:${indentBlock(e_thn, 4)}\nelse:${indentBlock(e_els, 4)}`
    }
  }

  let exprLetToString = (xes, b) => {
    exprAppToString(exprLamToString(xes->List.map(((x, e)) => `${x}=${e}`), b), list{})
  }
  let exprLetrecToString = (_xes, _b) => {
    raise(SMoLPrintError("Python translation does not support letrec-expression."))
  }

  let consider_context = (code: string, ctx: context) => {
    switch ctx.node {
    | Return => `return ${code}`
    | TopLevel => `print(${code})`
    | _ => code
    }
  }

  let rec expToString = (ctx: context, e: annotated<expression>): string => {
    switch e.it {
    | Con(c) => constantToString(c)->consider_context(ctx)
    | Ref(x) => xToString(x.it)->consider_context(ctx)
    | Set(x, e) => {
        let _ = Js.Array.unshift(unannotate(x), ctx.refs)
        exprSetToString(
          ctx,
          x->unannotate->xToString,
          expToString(
            {
              ...ctx,
              node: Expr(false),
            },
            e,
          ),
        )
      }
    | Lam(xs, b) =>
      exprLamToString(
        xs->List.map(unannotate)->List.map(xToString),
        printBlock({...ctx, node: Expr(false)}, xs, b),
      )->consider_context(ctx)
    | AppPrm(VecSet, es) =>
      exprApp_prmToString(ctx, VecSet, es->List.map(expToString({...ctx, node: Expr(false)})))
    | AppPrm(p, es) =>
      exprApp_prmToString(
        ctx,
        p,
        es->List.map(
          expToString({
            ...ctx,
            node: Expr(true),
          }),
        ),
      )
    | App(e, es) =>
      exprAppToString(
        expToString({...ctx, node: Expr(false)}, e),
        es->List.map(expToString({...ctx, node: Expr(false)})),
      )->consider_context(ctx)
    | Let(xes, b) =>
      exprLetToString(
        xes->List.map(xeToString(ctx)),
        printBlock({...ctx, node: Expr(false)}, xes->List.map(((x, _e)) => x), b),
      )->consider_context(ctx)
    | Letrec(xes, b) =>
      exprLetrecToString(
        xes->List.map(xeToString(ctx)),
        printBlock({...ctx, node: Expr(false)}, xes->List.map(((x, _e)) => x), b),
      )->consider_context(ctx)
    | Cnd(ebs, ob) =>
      switch ctx.node {
      | Expr(_) => raise(SMoLPrintError("Python translation does not fully support `cond` yet."))
      | _ => exprCndToString(ebs->List.map(ebToString(ctx)), obToString(ctx, ob))
      }
    | If(e_cnd, e_thn, e_els) =>
      exprIfToString(
        ctx,
        expToString({...ctx, node: Expr(false)}, e_cnd),
        expToString(ctx, e_thn),
        expToString(ctx, e_els),
      )
    | Bgn(es, e) => exprBgnToString(ctx, es, e)
    }
  }
  and exprBgnToString = (ctx, es, e) => {
    switch ctx.node {
    | Expr(_) => {
        let ese = list{...es, e}
        let ese = ese->List.map(expToString({...ctx, node: Expr(false)}))
        `[${String.concat(", ", ese)}][-1]`
      }
    | _ => {
        let es = es->List.map(expToString({...ctx, node: Stat}))
        let e = e |> expToString(ctx)
        String.concat("\n", list{...es, e})
      }
    }
  }
  and defToString = (ctx: context, d: annotated<definition>): string => {
    switch d.it {
    | Var(x, e) => defvarToString(x.it, expToString({...ctx, node: Expr(false)}, e))
    | Fun(f, xs, b) =>
      deffunToString(
        f->unannotate->xToString,
        xs->List.map(unannotate)->List.map(xToString),
        printBlock({...ctx, node: Return}, xs, b),
      )
    }
  }
  and xeToString = (ctx: context, xe) => {
    let (x, e) = xe
    (xToString(x.it), expToString({...ctx, node: Expr(false)}, e))
  }
  and ebToString = (ctx: context, eb) => {
    let (e, b) = eb
    (expToString({...ctx, node: Expr(false)}, e), printBlock(ctx, list{}, b))
  }
  and obToString = (ctx: context, ob) => {
    ob->Option.map(printBlock(ctx, list{}))
  }
  and identifier_of_term = t => {
    switch t {
    | Def(d) =>
      switch d.it {
      | Var(x, _e) => Some(x)
      | Fun(f, _xs, _b) => Some(f)
      }
    | Exp(_) => None
    }
  }
  and xs_of_ts = ts => ts->List.keepMap(identifier_of_term)->List.map(unannotate)

  and printBlock = (ctx: context, xs, b: block) => {
    let (ts, e) = b
    let refs: Js.Array.t<symbol> = []
    let ys: list<string> = ts->xs_of_ts
    // extend the outside environment
    let ctx = {...ctx, env: make_local_env(ctx.env, list{...xs->List.map(unannotate), ...ys})}
    // now we must be in a local scope
    let ctx = {...ctx, block: Local}
    let ctx = {...ctx, refs}
    // Js.Console.log("Set up a new environment and a new refs!")
    // Js.Console.log(ctx.env)
    // Js.Console.log(ctx.refs)
    switch ctx.node {
    | Expr(_) => {
        let block_as_expr = (ctx, ts, e) => {
          switch ts {
          | list{} => expToString(ctx, e)
          | ts => {
              let is_exp = t =>
                switch t {
                | Exp(_) => true
                | Def(_) => false
                }
              let as_exp = t =>
                switch t {
                | Exp(e) => e
                | Def(_) => raise(Impossible("We have checked!"))
                }
              if List.every(ts, is_exp) {
                let es = list{...ts->List.map(as_exp), e}
                let es = es->List.map(expToString({...ctx, node: Expr(false)}))
                `(${String.concat(", ", es)})[-1]`
              } else {
                raise(
                  SMoLPrintError(
                    "Python translator can't translate block that contains definitions and appears in an expression context.",
                  ),
                )
              }
            }
          }
        }
        let result = block_as_expr(ctx, ts, e)
        let refs = refs |> Js.Array.filter(x => {
          switch Js.Dict.get(ctx.env, x)->Option.getWithDefault(NonLocal) {
          | Local | BuiltIn => false
          | _ => true
          }
        })

        if Js.Array.length(refs) == 0 {
          result
        } else {
          `("WARNING: the translation might be inaccurate", ${result})[-1]`
        }
      }
    | _ =>
      let result = String.concat(
        "\n",
        list{...ts->List.map(termToString({...ctx, node: Stat})), expToString(ctx, e)},
      )
      let refs = refs |> Js.Array.filter(x => {
        switch Js.Dict.get(ctx.env, x)->Option.getWithDefault(NonLocal) {
        | Local | BuiltIn => false
        | _ => true
        }
      })
      let globals = refs |> Js.Array.filter(x => {
        switch Js.Dict.get(ctx.env, x)->Option.getWithDefault(NonLocal) {
        | Global => true
        | _ => false
        }
      })
      let nonlocals = refs |> Js.Array.filter(x => {
        switch Js.Dict.get(ctx.env, x)->Option.getWithDefault(NonLocal) {
        | NonLocal => true
        | _ => false
        }
      })
      let decl_globals = if Array.length(globals) == 0 {
        ""
      } else {
        `global ${String.concat(", ", globals->List.fromArray)}\n`
      }
      let decl_nonlocals = if Array.length(nonlocals) == 0 {
        ""
      } else {
        `nonlocal ${String.concat(", ", nonlocals->List.fromArray)}\n`
      }
      `${decl_globals}${decl_nonlocals}${result}`
    }
  }
  and termToString = (ctx, t) => {
    switch t {
    | Exp(e) => expToString(ctx, e)
    | Def(d) => defToString(ctx, d)
    }
  }

  let printTerm = t => {
    let ctx = {node: Stat, block: Global, env: make_global_env(list{t}->xs_of_ts), refs: []}
    termToString(ctx, t)
  }

  let printProgram = ts => {
    let ctx = {node: TopLevel, block: Global, env: make_global_env(ts->xs_of_ts), refs: []}
    String.concat("\n", ts->List.map(termToString(ctx)))
  }

  let printBlock = ((ts, e)) => {
    let ctx = {node: Return, block: Local, env: Js.Dict.empty(), refs: []}
    printBlock(ctx, list{}, (ts, e))
  }
}

module type Translator = {
  // print terms, interleaved with whitespace
  let translateTerms: string => string
  // print runnable full programs
  let translateProgram: string => string
}
module TranslateError = {
  type t =
    | ParseError(ParseError.t)
    | PrintError(string)
  let toString = t => {
    switch t {
    | ParseError(err) => ParseError.toString(err)
    | PrintError(err) => err
    }
  }
}
exception SMoLTranslateError(TranslateError.t)

module PYTranslator = {
  let translateTerms = src => {
    switch Parser.parseTerms(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | ts =>
      switch String.concat(" ", ts->List.map(PYPrinter.printTerm)) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | dst => dst
      }
    }
  }
  let translateProgram = src => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch PYPrinter.printProgram(p) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | dst => dst
      }
    }
  }
}

module JSTranslator = {
  let translateTerms = src => {
    switch Parser.parseTerms(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | ts =>
      switch String.concat(" ", ts->List.map(JSPrinter.printTerm)) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | dst => dst
      }
    }
  }
  let translateProgram = src => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch JSPrinter.printProgram(p) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | dst => dst
      }
    }
  }
}

module ScalaTranslator = {
  let translateTerms = src => {
    switch Parser.parseTerms(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | ts =>
      switch String.concat(" ", ts->List.map(ScalaPrinter.printTerm)) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | dst => dst
      }
    }
  }
  let translateProgram = src => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch ScalaPrinter.printProgram(p) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | dst => dst
      }
    }
  }
}
