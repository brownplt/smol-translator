open Belt
open SExpression

type annotated<'it, 'ann> = {it: 'it, ann: 'ann}
let mapAnn = (f, {ann, it}: annotated<_, _>): annotated<_, _> => {
  {
    ann,
    it: f(it),
  }
}

module Print = {
  type rec t =
    | Plain(string)
    | Group(list<annotated<t, option<srcrange>>>)
  let rec toString: t => string = it => {
    switch it {
    | Plain(s) => s
    | Group(ts) =>
      String.concat(
        "",
        ts->List.map(({it }) => {
          toString(it)
        }),
      )
    }
  }
  let rec map = (f, it) => {
    switch it {
    | Plain(s) => Plain(f(s))
    | Group(ts) => Group(ts->List.map(mapAnn(map(f))))
    }
  }
}
open Print

let printConcat = (t: string, ss: list<_>): t => {
  let intersperse = (x, ys) => {
    switch ys {
    | list{} => list{}
    | list{y} => list{y}
    | list{y, ...ys} => {
        let rec loop = ys => {
          switch ys {
          | list{} => list{}
          | list{y, ...ys} => list{x, y, ...loop(ys)}
          }
        }
        list{y, ...loop(ys)}
      }
    }
  }
  Group(intersperse({it: Plain(t), ann: None}, ss))
}

let rec containsNL = it => {
  switch it {
  | Plain(s) => String.contains(s, '\n')
  | Group(ts) =>
    ts->List.some(({it}) => {
      containsNL(it)
    })
  }
}
let plain = s => {it: Plain(s), ann: None}
let group = ss => {it: Group(ss), ann: None}
let group2 = (s1, s2) => group(list{s1, s2})
let surround = (prefix, s, suffix) => Group(list{plain(prefix), s, plain(suffix)})
let dummy = it => {it, ann: None}

type constant =
  | Uni
  | Nil
  | Num(float)
  | Lgc(bool)
  | Str(string)

type rec val =
  | Con(constant)
  | Lst(list<val>)
  | Vec(list<val>)
type outputlet =
  | OVal(val)
  | OErr
type output = list<outputlet>

module Primitive = {
  type arith =
    | Add
    | Sub
    | Mul
    | Div
  type cmp =
    | Lt
    | Eq
    | Gt
    | Le
    | Ge
    | Ne
  type t =
    | Arith(arith)
    | Cmp(cmp)
    | PairNew
    | PairRefLeft
    | PairRefRight
    | PairSetLeft
    | PairSetRight
    | VecNew
    | VecRef
    | VecSet
    | VecLen
    | Err
    | Not
    | Print
    | Next
    | Cons
  let toString: t => string = t => {
    switch t {
    | Arith(Add) => "+"
    | Arith(Sub) => "-"
    | Arith(Mul) => "*"
    | Arith(Div) => "/"
    | Cmp(Lt) => "<"
    | Cmp(Gt) => ">"
    | Cmp(Le) => "<="
    | Cmp(Ge) => ">="
    | Cmp(Ne) => "!="
    | Cmp(Eq) => "eq?"
    | PairNew => "mpair"
    | PairRefLeft => "left"
    | PairRefRight => "right"
    | PairSetLeft => "set-left!"
    | PairSetRight => "set-right!"
    | VecNew => "mvec"
    | VecRef => "vec-ref"
    | VecSet => "vec-set!"
    | VecLen => "vec-len"
    | Err => "error"
    | Not => "not"
    | Print => "print"
    | Next => "next"
    | Cons => "cons"
    }
  }
}
open Primitive

type symbol = string

type rec expressionNode<'ann> =
  | Con(constant)
  | Ref(symbol)
  | Set(annotated<symbol, 'ann>, expression<'ann>)
  | Lam(list<annotated<symbol, 'ann>>, block<'ann>)
  | Let(list<bind<'ann>>, block<'ann>)
  | Letrec(list<bind<'ann>>, block<'ann>)
  | AppPrm(Primitive.t, list<expression<'ann>>)
  | App(expression<'ann>, list<expression<'ann>>)
  | Bgn(list<expression<'ann>>, expression<'ann>)
  | If(expression<'ann>, expression<'ann>, expression<'ann>)
  | Cnd(list<(expression<'ann>, block<'ann>)>, option<block<'ann>>)
  | GLam(list<annotated<symbol, 'ann>>, block<'ann>)
  | Yield(expression<'ann>)
and expression<'ann> = annotated<expressionNode<'ann>, 'ann>
and bindNode<'ann> = (annotated<symbol, 'ann>, expression<'ann>)
and bind<'ann> = annotated<bindNode<'ann>, 'ann>
and blockNode<'ann> = (list<term<'ann>>, expression<'ann>)
and block<'ann> = annotated<blockNode<'ann>, 'ann>

and definitionNode<'ann> =
  | Var(annotated<symbol, 'ann>, expression<'ann>)
  | Fun(annotated<symbol, 'ann>, list<annotated<symbol, 'ann>>, block<'ann>)
  | GFun(annotated<symbol, 'ann>, list<annotated<symbol, 'ann>>, block<'ann>)
and definition<'ann> = annotated<definitionNode<'ann>, 'ann>

and termNode<'ann> = Def(definitionNode<'ann>) | Exp(expressionNode<'ann>)
and term<'ann> = annotated<termNode<'ann>, 'ann>

and programNode<'ann> = list<term<'ann>>
and program<'ann> = annotated<programNode<'ann>, 'ann>

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
    | SExprKindError(SExprKind.t, string, SExpression.annotated<SExpression.t>)
    | SExprArityError(Arity.t, string, list<SExpression.annotated<SExpression.t>>)
    | LiteralSymbolError(string)
    | LiteralListError(SExpression.annotated<SExpression.t>)
    | TermKindError(TermKind.t, string, term<srcrange>)
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
      // `expecting ${context}, given ${SMoLPrinter.printTerm(term)}`
      `expecting ${context}, given something else at ${SrcLoc.toString(
          term.ann.begin,
        )}-${SrcLoc.toString(term.ann.end)}`
    }
  }
}
exception SMoLParseError(ParseError.t)
let raiseParseError = err => raise(SMoLParseError(err))

let makeBlock = (ts, e) => {
  {
    ann: {
      begin: ts->List.head->Option.map(t => t.ann.begin)->Option.getWithDefault(e.ann.begin),
      end: e.ann.end,
    },
    it: (ts, e),
  }
}
module Parser = {
  let constant_of_atom = atom => {
    switch atom {
    | SExpression.Atom.Str(s) => Str(s)
    | Sym("#t") => Lgc(true)
    | Sym("#f") => Lgc(false)
    | Sym(x) => {
        let tryNum = x->Float.fromString
        switch tryNum {
        | None => raiseParseError(LiteralSymbolError(x))
        | Some(n) => Num(n)
        }
      }
    }
  }

  let outputletOfSExpr = (e: SExpression.annotated<SExpression.t>): outputlet => {
    let {ann: _, it} = e
    switch it {
    | Atom(Sym("error")) => OErr
    | _ =>
      OVal({
        let rec p = (e: SExpression.annotated<SExpression.t>): val => {
          switch e.it {
          | Atom(atom) => Con(constant_of_atom(atom))
          | Sequence(Vector, _b, es) => Vec(es->List.map(p))
          | Sequence(List, _b, es) => Lst(es->List.map(p))
          }
        }
        p(e)
      })
    }
  }

  let rec parseValue = (e: SExpression.annotated<SExpression.t>) => {
    let {ann, it} = e
    switch it {
    | Atom(atom) => {ann, it: Con(constant_of_atom(atom))}
    | Sequence(Vector, _b, es) => {
        let es = es->List.map(parseValue)
        {ann, it: AppPrm(VecNew, es)}
      }
    | Sequence(List, _, _) => raiseParseError(LiteralListError(e))
    }
  }

  let as_id = (context, e: SExpression.annotated<SExpression.t>) => {
    switch e.it {
    | Atom(Sym(x)) => {it: x, ann: e.ann}
    | _ => raiseParseError(SExprKindError(Atom, context, e))
    }
  }

  let as_list = (context, e: SExpression.annotated<SExpression.t>) => {
    let {ann, it} = e
    switch it {
    | Sequence(List, _b, ls) => {ann, it: ls}
    | _ => raiseParseError(SExprKindError(List, context, e))
    }
  }

  let as_one_then_many = (context, es: list<SExpression.annotated<_>>) => {
    switch es {
    | list{e1, ...es} => (e1, es)
    | _ => raiseParseError(SExprArityError(OneThenMany, context, es))
    }
  }

  let as_many_then_one = (context, es: list<SExpression.annotated<_>>) => {
    switch es {
    | list{e1, ...rest} =>
      switch List.reverse(rest) {
      | list{} => (list{}, e1)
      | list{x, ...xs} => (list{e1, ...List.reverse(xs)}, x)
      }
    | _ => raiseParseError(SExprArityError(ManyThenOne, context, es))
    }
  }

  let as_one = (context, es: list<SExpression.annotated<_>>) => {
    switch es {
    | list{e} => e
    | _ => raiseParseError(SExprArityError(ExactlyOne, context, es))
    }
  }

  let as_two = (context, es: list<SExpression.annotated<_>>) => {
    switch es {
    | list{e1, e2} => (e1, e2)
    | _ => raiseParseError(SExprArityError(ExactlyTwo, context, es))
    }
  }
  let as_three = (context, es: list<SExpression.annotated<_>>) => {
    switch es {
    | list{e1, e2, e3} => (e1, e2, e3)
    | _ => raiseParseError(SExprArityError(ExactlyThree, context, es))
    }
  }
  let as_one_then_many_then_one = (context, es: list<_>) => {
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
    let {ann, it} = e
    switch it {
    | Exp(it) => {ann, it}
    | _ => raiseParseError(TermKindError(Expression, context, e))
    }
  }

  let expr_of_atom = atom => {
    switch atom {
    | SExpression.Atom.Str(s) => Con(Str(s))
    | Sym("#t") => Con(Lgc(true))
    | Sym("#f") => Con(Lgc(false))
    | Sym(x) =>
      let e = {
        let tryNum = x->Float.fromString->Option.map(n => Con(Num(n)))
        tryNum->Option.getWithDefault(Ref(x))
      }
      e
    }
  }

  let rec letstar = (ann, xes, body) => {
    switch xes {
    | list{} =>
      switch body.it {
      | (list{}, e) => e
      | _ => {ann, it: Let(list{}, body)}
      }
    | list{xe} => {ann, it: Let(list{xe}, body)}
    | list{xe, ...xes} => {
        ann,
        it: Let(
          list{xe},
          makeBlock(
            list{},
            letstar(
              {
                begin: xes
                ->List.head
                ->Option.map(xe => xe.ann.begin)
                ->Option.getWithDefault(body.ann.begin),
                end: body.ann.end,
              },
              xes,
              body,
            ),
          ),
        ),
      }
    }
  }

  let rec parseTerm = (e: SExpression.annotated<SExpression.t>): term<srcrange> => {
    let ann = e.ann
    switch e.it {
    | Sequence(Vector, _b, es) => {
        let es = es->List.map(parseValue)
        {ann, it: Exp(AppPrm(VecNew, es))}
      }
    | Sequence(List, _b, list{{it: Atom(Sym("quote")), ann: _}, ...rest}) => {
        let e = as_one("a quoted value", rest)
        parseValue(e) |> mapAnn(v => Exp(v))
      }
    | Sequence(List, _b, list{{it: Atom(Sym("defvar")), ann: _}, ...rest}) => {
        let (x, e) = as_two("a variable and an expression", rest)
        let x = as_id("a variable name", x)
        let e = as_expr("an expression", parseTerm(e))
        {ann, it: Def(Var(x, e))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("deffun")), ann: _}, ...rest}) => {
        let (head, terms, result) = as_one_then_many_then_one("a function header and a body", rest)
        let (fun, args) = as_one_then_many(
          "function name followed by parameters",
          as_list("function name and parameters", head).it,
        )
        let fun = as_id("a function name", fun)
        let args = List.map(args, as_id("a parameter"))
        let terms = Belt.List.map(terms, parseTerm)
        let result = result |> parseTerm |> as_expr("an expression to be returned")
        {ann, it: Def(Fun(fun, args, makeBlock(terms, result)))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("defgen")), ann: _}, ...rest}) => {
        let (head, terms, result) = as_one_then_many_then_one("a generator header and a body", rest)
        let (fun, args) = as_one_then_many(
          "generator name followed by parameters",
          as_list("generator name and parameters", head).it,
        )
        let fun = as_id("a generator name", fun)
        let args = List.map(args, as_id("a parameter"))
        let terms = Belt.List.map(terms, parseTerm)
        let result = result |> parseTerm |> as_expr("an expression to be returned")
        {ann, it: Def(GFun(fun, args, makeBlock(terms, result)))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("lambda")), ann: _}, ...rest}) => {
        let (args, terms, result) = as_one_then_many_then_one(
          "the function signature followed by the function body",
          rest,
        )
        let args = as_list("function parameters", args).it->List.map(as_id("a parameter"))
        let terms = terms->List.map(parseTerm)
        let result = result |> parseTerm |> as_expr("an expression to be returned")
        {ann, it: Exp(Lam(args, makeBlock(terms, result)))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("generator")), ann: _}, ...rest}) => {
        let (args, terms, result) = as_one_then_many_then_one(
          "the generator signature followed by the function body",
          rest,
        )
        let args = as_list("generator parameters", args).it->List.map(as_id("a parameter"))
        let terms = terms->List.map(parseTerm)
        let result = result |> parseTerm |> as_expr("an expression to be returned")
        {ann, it: Exp(GLam(args, makeBlock(terms, result)))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("yield")), ann: _}, ...rest}) => {
        let e = as_one("an expression", rest)
        let e = as_expr("an expression", parseTerm(e))
        {ann, it: Exp(Yield(e))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("λ")), ann: _}, ...rest}) => {
        let (args, terms, result) = as_one_then_many_then_one(
          "the function signature followed by the function body",
          rest,
        )
        let args = as_list("function parameters", args).it->List.map(as_id("a parameter"))
        let terms = terms->List.map(parseTerm)
        let result = result |> parseTerm |> as_expr("an expression to be returned")
        {ann, it: Exp(Lam(args, makeBlock(terms, result)))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("begin")), ann: _}, ...rest}) => {
        let (terms, result) = as_many_then_one("one or more expressions", rest)
        let terms = terms->List.map(parseTerm)->List.map(as_expr("an expression"))
        let result = result->parseTerm |> as_expr("an expression")
        {ann, it: Exp(Bgn(terms, result))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("set!")), ann: _}, ...rest}) => {
        let (x, e) = as_two("a variable and an expression", rest)
        let x = as_id("a variable to be set", x)
        let e = as_expr("an expression", parseTerm(e))
        {ann, it: Exp(Set(x, e))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("if")), ann: _}, ...rest}) => {
        let (e_cnd, e_thn, e_els) = as_three(
          "three expressions (i.e., a condition, the \"then\" branch, and the \"else\" branch)",
          rest,
        )
        let e_cnd = as_expr("a (conditional) expression", parseTerm(e_cnd))
        let e_thn = as_expr("an expression", parseTerm(e_thn))
        let e_els = as_expr("an expression", parseTerm(e_els))
        {ann, it: Exp(If(e_cnd, e_thn, e_els))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("cond")), ann: _}, ...branches}) => {
        let branches =
          branches
          ->List.map(v => as_list("a `cond` branch", v).it)
          ->List.map(as_one_then_many_then_one("the condition followed by the branch"))
        let rec loop = (parsed, branches) => {
          switch branches {
          | list{} => {ann, it: Exp(Cnd(List.reverse(parsed), None))}
          | list{(
              {it: Atom(Sym("else")), ann: _}: SExpression.annotated<SExpression.t>,
              terms,
              result,
            )} => {
              let terms = terms->List.map(parseTerm)
              let result = result |> parseTerm |> as_expr("an expression")
              {ann, it: Exp(Cnd(List.reverse(parsed), Some(makeBlock(terms, result))))}
            }

          | list{(case, terms, result), ...branches} => {
              let case = case->parseTerm |> as_expr("a (conditional) expression")
              let terms = terms->List.map(parseTerm)
              let result = result |> parseTerm |> as_expr("an expression")
              loop(list{(case, makeBlock(terms, result)), ...parsed}, branches)
            }
          }
        }
        loop(list{}, branches)
      }

    | Sequence(List, _b, list{{it: Atom(Sym("let")), ann: _}, ...rest}) => {
        let (xes, ts, result) = as_one_then_many_then_one("the bindings followed by the body", rest)
        let xes =
          as_list("variable-expression pairs", xes).it
          ->List.map(as_list("a variable and an expression"))
          ->List.map(mapAnn(xe => xe |> as_two("a variable and an expression")))
        let xes = xes->List.map(
          mapAnn(((x, e)) => {
            let x = as_id("a variable to be bound", x)
            let e = parseTerm(e) |> as_expr("an expression")
            (x, e)
          }),
        )
        let ts = ts->List.map(parseTerm)
        let result = parseTerm(result) |> as_expr("an expression to be return")
        {ann, it: Exp(Let(xes, makeBlock(ts, result)))}
      }

    | Sequence(List, _b, list{{it: Atom(Sym("let*")), ann: _}, ...rest}) => {
        let (xes, ts, result) = as_one_then_many_then_one("the bindings followed by the body", rest)
        let xes =
          as_list("variable-expression pairs", xes).it
          ->List.map(as_list("a variable and an expression"))
          ->List.map(mapAnn(as_two("a variable and an expression")))
        let xes = xes->List.map(
          mapAnn(((x, e)) => {
            let x = as_id("a variable to be bound", x)
            let e = parseTerm(e) |> as_expr("an expression")
            (x, e)
          }),
        )
        let ts = ts->List.map(parseTerm)
        let result = parseTerm(result) |> as_expr("an expression to be return")
        letstar(ann, xes, makeBlock(ts, result)) |> mapAnn(v => Exp(v))
      }

    | Sequence(List, _b, list{{it: Atom(Sym("letrec")), ann: _}, ...rest}) => {
        let (xes, ts, result) = as_one_then_many_then_one("the bindings followed by the body", rest)
        let xes =
          as_list("variable-expression pairs", xes).it
          ->List.map(as_list("a variable and an expression"))
          ->List.map(mapAnn(as_two("a variable and an expression")))
        let xes = xes->List.map(
          mapAnn(((x, e)) => {
            let x = as_id("a variable to be bound", x)
            let e = parseTerm(e) |> as_expr("an expression")
            (x, e)
          }),
        )
        let ts = ts->List.map(parseTerm)
        let result = parseTerm(result) |> as_expr("an expression to be return")
        {ann, it: Exp(Letrec(xes, makeBlock(ts, result)))}
      }

    | Atom(atom) => {ann, it: Exp(expr_of_atom(atom))}
    | Sequence(List, _b, list{{it: Atom(Sym("next")), ann: _}, ...es}) => makeAppPrm(ann, Next, es)
    | Sequence(List, _b, list{{it: Atom(Sym("+")), ann: _}, ...es}) =>
      makeAppPrm(ann, Arith(Add), es)
    | Sequence(List, _b, list{{it: Atom(Sym("-")), ann: _}, ...es}) =>
      makeAppPrm(ann, Arith(Sub), es)
    | Sequence(List, _b, list{{it: Atom(Sym("*")), ann: _}, ...es}) =>
      makeAppPrm(ann, Arith(Mul), es)
    | Sequence(List, _b, list{{it: Atom(Sym("/")), ann: _}, ...es}) =>
      makeAppPrm(ann, Arith(Div), es)
    | Sequence(List, _b, list{{it: Atom(Sym("<")), ann: _}, ...es}) => makeAppPrm(ann, Cmp(Lt), es)
    | Sequence(List, _b, list{{it: Atom(Sym("=")), ann: _}, ...es}) => makeAppPrm(ann, Cmp(Eq), es)
    | Sequence(List, _b, list{{it: Atom(Sym(">")), ann: _}, ...es}) => makeAppPrm(ann, Cmp(Gt), es)
    | Sequence(List, _b, list{{it: Atom(Sym("<=")), ann: _}, ...es}) => makeAppPrm(ann, Cmp(Le), es)
    | Sequence(List, _b, list{{it: Atom(Sym(">=")), ann: _}, ...es}) => makeAppPrm(ann, Cmp(Ge), es)
    | Sequence(List, _b, list{{it: Atom(Sym("!=")), ann: _}, ...es}) => makeAppPrm(ann, Cmp(Ne), es)
    | Sequence(List, _b, list{{it: Atom(Sym("pair")), ann: _}, ...es}) =>
      makeAppPrm(ann, PairNew, es)
    | Sequence(List, _b, list{{it: Atom(Sym("mpair")), ann: _}, ...es}) =>
      makeAppPrm(ann, PairNew, es)
    | Sequence(List, _b, list{{it: Atom(Sym("left")), ann: _}, ...es}) =>
      makeAppPrm(ann, PairRefLeft, es)
    | Sequence(List, _b, list{{it: Atom(Sym("right")), ann: _}, ...es}) =>
      makeAppPrm(ann, PairRefRight, es)
    | Sequence(List, _b, list{{it: Atom(Sym("set-left!")), ann: _}, ...es}) =>
      makeAppPrm(ann, PairSetLeft, es)
    | Sequence(List, _b, list{{it: Atom(Sym("set-right!")), ann: _}, ...es}) =>
      makeAppPrm(ann, PairSetRight, es)
    | Sequence(List, _b, list{{it: Atom(Sym("mvec")), ann: _}, ...es}) =>
      makeAppPrm(ann, VecNew, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vec-ref")), ann: _}, ...es}) =>
      makeAppPrm(ann, VecRef, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vref")), ann: _}, ...es}) =>
      makeAppPrm(ann, VecRef, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vec-set!")), ann: _}, ...es}) =>
      makeAppPrm(ann, VecSet, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vset!")), ann: _}, ...es}) =>
      makeAppPrm(ann, VecSet, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vec-len")), ann: _}, ...es}) =>
      makeAppPrm(ann, VecLen, es)
    | Sequence(List, _b, list{{it: Atom(Sym("vlen")), ann: _}, ...es}) =>
      makeAppPrm(ann, VecLen, es)
    | Sequence(List, _b, list{{it: Atom(Sym("eq?")), ann: _}, ...es}) =>
      makeAppPrm(ann, Cmp(Eq), es)
    | Sequence(List, _b, list{{it: Atom(Sym("error")), ann: _}, ...es}) => makeAppPrm(ann, Err, es)
    | Sequence(List, _b, list{{it: Atom(Sym("not")), ann: _}, ...es}) => makeAppPrm(ann, Not, es)
    | Sequence(List, _b, list{{it: Atom(Sym("print")), ann: _}, ...es}) =>
      makeAppPrm(ann, Print, es)
    | Sequence(List, _b, es) => {
        let (e, es) = as_one_then_many(
          "a function call/application, which includes a function and then zero or more arguments",
          es,
        )
        let e = e->parseTerm |> as_expr("a function")
        let es = es->List.map(parseTerm)->List.map(as_expr("an argument"))
        {ann, it: Exp(App(e, es))}
      }
    }
  }
  and makeAppPrm = (ann, p, es) => {
    let es = es->List.map(parseTerm)->List.map(as_expr("an argument"))
    {ann, it: Exp(AppPrm(p, es))}
  }

  let parseTerms = (src: string) => {
    switch src->SExpression.fromString->List.map(parseTerm) {
    | terms => terms
    | exception SExpression.SExpressionError(err) =>
      raiseParseError(SExprParseError(SExpression.Error.toString(err)))
    }
  }

  let parseProgram = src => {
    let it = parseTerms(src)
    let begin = it->List.head->Option.map(v => v.ann.begin)->Option.getWithDefault({ln: 0, ch: 0})
    let end =
      it->List.reverse->List.head->Option.map(v => v.ann.end)->Option.getWithDefault({ln: 0, ch: 0})
    {
      it,
      ann: {begin, end},
    }
  }

  let parseOutput = src => {
    switch src->SExpression.fromString->List.map(outputletOfSExpr) {
    | terms => terms
    | exception SExpression.SExpressionError(err) =>
      raiseParseError(SExprParseError(SExpression.Error.toString(err)))
    }
  }
}

type exn += SMoLPrintError(string)
let raisePrintError = err => raise(SMoLPrintError(err))

type printAnn = {srcrange: srcrange, print: Print.t}
module type Printer = {
  let printOutput: output => string
  let printProgram: (bool, program<srcrange>) => string
  let printProgramFull: (bool, program<srcrange>) => program<printAnn>
}

let getPrint = ({ann: {print, srcrange}}) => {
  {it: print, ann: Some(srcrange)}
}

let indent = (t: annotated<Print.t, option<srcrange>>, i): annotated<Print.t, option<srcrange>> => {
  let pad = Js.String.repeat(i, " ")
  t |> mapAnn(Print.map(s => Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s)))
}
let indentBlock = (s, i) => indent(group(list{plain("\n"), s}), i)
let hcat = (s1, s2) => {
  Group(list{s1, indent(s2, String.length(s1.it |> Print.toString))})
}

module SMoLPrinter = {
  let constantToString = c => {
    switch c {
    | Uni => "#<void>"
    | Nil => "#<empty>"
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
    Group(list{plain("("), dummy(printConcat(" ", ss)), plain(")")})
    // "(" ++ String.concat(" ", ss) ++ ")"
  }

  let defvarLike = (op, x: annotated<_, _>, e: annotated<_, _>) => {
    if containsNL(e.it) {
      Group(list{plain("("), plain(op), plain(" "), x, indentBlock(e, 2), plain(")")})
    } else {
      listToString(list{plain(op), x, e})
    }
  }

  let defvarToString = (x, e) => {
    defvarLike("defvar", x, e)
  }

  let deffunToString = (f, xs, b) => {
    defvarLike("deffun", dummy(listToString(list{f, ...xs})), b)
  }

  let defgenToString = (f, xs, b) => {
    defvarLike("defgen", dummy(listToString(list{f, ...xs})), b)
  }

  let exprSetToString = (x, e) => {
    defvarLike("set!", x, e)
  }

  let exprLamToString = (xs, b) => {
    defvarLike("lambda", dummy(listToString(xs)), b)
  }
  let exprGenToString = (xs, b) => {
    defvarLike("generator", dummy(listToString(xs)), b)
  }
  let exprYieldToString = e => Group({list{plain("(yield "), e, plain(")")}})

  let exprAppToString = (e, es) => {
    listToString(list{e, ...es})
  }

  let beginLike = (op, ts) => {
    Group(list{plain(op), indentBlock(printConcat("\n", ts) |> dummy, 2)})
    // `(${op}${)`
  }
  let exprBgnToString = (es, e) => {
    beginLike("begin", list{...es, e})
  }

  let exprCndToString = (ebs: list<(annotated<_, _>, annotated<_, _>)>, ob) => {
    let ebs = {
      switch ob {
      | None => ebs
      | Some(b) => list{...ebs, (plain("else"), b)}
      }
    }
    let ebs = ebs->List.map(((e, b)) => group(list{plain("["), e, indentBlock(b, 1), plain("]")}))
    beginLike("cond", ebs)
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    hcat(plain(`(if `), group2(printConcat("\n", list{e_cnd, e_thn, e_els}) |> dummy, plain(")")))
    // hcat(plain(`(if `), `${String.concat("\n", list{e_cnd, e_thn, e_els})})`)
  }

  let letLike = (op: string, xes: list<_>, b: _) => {
    let xes = printConcat("\n", xes) |> dummy
    let xes = group(list{plain("("), indent(xes, 1), plain(")")})
    Group(list{
      hcat(group(list{plain("("), plain(op), plain(" ")}), xes) |> dummy,
      indentBlock(b, 2),
      plain(")"),
    })
    // hcat(`(${op} `, `${xes}`) ++ `${indentBlock(b, 2)})`
  }
  let exprLetToString = (xes, b) => {
    letLike("let", xes, b)
  }
  let exprLetrecToString = (xes, b) => {
    letLike("letrec", xes, b)
  }

  let symbolToString = ({it, ann}) => {
    {
      it,
      ann: {
        srcrange: ann,
        print: Plain(it),
      },
    }
  }

  let rec printExp = ({it, ann: srcrange}: expression<srcrange>): expression<printAnn> => {
    let e: annotated<expressionNode<printAnn>, Print.t> = switch it {
    | Con(c) => {
        it: Con(c),
        ann: Plain(constantToString(c)),
      }
    | Ref(x) => {
        it: Ref(x),
        ann: Plain(x),
      }
    | Set(x, e) => {
        let x = symbolToString(x)
        let e = e->printExp
        {
          ann: exprSetToString(getPrint(x), getPrint(e)),
          it: Set(x, e),
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: exprLamToString(xs->List.map(x => getPrint(x)), getPrint(b)),
          it: Lam(xs, b),
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: exprGenToString(xs->List.map(x => getPrint(x)), getPrint(b)),
          it: Lam(xs, b),
        }
      }
    | Yield(e) => {
        let e = e->printExp
        {
          ann: exprYieldToString(getPrint(e)),
          it: Yield(e),
        }
      }
    | AppPrm(p, es) => {
        let es = es->List.map(printExp)
        {
          ann: exprAppToString(plain(Primitive.toString(p)), es->List.map(e => getPrint(e))),
          it: AppPrm(p, es),
        }
      }
    | App(e, es) => {
        let e = e->printExp
        let es = es->List.map(printExp)
        {
          ann: exprAppToString(getPrint(e), es->List.map(e => getPrint(e))),
          it: App(e, es),
        }
      }
    | Let(xes, b) => {
        let xes = xes->List.map(xeToString)
        let b = b->printBlock
        {
          ann: exprLetToString(xes->List.map(xe => getPrint(xe)), getPrint(b)),
          it: Let(xes, b),
        }
      }
    | Letrec(xes, b) => {
        let xes = xes->List.map(xeToString)
        let b = b->printBlock
        {
          ann: exprLetrecToString(xes->List.map(xe => getPrint(xe)), getPrint(b)),
          it: Letrec(xes, b),
        }
      }
    | Cnd(ebs, ob) => {
        let ebs = ebs->List.map(ebToString)
        let ob = ob->obToString
        {
          ann: exprCndToString(
            ebs->List.map(((e, b)) => (getPrint(e), getPrint(b))),
            ob->Option.map(b => getPrint(b)),
          ),
          it: Cnd(ebs, ob),
        }
      }
    | If(e_cnd, e_thn, e_els) => {
        let e_cnd = e_cnd->printExp
        let e_thn = e_thn->printExp
        let e_els = e_els->printExp
        {
          ann: exprIfToString(getPrint(e_cnd), getPrint(e_thn), getPrint(e_els)),
          it: If(e_cnd, e_thn, e_els),
        }
      }
    | Bgn(es, e) => {
        let es = es->List.map(printExp)
        let e = e->printExp
        {
          ann: exprBgnToString(es->List.map(e => getPrint(e)), getPrint(e)),
          it: Bgn(es, e),
        }
      }
    }
    let {ann: print, it} = e
    {ann: {print, srcrange}, it}
  }
  and printDef = ({ann: srcrange, it: d}: definition<srcrange>): definition<printAnn> => {
    let d = switch d {
    | Var(x, e) => {
        let x = symbolToString(x)
        let e = e->printExp
        {
          ann: defvarToString(getPrint(x), getPrint(e)),
          it: Var(x, e),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: deffunToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: defgenToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
          it: GFun(f, xs, b),
        }
      }
    }
    let {ann: print, it} = d
    {ann: {print, srcrange}, it}
  }
  and xeToString = ({it: xe, ann: srcrange}: bind<srcrange>): bind<printAnn> => {
    let (x, e) = xe
    let (x, e) = (symbolToString(x), printExp(e))
    let print = hcat(group2(plain("["), getPrint(x)), group2(getPrint(e), plain("]")))
    {
      it: (x, e),
      ann: {
        print,
        srcrange,
      },
    }
  }
  and ebToString = eb => {
    let (e, b) = eb
    (printExp(e), printBlock(b))
  }
  and obToString = ob => {
    ob->Option.map(printBlock)
  }
  and printBlock = ({ann: srcrange, it: b}) => {
    let (ts, e) = b
    let ts: list<term<printAnn>> = ts->List.map(printTerm)
    let e = e->printExp
    let print = printConcat("\n", list{...ts->List.map(getPrint), getPrint(e)})
    {
      ann: {print, srcrange},
      it: (ts, e),
    }
  }
  and printTerm = ({ann: srcrange, it: t}: term<srcrange>): term<printAnn> => {
    switch t {
    | Exp(it) => printExp({ann: srcrange, it}) |> mapAnn(v => Exp(v))
    | Def(it) => printDef({ann: srcrange, it}) |> mapAnn(v => Def(v))
    }
  }

  let printOutput = (os): string => {
    let rec p = (v: val): string => {
      switch v {
      | Con(c) => constantToString(c)
      | Lst(es) => `(${String.concat(" ", es->List.map(p))})`
      | Vec(es) => `#(${String.concat(" ", es->List.map(p))})`
      }
    }
    os->List.map(o => {
      switch o {
      | OErr => "error"
      | OVal(v) => p(v)
      }
    }) |> String.concat(" ")
  }

  let printProgramFull = (_insertPrintTopLevel, {ann: srcrange, it: ts}) => {
    let ts = ts->List.map(printTerm)
    let print = printConcat("\n", ts->List.map(getPrint))
    {
      ann: {print, srcrange},
      it: ts,
    }
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print->Print.toString
  }
}

type statContext =
  | Step
  | Return
  | TopLevel

type context =
  | Expr(bool) // the bool indicates whether we are in an infix operation
  | Stat(statContext)

let op1 = (s1, p1, s2) => group(list{plain(s1), p1, plain(s2)})
let op2 = (s1, p1, s2, p2, s3) => group(list{plain(s1), p1, plain(s2), p2, plain(s3)})
let op3 = (s1, p1, s2, p2, s3, p3, s4) =>
  group(list{plain(s1), p1, plain(s2), p2, plain(s3), p3, plain(s4)})

module JSPrinter = {
  let escapeName = x => {
    let re = %re("/-./g")
    let matchFn = (matchPart, _offset, _wholeString) => {
      Js.String2.toUpperCase(Js.String2.substringToEnd(matchPart, ~from=1))
    }
    let x = Js.String2.unsafeReplaceBy0(x, re, matchFn)

    // add `$` to the beginning of reserved words
    if x == "var" {
      "$var"
    } else {
      x
    }
  }

  let constantToString = c => {
    switch c {
    | Uni => "null"
    | Nil => raisePrintError("Lists are not supported in JavaScript")
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

  let listToString = es => {
    if es->List.some(e => containsNL(e.it)) {
      Group(list{plain("("), indentBlock(dummy(printConcat(",\n", es)), 2), plain(")")})
    } else {
      Group(list{plain("("), dummy(printConcat(", ", es)), plain(")")})
    }
  }

  let defvarLike = (op, x, e) => {
    group(list{plain(op), x, plain(" = "), indent(e, 2)})
  }

  let exprAppToString = (e, es) => {
    group2(e, listToString(es) |> dummy)
  }

  let printingTopLevel = ref(false)

  let consumeContext = (e: annotated<_, _>, context) => {
    switch context {
    | Expr(_) => surround("", e, "")
    | Stat(ctx) =>
      switch ctx {
      | Step => surround("", e, ";")
      | Return => surround("return ", e, ";") //`return ${e};`
      | TopLevel =>
        if printingTopLevel.contents {
          surround("console.log(", e, ");")
        } else {
          surround("", e, ";")
        }
      }
    }
  }

  let consumeContextWrap = (e: annotated<_, _>, context: context) => {
    switch context {
    | Expr(true) => surround("(", e, ")")
    | _ => consumeContext(e, context)
    }
  }

  let consumeContextVoid = (e: annotated<_, _>, context) => {
    switch context {
    | Stat(Return) => surround("", e, ";\nreturn;")
    | Stat(TopLevel) => surround("", e, ";")
    | _ => consumeContext(e, context)
    }
  }

  let consumeContextStat = (e: annotated<_, _>, context: context) => {
    switch context {
    | Expr(_) =>
      raisePrintError(`${e.it |> Print.toString} can't be used as a expression in JavaScript`)
    | _ => consumeContextVoid(e, context)
    }
  }

  let exprAppPrmToString = (
    p: Primitive.t,
    es: list<bool => expression<printAnn>>,
    context: context,
  ): annotated<_, Print.t> => {
    switch (p, es) {
    | (Arith(o), es) => {
        let os = switch o {
        | Add => "+"
        | Sub => "-"
        | Mul => "*"
        | Div => "/"
        }
        let es = es->List.map(e => e(true))
        {
          ann: printConcat(` ${os} `, es->List.map(e => getPrint(e)))
          ->dummy
          ->consumeContextWrap(context),
          it: (Arith(o), es),
        }
      }
    | (Cmp(o), list{e1, e2}) => {
        let os = switch o {
        | Lt => "<"
        | Eq => "=="
        | Gt => ">"
        | Le => "<="
        | Ge => ">="
        | Ne => "!="
        }
        let e1 = e1(true)
        let e2 = e2(true)
        {
          ann: op2("", getPrint(e1), os, getPrint(e2), "")->consumeContext(context),
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("[ ", getPrint(e1), ", ", getPrint(e2), " ]")->consumeContext(context),
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("", getPrint(e1), "[0]")->consumeContext(context),
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("", getPrint(e1), "[1]")->consumeContext(context),
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[0] = ", getPrint(e2), "")->consumeContextStat(context),
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[1] = ", getPrint(e2), "")->consumeContextStat(context),
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          ann: op1(
            "[ ",
            printConcat(`, `, es->List.map(e => getPrint(e))) |> dummy,
            " ]",
          )->consumeContext(context),
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[", getPrint(e2), "]")->consumeContext(context),
          it: (VecRef, list{e1, e2}),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          // ann: `${getPrint(e1)}[${getPrint(e2)}] = ${getPrint(e3)}`->consumeContextStat(context),
          ann: op3(
            "",
            getPrint(e1),
            "[",
            getPrint(e2),
            "] = ",
            getPrint(e3),
            "",
          )->consumeContextStat(context),
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("", getPrint(e1), ".length")->consumeContext(context),
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("throw ", getPrint(e1), "")->consumeContextWrap(context),
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("! ", getPrint(e1), "")->consumeContextWrap(context),
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("console.log(", getPrint(e1), ")")->consumeContextVoid(context),
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("", getPrint(e1), ".next()")->consumeContextVoid(context),
          it: (Next, list{e1}),
        }
      }
    | (Cons, _) => raisePrintError("List is not supported by JavaScript")
    | _ =>
      raisePrintError(
        `JavaScript doesn't let you use ${Primitive.toString(p)} on ${List.length(
            es,
          ) |> Int.toString} parameter(s).`,
      )
    }
  }

  let funLike = (op, x, xs, e) => {
    op2(`${op} `, exprAppToString(x, xs), " {", indentBlock(e, 2), "\n}")
    // `${op} ${exprAppToString(x, xs)} {${}\n}`
  }

  let defvarToString = (x, e) => {
    op1("", defvarLike("let ", x, e), ";")
  }

  let deffunToString = (f, xs, b) => {
    funLike("function", f, xs, b)
  }

  let defgenToString = (f, xs, b) => {
    funLike("function*", f, xs, b)
  }

  let exprSetToString = (x, e) => {
    defvarLike("", x, e)
  }

  let exprLamToString = (xs, b) => {
    funLike("function", plain(""), xs, b)
  }
  let exprGenToString = (xs, b) => {
    funLike("function*", plain(""), xs, b)
  }
  let exprYieldToString = e => op1("yield ", e, "")

  let exprBgnToString = (es, e) => {
    listToString(list{...es, e}) |> dummy
    // `{${indentBlock(String.concat(", ", es), 2)}\n}`
    // beginLike("begin", list{...es, e})
  }

  let exprCndToString = (ebs: list<(_, _)>, ob) => {
    let ebs = {
      switch ob {
      | None => ebs
      | Some(b) => list{...ebs, (plain(""), b)}
      }
    }
    let ebs = ebs->List.map(((e, b)) => op2("if (", e, ") {", indentBlock(b, 2), "\n}"))
    printConcat(" else ", ebs)
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    op3("", e_cnd, " ? ", e_thn, " : ", e_els, "")
    // `${e_cnd} ? ${e_thn} : ${e_els}`
  }

  let symbolToString = ({it, ann}) => {
    {
      it,
      ann: {
        srcrange: ann,
        print: Plain(escapeName(it)),
      },
    }
  }

  let rec printExp = ({it, ann: srcrange}, context) => {
    let e: annotated<expressionNode<printAnn>, Print.t> = switch it {
    | Con(c) => {
        it: Con(c),
        ann: plain(constantToString(c))->consumeContext(context),
      }
    | Ref(x) => {
        it: Ref(x),
        ann: plain(x->escapeName)->consumeContext(context),
      }
    | Set(x, e) => {
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp(Expr(false))
        {
          ann: exprSetToString(getPrint(x), getPrint(e))->consumeContextStat(context),
          it: Set(x, e),
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprLamToString(xs->List.map(x => getPrint(x)), getPrint(b))->consumeContextWrap(
            context,
          ),
          it: Lam(xs, b),
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprGenToString(xs->List.map(x => getPrint(x)), getPrint(b))->consumeContextWrap(
            context,
          ),
          it: Lam(xs, b),
        }
      }
    | Yield(e) => {
        let e = e->printExp(Expr(false))
        {
          ann: exprYieldToString(getPrint(e))->consumeContextWrap(context),
          it: Yield(e),
        }
      }
    | AppPrm(p, es) => {
        let es = es->List.map((e, b) => e->printExp(Expr(b)))
        let {ann: print, it: (p, es)} = exprAppPrmToString(p, es, context)
        {
          it: AppPrm(p, es),
          ann: print,
        }
      }
    | App(e, es) => {
        let e = e->printExp(Expr(false))
        let es = es->List.map(e => e->printExp(Expr(false)))
        {
          ann: exprAppToString(getPrint(e), es->List.map(e => getPrint(e)))->consumeContext(
            context,
          ),
          it: App(e, es),
        }
      }
    | Let(_xes, _b) => raisePrintError("let-expressions are not supported by JavaScript")
    | Letrec(xes, b) =>
      switch context {
      | Expr(_) => raisePrintError("letrec-expressions are not supported by JavaScript")
      | Stat(ctx) => {
          let xes = xes->List.map(xeToString)
          let b = b->printBlock(ctx)
          {
            ann: Group(list{
              plain("{\n"),
              indentBlock(
                printConcat("\n", list{...xes->List.map(xe => getPrint(xe)), getPrint(b)}) |> dummy,
                2,
              ),
              plain("\n}"),
            }),
            it: Letrec(xes, b),
          }
        }
      }
    | Cnd(ebs, ob) =>
      switch context {
      | Expr(_) =>
        raisePrintError(
          "Multi-armed conditionals in JavaScript is not supported by the translator yet.",
        )
      | Stat(context) => {
          let ebs: list<(expression<printAnn>, block<printAnn>)> =
            ebs->List.map(eb => eb->ebToString(context))
          let ob = ob->obToString(context)
          {
            ann: exprCndToString(
              ebs->List.map(((e, b)) => (getPrint(e), getPrint(b))),
              ob->Option.map(b => getPrint(b)),
            ),
            it: Cnd(ebs, ob),
          }
        }
      }
    | If(e_cnd, e_thn, e_els) => {
        let e_cnd = e_cnd->printExp(Expr(true))
        let e_thn = e_thn->printExp(Expr(true))
        let e_els = e_els->printExp(Expr(true))
        {
          ann: exprIfToString(
            getPrint(e_cnd),
            getPrint(e_thn),
            getPrint(e_els),
          )->consumeContextWrap(context),
          it: If(e_cnd, e_thn, e_els),
        }
      }
    | Bgn(es, e) => {
        let es = es->List.map(e => e->printExp(Expr(false)))
        let e = e->printExp(Expr(false))
        {
          ann: exprBgnToString(es->List.map(e => getPrint(e)), getPrint(e))->consumeContext(
            context,
          ),
          it: Bgn(es, e),
        }
      }
    }
    let {ann: print, it} = e
    {ann: {print, srcrange}, it}
  }
  and defToString = ({ann: srcrange, it: d}: definition<srcrange>): definition<printAnn> => {
    let d = switch d {
    | Var(x, e) => {
        let x = x->symbolToString
        let e = e->printExp(Expr(false))
        {
          ann: defvarToString(getPrint(x), getPrint(e)),
          it: Var(x, e),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: deffunToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: defgenToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
          it: GFun(f, xs, b),
        }
      }
    }
    let {ann: print, it} = d
    {ann: {print: print.it, srcrange}, it}
  }
  and xeToString = ({it: xe, ann: srcrange}: bind<srcrange>): bind<printAnn> => {
    let (x, e) = xe
    let (x, e) = (symbolToString(x), e->printExp(Expr(false)))
    let print = defvarToString(getPrint(x), getPrint(e)).it
    {
      it: (x, e),
      ann: {
        print,
        srcrange,
      },
    }
  }
  and ebToString = (eb, ctx: statContext) => {
    let (e, b) = eb
    (e->printExp(Expr(false)), b->printBlock(ctx))
  }
  and obToString = (ob, ctx: statContext) => {
    ob->Option.map(b => b->printBlock(ctx))
  }
  and printBlock = ({ann: srcrange, it: b}, context: statContext) => {
    let (ts, e) = b
    let ts = ts->List.map(t => t->printTerm(Step))
    let e = e->printExp(Stat(context))
    let print = printConcat("\n", list{...ts->List.map(getPrint), getPrint(e)})
    {
      ann: {print, srcrange},
      it: (ts, e),
    }
  }
  and printTerm = ({ann: srcrange, it: t}: term<srcrange>, ctx): term<printAnn> => {
    switch t {
    | Exp(it) => printExp({ann: srcrange, it}, Stat(ctx)) |> mapAnn(v => Exp(v))
    | Def(it) => defToString({ann: srcrange, it}) |> mapAnn(v => Def(v))
    }
  }

  let printOutput = (os): string => {
    let rec p = (v: val): string => {
      switch v {
      | Con(c) => constantToString(c)
      | Lst(_es) => raisePrintError("Lists are not supported in JavaScript")
      | Vec(es) => `[ ${String.concat(", ", es->List.map(p))} ]`
      }
    }
    os->List.map(o => {
      switch o {
      | OErr => "error"
      | OVal(v) => p(v)
      }
    }) |> String.concat(" ")
  }

  let printProgramFull = (insertPrintTopLevel, {ann: srcrange, it: ts}) => {
    printingTopLevel := insertPrintTopLevel
    let ts = ts->List.map(t => t->printTerm(TopLevel))
    let print = printConcat("\n", ts->List.map(getPrint))
    {
      ann: {print, srcrange},
      it: ts,
    }
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print |> Print.toString
  }
}

module PYPrinter = {
  // Python translation is tricky because we need to know whether a variable
  // reference is pointing to non-local variable.
  // - If the variable is local, we proceed normally.
  // - If the variable is external but neither global nor built-in, we need to declare it nonlocal in the local scope
  // - If the variable is global, we need to declare it global
  // - If the variable is built-in, we proceed normally

  module RefDecl = {
    type t =
      | Nonlocal
      | Global
    let toString = x => {
      switch x {
      | Nonlocal => "nonlocal"
      | Global => "global"
      }
    }
  }
  open RefDecl

  type stringSet = HashSet.String.t
  type stringMap = HashMap.String.t<RefDecl.t>

  type rec env =
    | G(stringSet)
    | E(stringSet, env, stringMap)

  let refMut = (env: env, x: string) => {
    switch env {
    | G(_ss) => ()
    | E(ss, env, decl) => {
        let rec k = (env, x) => {
          switch env {
          | G(ss) =>
            if HashSet.String.has(ss, x) {
              HashMap.String.set(decl, x, Global)
            }
          | E(ss, env, _decl) =>
            if HashSet.String.has(ss, x) {
              HashMap.String.set(decl, x, Nonlocal)
            } else {
              k(env, x)
            }
          }
        }

        if !HashSet.String.has(ss, x) {
          k(env, x)
        }
      }
    }
  }

  let extend = (ss, env) => {
    let refs = HashMap.String.make(~hintSize=0)
    (refs, E(ss->List.toArray->HashSet.String.fromArray, env, refs))
  }

  let xsOfTerm = t => {
    switch t.it {
    | Exp(_) => list{}
    | Def(Var(x, _)) => list{x.it}
    | Def(Fun(f, _xs, _b)) => list{f.it}
    | Def(GFun(f, _xs, _b)) => list{f.it}
    }
  }

  let escapeName = x => {
    let re = %re("/-/g")
    let matchFn = (_matchPart, _offset, _wholeString) => {
      "_"
    }
    Js.String2.unsafeReplaceBy0(x, re, matchFn)
  }

  let constantToString = c => {
    switch c {
    | Uni => "None"
    | Nil => raisePrintError("Lists are not supported in Python")
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

  let listToString = es => {
    if es->List.some(e => containsNL(e.it)) {
      Group(list{plain("("), indentBlock(dummy(printConcat(",\n", es)), 4), plain(")")})
    } else {
      Group(list{plain("("), dummy(printConcat(", ", es)), plain(")")})
    }
  }

  let defvarLike = (op, x, e) => {
    group(list{plain(op), x, plain(" = "), indent(e, 2)})
  }

  let exprAppToString = (e, es) => {
    group2(e, listToString(es) |> dummy)
  }

  let printingTopLevel = ref(false)

  let consumeContext = (e: annotated<_, _>, context) => {
    switch context {
    | Expr(_) => surround("", e, "")
    | Stat(ctx) =>
      switch ctx {
      | Step => surround("", e, "")
      | Return => surround("return ", e, "") //`return ${e};`
      | TopLevel =>
        if printingTopLevel.contents {
          surround("print(", e, ")")
        } else {
          surround("", e, "")
        }
      }
    }
  }

  let consumeContextWrap = (e: annotated<_, _>, context: context) => {
    switch context {
    | Expr(true) => surround("(", e, ")")
    | _ => consumeContext(e, context)
    }
  }

  let consumeContextVoid = (e: annotated<_, _>, context) => {
    switch context {
    | Stat(Return) => surround("", e, "\nreturn")
    | Stat(TopLevel) => surround("", e, "")
    | _ => consumeContext(e, context)
    }
  }

  let consumeContextStat = (e: annotated<_, _>, context: context) => {
    switch context {
    | Expr(_) =>
      raisePrintError(`${e.it |> Print.toString} can't be used as a expression in Python`)
    | _ => consumeContextVoid(e, context)
    }
  }

  let exprAppPrmToString = (
    p: Primitive.t,
    es: list<bool => expression<printAnn>>,
    context: context,
  ): annotated<_, Print.t> => {
    switch (p, es) {
    | (Arith(o), es) => {
        let os = switch o {
        | Add => "+"
        | Sub => "-"
        | Mul => "*"
        | Div => "/"
        }
        let es = es->List.map(e => e(true))
        {
          ann: printConcat(` ${os} `, es->List.map(e => getPrint(e)))
          ->dummy
          ->consumeContextWrap(context),
          it: (Arith(o), es),
        }
      }
    | (Cmp(o), list{e1, e2}) => {
        let os = switch o {
        | Lt => "<"
        | Eq => "=="
        | Gt => ">"
        | Le => "<="
        | Ge => ">="
        | Ne => "!="
        }
        let e1 = e1(true)
        let e2 = e2(true)
        {
          ann: op2("", getPrint(e1), os, getPrint(e2), "")->consumeContext(context),
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("[", getPrint(e1), ", ", getPrint(e2), "]")->consumeContext(context),
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("", getPrint(e1), "[0]")->consumeContext(context),
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("", getPrint(e1), "[1]")->consumeContext(context),
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[0] = ", getPrint(e2), "")->consumeContextStat(context),
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[1] = ", getPrint(e2), "")->consumeContextStat(context),
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          ann: op1(
            "[",
            printConcat(`, `, es->List.map(e => getPrint(e))) |> dummy,
            "]",
          )->consumeContext(context),
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[", getPrint(e2), "]")->consumeContext(context),
          it: (VecRef, list{e1, e2}),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          ann: op3(
            "",
            getPrint(e1),
            "[",
            getPrint(e2),
            "] = ",
            getPrint(e3),
            "",
          )->consumeContextStat(context),
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("len(", getPrint(e1), ")")->consumeContext(context),
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("raise ", getPrint(e1), "")->consumeContextWrap(context),
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("not ", getPrint(e1), "")->consumeContextWrap(context),
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("print(", getPrint(e1), ")")->consumeContextVoid(context),
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("next(", getPrint(e1), ")")->consumeContextVoid(context),
          it: (Next, list{e1}),
        }
      }
    | (Cons, _) => raisePrintError("List is not supported by Python")
    | _ =>
      raisePrintError(
        `Python doesn't let you use ${Primitive.toString(p)} on ${List.length(
            es,
          ) |> Int.toString} parameter(s).`,
      )
    }
  }

  let funLike = (op, x, xs, e) => {
    op2(`${op} `, exprAppToString(x, xs), ":", indentBlock(e, 4), "")
    // `${op} ${exprAppToString(x, xs)} {${}\n}`
  }

  let defvarToString = (x, e) => {
    op1("", defvarLike("", x, e), "")
  }

  let deffunToString = (f, xs, b) => {
    funLike("def", f, xs, b)
  }

  let defgenToString = (f, xs, b) => {
    funLike("def", f, xs, b)
  }

  let exprSetToString = (x, e) => {
    defvarLike("", x, e)
  }

  let exprLamToString = (xs, b) => {
    op2("lambda ", printConcat(",", xs) |> dummy, ": ", b, "")
    // `lambda ${xs |> String.concat(",")}: ${b}`
  }
  let exprYieldToString = e => op1(`yield `, e, "")

  let exprBgnToString = (es, e) => {
    op1("", listToString(list{...es, e}) |> dummy, "[-1]")
  }

  let exprCndToString = (ebs: list<(_, _)>, ob) => {
    let ebs = {
      switch ob {
      | None => ebs
      | Some(b) => list{...ebs, (plain("se:"), b)}
      }
    }
    let ebs = ebs->List.map(((e, b)) => op2("if ", e, ":", indentBlock(b, 4), "\n"))
    printConcat(" el", ebs)
  }

  let exprIfToString = (e_cnd: _, e_thn: _, e_els: _) => {
    op3("", e_thn, " if ", e_cnd, " else ", e_els, "")
    // `${e_thn} if ${e_cnd} else ${e_els}`
  }

  let symbolToString = ({it, ann}) => {
    {
      it,
      ann: {
        srcrange: ann,
        print: Plain(escapeName(it)),
      },
    }
  }

  let rec printExp = ({it, ann: srcrange}, context, env) => {
    let e: annotated<expressionNode<printAnn>, Print.t> = switch it {
    | Con(c) => {
        it: Con(c),
        ann: plain(constantToString(c))->consumeContext(context),
      }
    | Ref(x) => {
        it: Ref(x),
        ann: plain(x->escapeName)->consumeContext(context),
      }
    | Set(x, e) => {
        refMut(env, x.it)
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp(Expr(false), env)
        {
          ann: exprSetToString(getPrint(x), getPrint(e))->consumeContextStat(context),
          it: Set(x, e),
        }
      }
    | Lam(xs, b) =>
      switch b.it {
      | (list{}, e) => {
          let xs = xs->List.map(symbolToString)
          let (refs, env) = extend(xs->List.map(x => x.it), env)
          let e = e->printExp(Expr(false), env)
          if HashMap.String.isEmpty(refs) {
            {
              ann: exprLamToString(xs->List.map(x => getPrint(x)), getPrint(e))->consumeContextWrap(
                context,
              ),
              it: Lam(
                xs,
                {
                  ann: {print: getPrint(e).it, srcrange: b.ann},
                  it: (list{}, e),
                },
              ),
            }
          } else {
            raisePrintError("Can't mutate variable inside Python lambda")
          }
        }
      | _ => raisePrintError("In Python, a lambda body must contains exactly one expression")
      }
    | GLam(_xs, _b) => raisePrintError("In Python, lambdas can't be generators.")
    | Yield(e) => {
        let e = e->printExp(Expr(false), env)
        {
          ann: exprYieldToString(getPrint(e))->consumeContextWrap(context),
          it: Yield(e),
        }
      }
    | AppPrm(p, es) => {
        let es = es->List.map((e, b) => e->printExp(Expr(b), env))
        let {ann: print, it: (p, es)} = exprAppPrmToString(p, es, context)
        {
          it: AppPrm(p, es),
          ann: print,
        }
      }
    | App(e, es) => {
        let e = e->printExp(Expr(false), env)
        let es = es->List.map(e => e->printExp(Expr(false), env))
        {
          ann: exprAppToString(getPrint(e), es->List.map(e => getPrint(e)))->consumeContext(
            context,
          ),
          it: App(e, es),
        }
      }
    | Let(_xes, _b) => raisePrintError("let-expressions are not supported by Python")
    | Letrec(_xes, _b) => raisePrintError("letrec-expressions are not supported by Python")
    | Cnd(ebs, ob) =>
      switch context {
      | Expr(_) =>
        raisePrintError(
          "Multi-armed conditionals in JavaScript is not supported by the translator yet.",
        )
      | Stat(context) => {
          let ebs: list<(expression<printAnn>, block<printAnn>)> =
            ebs->List.map(eb => eb->ebToString(context, env))
          let ob = ob->obToString(context, env)
          {
            ann: exprCndToString(
              ebs->List.map(((e, b)) => (getPrint(e), getPrint(b))),
              ob->Option.map(b => getPrint(b)),
            ),
            it: Cnd(ebs, ob),
          }
        }
      }
    | If(e_cnd, e_thn, e_els) => {
        let e_cnd = e_cnd->printExp(Expr(true), env)
        let e_thn = e_thn->printExp(Expr(true), env)
        let e_els = e_els->printExp(Expr(true), env)
        {
          ann: exprIfToString(
            getPrint(e_cnd),
            getPrint(e_thn),
            getPrint(e_els),
          )->consumeContextWrap(context),
          it: If(e_cnd, e_thn, e_els),
        }
      }
    | Bgn(es, e) => {
        let es = es->List.map(e => e->printExp(Expr(false), env))
        let e = e->printExp(Expr(false), env)
        {
          ann: exprBgnToString(es->List.map(e => getPrint(e)), getPrint(e))->consumeContext(
            context,
          ),
          it: Bgn(es, e),
        }
      }
    }
    let {ann: print, it} = e
    {ann: {print, srcrange}, it}
  }
  and defToString = ({ann: srcrange, it: d}: definition<srcrange>, env): definition<printAnn> => {
    let d = switch d {
    | Var(x, e) => {
        let x = x->symbolToString
        let e = e->printExp(Expr(false), env)
        {
          ann: defvarToString(getPrint(x), getPrint(e)),
          it: Var(x, e),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBody(Return, xs->List.map(x => x.it), env)
        {
          ann: deffunToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBody(Return, xs->List.map(x => x.it), env)
        {
          ann: defgenToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
          it: GFun(f, xs, b),
        }
      }
    }
    let {ann: print, it} = d
    {ann: {print: print.it, srcrange}, it}
  }
  and ebToString = (eb, ctx: statContext, env) => {
    let (e, b) = eb
    (e->printExp(Expr(false), env), b->printBlock(ctx, env))
  }
  and obToString = (ob, ctx: statContext, env) => {
    ob->Option.map(b => b->printBlock(ctx, env))
  }
  and printBlock = ({ann: srcrange, it: b}, context: statContext, env) => {
    let (ts, e) = b
    let xs = ts->List.map(xsOfTerm)->List.flatten
    if xs != list{} {
      raisePrintError("Python blocks can't declair local variables")
    }
    let ts = ts->List.map(t => t->printTerm(Step, env))
    let e = e->printExp(Stat(context), env)
    let print = printConcat("\n", list{...ts->List.map(getPrint), getPrint(e)})
    {
      ann: {print, srcrange},
      it: (ts, e),
    }
  }
  and printBody = ({ann: srcrange, it: b}, context: statContext, args, env) => {
    let (ts, e) = b
    let (refs, env) = extend(list{...args, ...ts->List.map(xsOfTerm)->List.flatten}, env)
    let ts = ts->List.map(t => t->printTerm(Step, env))
    let e = e->printExp(Stat(context), env)
    let print = printConcat(
      "\n",
      list{
        ...refs
        ->HashMap.String.toArray
        ->Js.Array2.map(((x, r)) => plain(`${r->RefDecl.toString} ${x}`))
        ->List.fromArray,
        ...ts->List.map(getPrint),
        getPrint(e),
      },
    )
    {
      ann: {print, srcrange},
      it: (ts, e),
    }
  }
  and printTerm = ({ann: srcrange, it: t}: term<srcrange>, ctx, env: env): term<printAnn> => {
    switch t {
    | Exp(it) => printExp({ann: srcrange, it}, Stat(ctx), env) |> mapAnn(v => Exp(v))
    | Def(it) => defToString({ann: srcrange, it}, env) |> mapAnn(v => Def(v))
    }
  }

  let printOutput = (os): string => {
    let rec p = (v: val): string => {
      switch v {
      | Con(c) => constantToString(c)
      | Lst(_es) => raisePrintError("Lists are not supported in JavaScript")
      | Vec(es) => `[${String.concat(", ", es->List.map(p))}]`
      }
    }
    os->List.map(o => {
      switch o {
      | OErr => "error"
      | OVal(v) => p(v)
      }
    }) |> String.concat(" ")
  }

  let printProgramFull = (insertPrintTopLevel, {ann: srcrange, it: ts}: program<srcrange>) => {
    printingTopLevel := insertPrintTopLevel
    let env = G(ts->List.map(xsOfTerm)->List.flatten->List.toArray->HashSet.String.fromArray)
    let ts = ts->List.map(t => t->printTerm(TopLevel, env))
    let print = printConcat("\n", ts->List.map(getPrint))
    {
      ann: {print, srcrange},
      it: ts,
    }
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print |> Print.toString
  }
}

module type Translator = {
  // print terms, interleaved with whitespace
  let translateOutput: string => string
  // print runnable full programs
  let translateProgram: (bool, string) => string
  let translateProgramFull: (bool, string) => program<printAnn>
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

module JSTranslator = {
  let translateOutput = src => {
    switch Parser.parseOutput(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | output =>
      switch JSPrinter.printOutput(output) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | output => output
      }
    }
  }
  let translateProgram = (printTopLevel, src) => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch JSPrinter.printProgram(printTopLevel, p) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | p => p
      }
    }
  }
  let translateProgramFull = (printTopLevel, src) => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch JSPrinter.printProgramFull(printTopLevel, p) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | p => p
      }
    }
  }
}
module PYTranslator = {
  let translateOutput = src => {
    switch Parser.parseOutput(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | output =>
      switch PYPrinter.printOutput(output) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | output => output
      }
    }
  }
  let translateProgram = (printTopLevel, src) => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch PYPrinter.printProgram(printTopLevel, p) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | p => p
      }
    }
  }
  let translateProgramFull = (printTopLevel, src) => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch PYPrinter.printProgramFull(printTopLevel, p) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | p => p
      }
    }
  }
}
