open Belt
open SExpression

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

type annotated<'it, 'ann> = {it: 'it, ann: 'ann}
let mapAnn = (f, {ann, it}: annotated<_, _>): annotated<_, _> => {
  {
    ann,
    it: f(it),
  }
}

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

type printAnn = {srcrange: srcrange, print: string}
module type Printer = {
  let printOutput: output => string
  let printProgramFull: (bool, program<srcrange>) => program<printAnn>
  let printProgram: (bool, program<srcrange>) => string
}

let indent = (s, i) => {
  let pad = Js.String.repeat(i, " ")
  Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s)
}
let indentBlock = (s, i) => indent("\n" ++ s, i)
let hcat = (s1, s2) => {
  `${s1}${indent(s2, String.length(s1))}`
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
    "(" ++ String.concat(" ", ss) ++ ")"
  }

  let defvarLike = (op, x, e) => {
    if String.contains(e, '\n') {
      `(${op} ${x}${indentBlock(e, 2)})`
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

  let defgenToString = (f, xs, b) => {
    defvarLike("defgen", listToString(list{f, ...xs}), b)
  }

  let exprSetToString = (x, e) => {
    defvarLike("set!", x, e)
  }

  let exprLamToString = (xs, b) => {
    defvarLike("lambda", listToString(xs), b)
  }
  let exprGenToString = (xs, b) => {
    defvarLike("generator", listToString(xs), b)
  }
  let exprYieldToString = e => `(yield ${e})`

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
    hcat(`(if `, `${String.concat("\n", list{e_cnd, e_thn, e_els})})`)
  }

  let letLike = (op: string, xes: list<string>, b: string) => {
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

  let symbolToString = ({it, ann}) => {
    {
      it,
      ann: {
        srcrange: ann,
        print: it,
      },
    }
  }

  let rec printExp = ({it, ann: srcrange}: expression<srcrange>): expression<printAnn> => {
    let e: annotated<expressionNode<printAnn>, string> = switch it {
    | Con(c) => {
        it: Con(c),
        ann: constantToString(c),
      }
    | Ref(x) => {
        it: Ref(x),
        ann: x,
      }
    | Set(x, e) => {
        let x = symbolToString(x)
        let e = e->printExp
        {
          ann: exprSetToString(x.ann.print, e.ann.print),
          it: Set(x, e),
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: exprLamToString(xs->List.map(x => x.ann.print), b.ann.print),
          it: Lam(xs, b),
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: exprGenToString(xs->List.map(x => x.ann.print), b.ann.print),
          it: Lam(xs, b),
        }
      }
    | Yield(e) => {
        let e = e->printExp
        {
          ann: exprYieldToString(e.ann.print),
          it: Yield(e),
        }
      }
    | AppPrm(p, es) => {
        let es = es->List.map(printExp)
        {
          ann: exprAppToString(Primitive.toString(p), es->List.map(e => e.ann.print)),
          it: AppPrm(p, es),
        }
      }
    | App(e, es) => {
        let e = e->printExp
        let es = es->List.map(printExp)
        {
          ann: exprAppToString(e.ann.print, es->List.map(e => e.ann.print)),
          it: App(e, es),
        }
      }
    | Let(xes, b) => {
        let xes = xes->List.map(xeToString)
        let b = b->printBlock
        {
          ann: exprLetToString(xes->List.map(xe => xe.ann.print), b.ann.print),
          it: Let(xes, b),
        }
      }
    | Letrec(xes, b) => {
        let xes = xes->List.map(xeToString)
        let b = b->printBlock
        {
          ann: exprLetrecToString(xes->List.map(xe => xe.ann.print), b.ann.print),
          it: Letrec(xes, b),
        }
      }
    | Cnd(ebs, ob) => {
        let ebs = ebs->List.map(ebToString)
        let ob = ob->obToString
        {
          ann: exprCndToString(
            ebs->List.map(((e, b)) => (e.ann.print, b.ann.print)),
            ob->Option.map(b => b.ann.print),
          ),
          it: Cnd(ebs, ob),
        }
      }
    | If(e_cnd, e_thn, e_els) => {
        let e_cnd = e_cnd->printExp
        let e_thn = e_thn->printExp
        let e_els = e_els->printExp
        {
          ann: exprIfToString(e_cnd.ann.print, e_thn.ann.print, e_els.ann.print),
          it: If(e_cnd, e_thn, e_els),
        }
      }
    | Bgn(es, e) => {
        let es = es->List.map(printExp)
        let e = e->printExp
        {
          ann: exprBgnToString(es->List.map(e => e.ann.print), e.ann.print),
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
          ann: defvarToString(x.ann.print, e.ann.print),
          it: Var(x, e),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: deffunToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: defgenToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
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
    let print = hcat(`[${x.ann.print}`, `${e.ann.print}]`)
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
    let print = String.concat("\n", list{...ts->List.map(t => t.ann.print), e.ann.print})
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
    let print = String.concat("\n", ts->List.map(t => t.ann.print))
    {
      ann: {print, srcrange},
      it: ts,
    }
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print
  }
}

type statContext =
  | Step
  | Return
  | TopLevel

type context =
  | Expr(bool) // the bool indicates whether we are in an infix operation
  | Stat(statContext)

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
    if es->List.some(e => String.contains(e, '\n')) {
      `(${indentBlock(String.concat(",\n", es), 2)}\n)`
    } else {
      `(${String.concat(", ", es)})`
    }
  }

  let defvarLike = (op, x, e) => {
    `${op}${x} = ${indent(e, 2)}`
  }

  let exprAppToString = (e, es) => {
    `${e}${listToString(es)}`
  }

  let printingTopLevel = ref(false)

  let consumeContext = (e, context) => {
    switch context {
    | Expr(_) => e
    | Stat(ctx) =>
      switch ctx {
      | Step => `${e};`
      | Return => `return ${e};`
      | TopLevel =>
        if printingTopLevel.contents {
          `console.log(${e});`
        } else {
          `${e};`
        }
      }
    }
  }

  let consumeContextWrap = (e: string, context: context) => {
    switch context {
    | Expr(true) => `(${e})`
    | _ => consumeContext(e, context)
    }
  }

  let consumeContextVoid = (e, context) => {
    switch context {
    | Stat(Return) => `${e};\nreturn;`
    | Stat(TopLevel) => `${e};`
    | _ => consumeContext(e, context)
    }
  }

  let consumeContextStat = (e: string, context: context) => {
    switch context {
    | Expr(_) => raisePrintError(`${e} can't be used as a expression in JavaScript`)
    | _ => consumeContextVoid(e, context)
    }
  }

  let exprAppPrmToString = (
    p: Primitive.t,
    es: list<bool => expression<printAnn>>,
    context: context,
  ) => {
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
          ann: String.concat(` ${os} `, es->List.map(e => e.ann.print))->consumeContextWrap(
            context,
          ),
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
          ann: `${e1.ann.print} ${os} ${e2.ann.print}`->consumeContextWrap(context),
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: `[ ${e1.ann.print}, ${e2.ann.print} ]`->consumeContext(context),
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: `${e1.ann.print}[0]`->consumeContext(context),
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: `${e1.ann.print}[1]`->consumeContext(context),
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: `${e1.ann.print}[0] = ${e2.ann.print}`->consumeContextStat(context),
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: `${e1.ann.print}[1] = ${e2.ann.print}`->consumeContextStat(context),
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          ann: `[ ${String.concat(`, `, es->List.map(e => e.ann.print))} ]`->consumeContext(
            context,
          ),
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: `${e1.ann.print}[${e2.ann.print}]`->consumeContext(context),
          it: (VecRef, list{e1, e2}),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          ann: `${e1.ann.print}[${e2.ann.print}] = ${e3.ann.print}`->consumeContextStat(context),
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: `${e1.ann.print}.length`->consumeContext(context),
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: `throw ${e1.ann.print}`->consumeContextWrap(context),
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: `!${e1.ann.print}`->consumeContextWrap(context),
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: `print(${e1.ann.print})`->consumeContextVoid(context),
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: `${e1.ann.print}.next()`->consumeContextVoid(context),
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
    // if String.contains(e, '\n') {
    `${op} ${exprAppToString(x, xs)} {${indentBlock(e, 2)}\n}`
    // } else {
    //   `${op} ${exprAppToString(x, xs)} { ${e} }`
    // }
  }

  let defvarToString = (x, e) => {
    `${defvarLike("let ", x, e)};`
  }

  let deffunToString = (f, xs, b) => {
    `${funLike("function", f, xs, b)}`
  }

  let defgenToString = (f, xs, b) => {
    `${funLike("function*", f, xs, b)}`
  }

  let exprSetToString = (x, e) => {
    defvarLike("", x, e)
  }

  let exprLamToString = (xs, b) => {
    funLike("function", "", xs, b)
  }
  let exprGenToString = (xs, b) => {
    funLike("function*", "", xs, b)
  }
  let exprYieldToString = e => `yield ${e}`

  let exprBgnToString = (es, e) => {
    listToString(list{...es, e})
    // `{${indentBlock(String.concat(", ", es), 2)}\n}`
    // beginLike("begin", list{...es, e})
  }

  let exprCndToString = (ebs: list<(string, string)>, ob) => {
    let ebs = {
      switch ob {
      | None => ebs
      | Some(b) => list{...ebs, ("", b)}
      }
    }
    let ebs = ebs->List.map(((e, b)) => `if ${e} {${indentBlock(b, 2)}\n}`)
    String.concat(" else ", ebs)
  }

  let exprIfToString = (e_cnd: string, e_thn: string, e_els: string) => {
    `${e_cnd} ? ${e_thn} : ${e_els}`
  }

  let symbolToString = ({it, ann}) => {
    {
      it,
      ann: {
        srcrange: ann,
        print: escapeName(it),
      },
    }
  }

  let rec printExp = ({it, ann: srcrange}, context) => {
    let e: annotated<expressionNode<printAnn>, string> = switch it {
    | Con(c) => {
        it: Con(c),
        ann: constantToString(c)->consumeContext(context),
      }
    | Ref(x) => {
        it: Ref(x),
        ann: x->escapeName->consumeContext(context),
      }
    | Set(x, e) => {
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp(Expr(false))
        {
          ann: exprSetToString(x.ann.print, e.ann.print)->consumeContextStat(context),
          it: Set(x, e),
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprLamToString(xs->List.map(x => x.ann.print), b.ann.print)->consumeContextWrap(
            context,
          ),
          it: Lam(xs, b),
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprGenToString(xs->List.map(x => x.ann.print), b.ann.print)->consumeContextWrap(
            context,
          ),
          it: Lam(xs, b),
        }
      }
    | Yield(e) => {
        let e = e->printExp(Expr(false))
        {
          ann: exprYieldToString(e.ann.print)->consumeContextWrap(context),
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
          ann: exprAppToString(e.ann.print, es->List.map(e => e.ann.print))->consumeContext(
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
            ann: `{\n${indentBlock(
                String.concat("\n", list{...xes->List.map(xe => xe.ann.print), b.ann.print}),
                2,
              )}\n}`,
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
              ebs->List.map(((e, b)) => (e.ann.print, b.ann.print)),
              ob->Option.map(b => b.ann.print),
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
            e_cnd.ann.print,
            e_thn.ann.print,
            e_els.ann.print,
          )->consumeContextWrap(context),
          it: If(e_cnd, e_thn, e_els),
        }
      }
    | Bgn(es, e) => {
        let es = es->List.map(e => e->printExp(Expr(false)))
        let e = e->printExp(Expr(false))
        {
          ann: exprBgnToString(es->List.map(e => e.ann.print), e.ann.print)->consumeContext(
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
          ann: defvarToString(x.ann.print, e.ann.print),
          it: Var(x, e),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: deffunToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: defgenToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: GFun(f, xs, b),
        }
      }
    }
    let {ann: print, it} = d
    {ann: {print, srcrange}, it}
  }
  and xeToString = ({it: xe, ann: srcrange}: bind<srcrange>): bind<printAnn> => {
    let (x, e) = xe
    let (x, e) = (symbolToString(x), e->printExp(Expr(false)))
    let print = defvarToString(x.ann.print, e.ann.print)
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
    let print = String.concat("\n", list{...ts->List.map(t => t.ann.print), e.ann.print})
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
    let print = String.concat("\n", ts->List.map(t => t.ann.print))
    {
      ann: {print, srcrange},
      it: ts,
    }
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print
  }
}

module PYPrinter = {
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
    if es->List.some(e => String.contains(e, '\n')) {
      `(${indentBlock(String.concat(",\n", es), 2)}\n)`
    } else {
      `(${String.concat(", ", es)})`
    }
  }

  let defvarLike = (op, x, e) => {
    `${op}${x} = ${indent(e, 2)}`
  }

  let exprAppToString = (e, es) => {
    `${e}${listToString(es)}`
  }

  let printingTopLevel = ref(false)

  let consumeContext = (e, context) => {
    switch context {
    | Expr(_) => e
    | Stat(ctx) =>
      switch ctx {
      | Step => `${e}`
      | Return => `return ${e}`
      | TopLevel =>
        if printingTopLevel.contents {
          `print(${e})`
        } else {
          `${e}`
        }
      }
    }
  }

  let consumeContextWrap = (e: string, context: context) => {
    switch context {
    | Expr(true) => `(${e})`
    | _ => consumeContext(e, context)
    }
  }

  let consumeContextVoid = (e, context) => {
    switch context {
    | Stat(Return) => `${e}\nreturn`
    | Stat(TopLevel) => `${e}`
    | _ => consumeContext(e, context)
    }
  }

  let consumeContextStat = (e: string, context: context) => {
    switch context {
    | Expr(_) => raisePrintError(`${e} can't be used as a expression in Python`)
    | _ => consumeContextVoid(e, context)
    }
  }

  let exprAppPrmToString = (
    p: Primitive.t,
    es: list<bool => expression<printAnn>>,
    context: context,
  ) => {
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
          ann: String.concat(` ${os} `, es->List.map(e => e.ann.print))->consumeContextWrap(
            context,
          ),
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
          ann: `${e1.ann.print} ${os} ${e2.ann.print}`->consumeContextWrap(context),
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: `[${e1.ann.print}, ${e2.ann.print}]`->consumeContext(context),
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: `${e1.ann.print}[0]`->consumeContext(context),
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: `${e1.ann.print}[1]`->consumeContext(context),
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: `${e1.ann.print}[0] = ${e2.ann.print}`->consumeContextStat(context),
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: `${e1.ann.print}[1] = ${e2.ann.print}`->consumeContextStat(context),
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          ann: `[${String.concat(`, `, es->List.map(e => e.ann.print))}]`->consumeContext(
            context,
          ),
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: `${e1.ann.print}[${e2.ann.print}]`->consumeContext(context),
          it: (VecRef, list{e1, e2}),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          ann: `${e1.ann.print}[${e2.ann.print}] = ${e3.ann.print}`->consumeContextStat(context),
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: `len(${e1.ann.print})`->consumeContext(context),
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: `raise ${e1.ann.print}`->consumeContextWrap(context),
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: `!${e1.ann.print}`->consumeContextWrap(context),
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: `print(${e1.ann.print})`->consumeContextVoid(context),
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: `next(${e1.ann.print})`->consumeContextVoid(context),
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
    `${op} ${exprAppToString(x, xs)}:${indentBlock(e, 4)}`
  }

  let defvarToString = (x, e) => {
    `${defvarLike("", x, e)}`
  }

  let deffunToString = (f, xs, b) => {
    `${funLike("def", f, xs, b)}`
  }

  let defgenToString = (f, xs, b) => {
    `${funLike("def", f, xs, b)}`
  }

  let exprSetToString = (x, e) => {
    defvarLike("", x, e)
  }

  let exprLamToString = (xs, b) => {
    `lambda ${xs|>String.concat(",")}: ${b}`
  }
  let exprGenToString = (_xs, _b) => {
    raisePrintError("In Python, lambdas can't be generators.")
  }
  let exprYieldToString = e => `yield ${e}`

  let exprBgnToString = (es, e) => {
    `${listToString(list{...es, e})}[-1]`
  }

  let exprCndToString = (ebs: list<(string, string)>, ob) => {
    let ebs = {
      switch ob {
      | None => ebs
      | Some(b) => list{...ebs, ("", b)}
      }
    }
    let ebs = ebs->List.map(((e, b)) => `if ${e}:${indentBlock(b, 4)}`)
    String.concat("else ", ebs)
  }

  let exprIfToString = (e_cnd: string, e_thn: string, e_els: string) => {
    `${e_thn} if ${e_cnd} else ${e_els}`
  }

  let symbolToString = ({it, ann}) => {
    {
      it,
      ann: {
        srcrange: ann,
        print: escapeName(it),
      },
    }
  }

  let rec printExp = ({it, ann: srcrange}, context) => {
    let e: annotated<expressionNode<printAnn>, string> = switch it {
    | Con(c) => {
        it: Con(c),
        ann: constantToString(c)->consumeContext(context),
      }
    | Ref(x) => {
        it: Ref(x),
        ann: x->escapeName->consumeContext(context),
      }
    | Set(x, e) => {
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp(Expr(false))
        {
          ann: exprSetToString(x.ann.print, e.ann.print)->consumeContextStat(context),
          it: Set(x, e),
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        switch b.it {
        | (list{}, e) => {
            let e = e->printExp(Expr(false))
            {
              ann: exprLamToString(xs->List.map(x => x.ann.print), e.ann.print)->consumeContextWrap(
                context,
              ),
              it: Lam(xs, {ann: {print: e.ann.print, srcrange: b.ann}, it: (list{}, e)}),
            }
          }
        | _ => raisePrintError("In Python, a lambda body must contains exactly one expression")
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprGenToString(xs->List.map(x => x.ann.print), b.ann.print)->consumeContextWrap(
            context,
          ),
          it: Lam(xs, b),
        }
      }
    | Yield(e) => {
        let e = e->printExp(Expr(false))
        {
          ann: exprYieldToString(e.ann.print)->consumeContextWrap(context),
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
          ann: exprAppToString(e.ann.print, es->List.map(e => e.ann.print))->consumeContext(
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
            ebs->List.map(eb => eb->ebToString(context))
          let ob = ob->obToString(context)
          {
            ann: exprCndToString(
              ebs->List.map(((e, b)) => (e.ann.print, b.ann.print)),
              ob->Option.map(b => b.ann.print),
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
            e_cnd.ann.print,
            e_thn.ann.print,
            e_els.ann.print,
          )->consumeContextWrap(context),
          it: If(e_cnd, e_thn, e_els),
        }
      }
    | Bgn(es, e) => {
        let es = es->List.map(e => e->printExp(Expr(false)))
        let e = e->printExp(Expr(false))
        {
          ann: exprBgnToString(es->List.map(e => e.ann.print), e.ann.print)->consumeContext(
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
          ann: defvarToString(x.ann.print, e.ann.print),
          it: Var(x, e),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: deffunToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: defgenToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: GFun(f, xs, b),
        }
      }
    }
    let {ann: print, it} = d
    {ann: {print, srcrange}, it}
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
    let print = String.concat("\n", list{...ts->List.map(t => t.ann.print), e.ann.print})
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

  let printProgramFull = (insertPrintTopLevel, {ann: srcrange, it: ts}) => {
    printingTopLevel := insertPrintTopLevel
    let ts = ts->List.map(t => t->printTerm(TopLevel))
    let print = String.concat("\n", ts->List.map(t => t.ann.print))
    {
      ann: {print, srcrange},
      it: ts,
    }
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print
  }
}

// module ScalaPrinter = {
//   let mutatingVariable = ref(false)
//   let usingBuffer = ref(false)

//   let consider_context = (e, ctx) => {
//     switch ctx {
//     | Expr(_) => `${e}`
//     | Stat => `${e}`
//     | Return => `${e}`
//     | TopLevel => `println(${e})`
//     }
//   }

//   let constantToString = c => {
//     switch c {
//     | Uni => "null"
//     | Num(n) => Float.toString(n)
//     | Lgc(l) =>
//       if l {
//         "true"
//       } else {
//         "false"
//       }
//     | Str(s) => "\"" ++ String.escaped(s) ++ "\""
//     }
//   }

//   let listToString = ss => {
//     "(" ++ String.concat(", ", ss) ++ ")"
//   }

//   let paraListToString = ss => {
//     if ss == list{} {
//       ""
//     } else {
//       listToString(ss)
//     }
//   }

//   let xToString = x => {
//     let re = %re("/-./g")
//     let matchFn = (matchPart, _offset, _wholeString) => {
//       Js.String2.toUpperCase(Js.String2.substringToEnd(matchPart, ~from=1))
//     }
//     let x = Js.String2.unsafeReplaceBy0(x, re, matchFn)

//     // add `$` to the beginning of reserved words
//     if x == "var" {
//       "$var"
//     } else if x == "+" {
//       "(x, y) => (x + y)"
//     } else if x == "-" {
//       "(x, y) => (x - y)"
//     } else if x == "*" {
//       "(x, y) => (x * y)"
//     } else if x == "/" {
//       "(x, y) => (x / y)"
//     } else {
//       x
//     }
//   }

//   let parameterToString = x => {
//     `${xToString(x)} : Int`
//   }

//   let defvarToString = (x: string, e) => {
//     `${mutatingVariable.contents ? "var" : "val"} ${x} = ${e}`
//   }

//   let deffunToString = (f, xs, b) => {
//     `def ${f}${paraListToString(xs)} =${indentBlock(b, 2)}`
//   }

//   let exprLamToString = (xs, b) => {
//     `${listToString(xs)} =>${indentBlock(b, 2)}`
//   }

//   let infix_consider_context = (e, ctx) => {
//     switch ctx {
//     | Expr(true) => `(${e})`
//     | _ => consider_context(e, ctx)
//     }
//   }

//   let assign_consider_context = (e, ctx) => {
//     switch ctx {
//     | Expr(true) => `(${e})`
//     | TopLevel => `${e}`
//     | Return => `${e}`
//     | _ => consider_context(e, ctx)
//     }
//   }

//   let error_consider_context = (e, _ctx) => {
//     e
//   }

//   let exprSetToString = (ctx, x, e) => {
//     `${x} = ${e}`->assign_consider_context(ctx)
//   }

//   let exprApp_prmToString = (ctx, p, es) => {
//     switch (p, es) {
//     | (Add, es) => `${String.concat(" + ", es)}`->infix_consider_context(ctx)
//     | (Sub, es) => `${String.concat(" - ", es)}`->infix_consider_context(ctx)
//     | (Mul, es) => `${String.concat(" * ", es)}`->infix_consider_context(ctx)
//     | (Div, es) => `${String.concat(" / ", es)}`->infix_consider_context(ctx)
//     | (Lt, list{e1, e2}) => `${e1} < ${e2}`->infix_consider_context(ctx)
//     | (Eq, list{e1, e2}) => `${e1} === ${e2}`->infix_consider_context(ctx)
//     | (Gt, list{e1, e2}) => `${e1} > ${e2}`->infix_consider_context(ctx)
//     | (Le, list{e1, e2}) => `${e1} <= ${e2}`->infix_consider_context(ctx)
//     | (Ge, list{e1, e2}) => `${e1} >= ${e2}`->infix_consider_context(ctx)
//     | (Ne, list{e1, e2}) => `${e1} != ${e2}`->infix_consider_context(ctx)
//     | (PairRefLeft, list{e1}) => `${e1}(0)`->consider_context(ctx)
//     | (PairRefRight, list{e1}) => `${e1}(1)`->consider_context(ctx)
//     | (PairSetLeft, list{e1, e2}) => `${e1}(0) = ${e2}`->assign_consider_context(ctx)
//     | (PairSetRight, list{e1, e2}) => `${e1}(1) = ${e2}`->assign_consider_context(ctx)
//     | (PairNew, list{e1, e2}) =>
//       `${if usingBuffer.contents {
//           "Buffer"
//         } else {
//           ""
//         }}(${e1}, ${e2})`->consider_context(ctx)
//     | (VecNew, es) =>
//       `${if usingBuffer.contents {
//           "Buffer"
//         } else {
//           ""
//         }}(${String.concat(", ", es)})`->consider_context(ctx)
//     | (VecSet, list{e1, e2, e3}) => `${e1}(${e2}) = ${e3}`->assign_consider_context(ctx)
//     | (VecRef, list{e1, e2}) => `${e1}(${e2})`->consider_context(ctx)
//     | (VecLen, list{e}) => `${e}.length`->consider_context(ctx)
//     | (Err, list{e}) => `throw ${e}`->error_consider_context(ctx)
//     | (Not, list{e}) => `! ${e}`->infix_consider_context(ctx)
//     | (Print, list{e}) => `println(${e})`->consider_context(ctx)
//     | _ =>
//       raise(
//         SMoLPrintError(`found a primitive operation (${Primitive.toString(p)}) not supported yet.`),
//       )
//     }
//   }

//   let exprAppToString = (e, es) => {
//     `${e}${paraListToString(es)}`
//   }

//   let exprBgnToString = (es, e) => {
//     `(${String.concat(", ", list{...es, e})})`
//   }

//   let exprCndToString = (ebs: list<(string, string)>, ob) => {
//     let ob = {
//       switch ob {
//       | None => ""
//       | Some(b) => ` else {\n  ${indent(b, 2)}\n}`
//       }
//     }
//     let ebs = ebs->List.map(((e, b)) => `if (${e})${indentBlock(b, 2)}\n`)
//     let ebs = String.concat(" else ", ebs)
//     ebs ++ ob
//   }

//   let exprIfToString = (e_cnd: string, e_thn: string, e_els: string) => {
//     `(${e_cnd} ? ${e_thn} : ${e_els})`
//   }

//   let exprLetToString = (xes, b) => {
//     `((${xes->List.map(((x, _e)) => x) |> String.concat(", ")})=>{${b}})(${xes->List.map(((
//         _x,
//         e,
//       )) => e) |> String.concat(", ")})`
//   }

//   let exprLetrecToString = (xes: list<(string, string)>, b) => {
//     let b = String.concat(
//       "\n",
//       list{...xes->List.map(((x: string, e: string)) => defvarToString(x, e)), b},
//     )
//     `()=>{${b}})()`
//   }

//   let rec expToString = (ctx: context, e): string => {
//     switch e.it {
//     | GLam(_xs, _b) => raise(SMoLPrintError("Generators not supported yet"))
//     | Yield(_e) => raise(SMoLPrintError("Generators not supported yet"))
//     | Con(c) => constantToString(c)->consider_context(ctx)
//     | Ref(x) => xToString(x.it)->consider_context(ctx)
//     | Set(x, e) => exprSetToString(ctx, x->unannotate->xToString, expToString(Expr(false), e))
//     | Lam(xs, b) =>
//       exprLamToString(
//         xs->List.map(unannotate)->List.map(parameterToString),
//         printBlock(Return, b),
//       )->consider_context(ctx)
//     | AppPrm(p, es) => exprApp_prmToString(ctx, p, es->List.map(expToString(Expr(true))))
//     | App(e, es) =>
//       exprAppToString(
//         expToString(Expr(false), e),
//         es->List.map(expToString(Expr(false))),
//       )->consider_context(ctx)
//     | Let(xes, b) =>
//       exprLetToString(xes->List.map(xeToString), printBlock(Return, b))->consider_context(ctx)
//     | Letrec(xes, b) =>
//       exprLetrecToString(xes->List.map(xeToString), printBlock(Return, b))->consider_context(ctx)
//     | Cnd(ebs, ob) => exprCndToString(ebs->List.map(ebToString(ctx)), obToString(ctx, ob))
//     | If(e_cnd, e_thn, e_els) =>
//       exprIfToString(
//         expToString(Expr(true), e_cnd),
//         expToString(Expr(true), e_thn),
//         expToString(Expr(true), e_els),
//       )->consider_context(ctx)
//     | Bgn(es, e) =>
//       exprBgnToString(
//         es->List.map(expToString(Expr(false))),
//         expToString(Expr(false), e),
//       )->consider_context(ctx)
//     }
//   }
//   and defToString = (d): string => {
//     switch d.it {
//     | Var(x, e) => defvarToString(x.it, expToString(Expr(false), e))
//     | Fun(f, xs, b) =>
//       deffunToString(
//         f->unannotate->xToString,
//         xs->List.map(unannotate)->List.map(parameterToString),
//         printBlock(Return, b),
//       )
//     | GFun(_f, _xs, _b) => raise(SMoLPrintError("Generators not supported yet"))
//     }
//   }
//   and xeToString = xe => {
//     let (x, e) = xe
//     (xToString(x.it), expToString(Expr(false), e))
//   }
//   and ebToString = (ctx, eb) => {
//     let (e, b) = eb
//     (expToString(Expr(false), e), printBlock(ctx, b))
//   }
//   and obToString = (ctx, ob) => {
//     ob->Option.map(printBlock(ctx))
//   }
//   and termAsStat = t => {
//     switch t {
//     | Exp(e) => expToString(Stat, e)
//     | Def(d) => defToString(d)
//     }
//   }
//   and printBlock = (ctx, b) => {
//     let (ts, e) = b
//     String.concat("\n", list{...ts->List.map(termAsStat), expToString(ctx, e)})
//   }
//   and printTerm = t => {
//     usingBuffer := true
//     switch t {
//     | Exp(e) => expToString(Expr(false), e)
//     | Def(d) => defToString(d)
//     }
//   }

//   let printProgram = (printTopLevel, p) => {
//     // when no variable mutation
//     mutatingVariable :=
//       Js.String.match_(%re("/[(]set!/"), SMoLPrinter.printProgram(false, p)) != None
//     // when no mutation at all
//     // usingBuffer := true
//     // usingBuffer := Js.String.match_(%re("/vec-set!/"), SMoLPrinter.printProgram(p)) != None
//     usingBuffer := Js.String.match_(%re("/set!/"), SMoLPrinter.printProgram(false, p)) != None
//     let tts = t => {
//       switch t {
//       | Exp(e) =>
//         expToString(
//           if printTopLevel {
//             TopLevel
//           } else {
//             Stat
//           },
//           e,
//         )
//       | Def(d) => defToString(d)
//       }
//     }
//     String.concat("\n", p.it->List.map(tts))
//   }

//   let printBlock = ((ts, e)) => {
//     String.concat("\n", list{...ts->List.map(termAsStat), expToString(Return, e)})
//   }
// }

// type js_context = context
// exception Impossible(string)
// module PYPrinter = {
//   // Python translation is tricky because we need to know whether a variable
//   // reference is pointing to non-local variable.
//   // - If the variable is local, we proceed normally.
//   // - If the variable is external but neither global nor built-in, we need to declare it local in the local scope
//   // - If the variable is global, we need to declare it global
//   // - If the variable is built-in, we proceed normally

//   type placeOfDef =
//     | BuiltIn
//     | Global
//     | NonLocal
//     | Local

//   type environment = Js.Dict.t<placeOfDef>

//   // When generating Python code, we need to think about where we are in
//   // an AST tree
//   type context = {
//     node: js_context,
//     block: placeOfDef,
//     refs: Js.Array.t<symbol>,
//     env: environment,
//   }

//   let base_env = Js.Dict.fromArray(all_primitives->Array.map(p => (Primitive.toString(p), BuiltIn)))
//   let make_global_env = xs => {
//     let env = Js.Dict.entries(base_env)
//     Js.Dict.fromArray(Array.concat(env, xs->List.map(x => (x, Global))->List.toArray))
//   }
//   let make_local_env = (env, xs) => {
//     let env = Js.Dict.entries(env)
//     let env = env->Array.map(((x, p)) => {
//       let p = switch p {
//       | Local => NonLocal
//       | p => p
//       }
//       (x, p)
//     })
//     Js.Dict.fromArray(Array.concat(env, xs->List.map(x => (x, Local))->List.toArray))
//   }

//   let constantToString = c => {
//     switch c {
//     | Uni => "None"
//     | Num(n) => Float.toString(n)
//     | Lgc(l) =>
//       if l {
//         "True"
//       } else {
//         "False"
//       }
//     | Str(s) => "\"" ++ String.escaped(s) ++ "\""
//     }
//   }

//   let listToString = ss => {
//     "(" ++ String.concat(", ", ss) ++ ")"
//   }

//   let xToString = x => {
//     if x != "-" {
//       let re = %re("/-/g")
//       let matchFn = (_matchPart, _offset, _wholeString) => {
//         "_"
//       }
//       Js.String2.unsafeReplaceBy0(x, re, matchFn)
//     } else {
//       x
//     }
//   }

//   let defvarToString = (x, e) => {
//     `${xToString(x)} = ${e}`
//   }

//   let deffunToString = (f, xs, b) => {
//     `def ${f}${listToString(xs)}:\n    ${indent(b, 4)}`
//   }

//   let exprSetToString = (ctx, x, e) => {
//     switch ctx.node {
//     | Expr(true) => `(${x} := ${e})`
//     | Expr(false) => `${x} := ${e}`
//     | Stat => `${x} = ${e}`
//     | Return => `return (${x} := ${e})`
//     | TopLevel => `${x} = ${e}`
//     }
//   }

//   let exprLamToString = (xs, b) => {
//     if xs == list{} {
//       `lambda: ${b}`
//     } else {
//       `lambda ${String.concat(",", xs)}: ${b}`
//     }
//   }

//   let ret = (ctx, code) => {
//     switch ctx.node {
//     | Return => `return ${code}`
//     | TopLevel => `print(${code})`
//     | _ => code
//     }
//   }
//   let wrap = (ctx, code) => {
//     switch ctx.node {
//     | Expr(true) => `(${code})`
//     | _ => ret(ctx, code)
//     }
//   }

//   let exprApp_prmToString = (ctx, p, es) => {
//     switch (p, es) {
//     | (Add, es) => `${String.concat(" + ", es)}` |> wrap(ctx)
//     | (Sub, es) => `${String.concat(" - ", es)}` |> wrap(ctx)
//     | (Mul, es) => `${String.concat(" * ", es)}` |> wrap(ctx)
//     | (Div, es) => `${String.concat(" / ", es)}` |> wrap(ctx)
//     | (Lt, list{e1, e2}) => `${e1} < ${e2}` |> wrap(ctx)
//     | (Eq, list{e1, e2}) => `${e1} == ${e2}` |> wrap(ctx)
//     | (Gt, list{e1, e2}) => `${e1} > ${e2}` |> wrap(ctx)
//     | (Le, list{e1, e2}) => `${e1} <= ${e2}` |> wrap(ctx)
//     | (Ge, list{e1, e2}) => `${e1} >= ${e2}` |> wrap(ctx)
//     | (Ne, list{e1, e2}) => `${e1} != ${e2}` |> wrap(ctx)
//     | (PairRefLeft, list{e1}) => `${e1}[0]` |> wrap(ctx)
//     | (PairRefRight, list{e1}) => `${e1}[1]` |> wrap(ctx)
//     | (PairSetLeft, list{e1, e2}) =>
//       switch ctx.node {
//       | Stat => `${e1}[0] = ${e2}`
//       | TopLevel => `${e1}[0] = ${e2}`
//       | Expr(true) => `${e1}.__setitem__(0, ${e2})`
//       | Expr(false) => `${e1}.__setitem__(0, ${e2})`
//       | Return => `return ${e1}.__setitem__(0, ${e2})`
//       }
//     | (PairSetRight, list{e1, e2}) =>
//       switch ctx.node {
//       | Stat => `${e1}[1] = ${e2}`
//       | TopLevel => `${e1}[1] = ${e2}`
//       | Expr(true) => `${e1}.__setitem__(1, ${e2})`
//       | Expr(false) => `${e1}.__setitem__(1, ${e2})`
//       | Return => `return ${e1}.__setitem__(1, ${e2})`
//       }
//     | (PairNew, list{e1, e2}) => `[ ${e1}, ${e2} ]` |> ret(ctx)
//     | (VecNew, es) => `[${String.concat(", ", es)}]` |> ret(ctx)
//     | (VecSet, list{e1, e2, e3}) =>
//       switch ctx.node {
//       | Stat => `${e1}[${e2}] = ${e3}`
//       | TopLevel => `${e1}[${e2}] = ${e3}`
//       | Expr(true) => `${e1}.__setitem__(${e2}, ${e3})`
//       | Expr(false) => `${e1}.__setitem__(${e2}, ${e3})`
//       | Return => `return ${e1}.__setitem__(${e2}, ${e3})`
//       }
//     | (VecRef, list{e1, e2}) => `${e1}[${e2}]` |> ret(ctx)
//     | (VecLen, list{e}) => `len(${e})` |> ret(ctx)
//     | (Err, list{e}) => `raise ${e}`
//     | (Not, list{e}) => `not ${e}` |> wrap(ctx)
//     | (Print, list{e}) => `print(${e})` |> wrap(ctx)
//     | (p, _) =>
//       raise(
//         SMoLPrintError(`found a primitive operation (${Primitive.toString(p)}) not supported yet.`),
//       )
//     }
//   }

//   let exprAppToString = (e, es) => {
//     `${e}${listToString(es)}`
//   }

//   let exprCndToString = (ebs: list<(string, string)>, ob) => {
//     let ob = {
//       switch ob {
//       | None => ""
//       | Some(b) => `else:${indentBlock(b, 4)}`
//       }
//     }
//     let ebs = ebs->List.map(((e, b)) => `if ${e}:${indentBlock(b, 4)}\n`)
//     let ebs = String.concat("el", ebs)
//     ebs ++ ob
//   }

//   let exprIfToString = (ctx, e_cnd: string, e_thn: string, e_els: string) => {
//     switch ctx.node {
//     | Expr(_) => `${e_thn} if ${e_cnd} else ${e_els}`
//     | _ => `if ${e_cnd}:${indentBlock(e_thn, 4)}\nelse:${indentBlock(e_els, 4)}`
//     }
//   }

//   let exprLetToString = (xes, b) => {
//     exprAppToString(exprLamToString(xes->List.map(((x, e)) => `${x}=${e}`), b), list{})
//   }
//   let exprLetrecToString = (_xes, _b) => {
//     raise(SMoLPrintError("Python translation does not support letrec-expression."))
//   }

//   let consider_context = (code: string, ctx: context) => {
//     switch ctx.node {
//     | Return => `return ${code}`
//     | TopLevel => `print(${code})`
//     | _ => code
//     }
//   }

//   let rec expToString = (ctx: context, e): string => {
//     switch e.it {
//     | Con(c) => constantToString(c)->consider_context(ctx)
//     | Ref(x) => xToString(x.it)->consider_context(ctx)
//     | Set(x, e) => {
//         let _ = Js.Array.unshift(unannotate(x), ctx.refs)
//         exprSetToString(
//           ctx,
//           x->unannotate->xToString,
//           expToString(
//             {
//               ...ctx,
//               node: Expr(false),
//             },
//             e,
//           ),
//         )
//       }
//     | Lam(xs, b) =>
//       exprLamToString(
//         xs->List.map(unannotate)->List.map(xToString),
//         printBlock({...ctx, node: Expr(false)}, xs, b),
//       )->consider_context(ctx)
//     | AppPrm(VecSet, es) =>
//       exprApp_prmToString(ctx, VecSet, es->List.map(expToString({...ctx, node: Expr(false)})))
//     | AppPrm(p, es) =>
//       exprApp_prmToString(
//         ctx,
//         p,
//         es->List.map(
//           expToString({
//             ...ctx,
//             node: Expr(true),
//           }),
//         ),
//       )
//     | App(e, es) =>
//       exprAppToString(
//         expToString({...ctx, node: Expr(false)}, e),
//         es->List.map(expToString({...ctx, node: Expr(false)})),
//       )->consider_context(ctx)
//     | Let(xes, b) =>
//       exprLetToString(
//         xes->List.map(xeToString(ctx)),
//         printBlock({...ctx, node: Expr(false)}, xes->List.map(((x, _e)) => x), b),
//       )->consider_context(ctx)
//     | Letrec(xes, b) =>
//       exprLetrecToString(
//         xes->List.map(xeToString(ctx)),
//         printBlock({...ctx, node: Expr(false)}, xes->List.map(((x, _e)) => x), b),
//       )->consider_context(ctx)
//     | Cnd(ebs, ob) =>
//       switch ctx.node {
//       | Expr(_) => raise(SMoLPrintError("Python translation does not fully support `cond` yet."))
//       | _ => exprCndToString(ebs->List.map(ebToString(ctx)), obToString(ctx, ob))
//       }
//     | If(e_cnd, e_thn, e_els) =>
//       exprIfToString(
//         ctx,
//         expToString({...ctx, node: Expr(false)}, e_cnd),
//         expToString(ctx, e_thn),
//         expToString(ctx, e_els),
//       )
//     | Bgn(es, e) => exprBgnToString(ctx, es, e)
//     | GLam(_xs, _b) => raise(SMoLPrintError("Generators not supported yet"))
//     | Yield(_e) => raise(SMoLPrintError("Generators not supported yet"))
//     }
//   }
//   and exprBgnToString = (ctx, es, e) => {
//     switch ctx.node {
//     | Expr(_) => {
//         let ese = list{...es, e}
//         let ese = ese->List.map(expToString({...ctx, node: Expr(false)}))
//         `[${String.concat(", ", ese)}][-1]`
//       }
//     | _ => {
//         let es = es->List.map(expToString({...ctx, node: Stat}))
//         let e = e |> expToString(ctx)
//         String.concat("\n", list{...es, e})
//       }
//     }
//   }
//   and defToString = (ctx: context, d): string => {
//     switch d.it {
//     | Var(x, e) => defvarToString(x.it, expToString({...ctx, node: Expr(false)}, e))
//     | Fun(f, xs, b) =>
//       deffunToString(
//         f->unannotate->xToString,
//         xs->List.map(unannotate)->List.map(xToString),
//         printBlock({...ctx, node: Return}, xs, b),
//       )
//     | GFun(_f, _xs, _b) => raise(SMoLPrintError("Generators not supported yet"))
//     }
//   }
//   and xeToString = (ctx: context, xe) => {
//     let (x, e) = xe
//     (xToString(x.it), expToString({...ctx, node: Expr(false)}, e))
//   }
//   and ebToString = (ctx: context, eb) => {
//     let (e, b) = eb
//     (expToString({...ctx, node: Expr(false)}, e), printBlock(ctx, list{}, b))
//   }
//   and obToString = (ctx: context, ob) => {
//     ob->Option.map(printBlock(ctx, list{}))
//   }
//   and identifier_of_term = t => {
//     switch t {
//     | Def(d) =>
//       switch d.it {
//       | Var(x, _e) => Some(x)
//       | Fun(f, _xs, _b) => Some(f)
//       | GFun(f, _xs, _b) => Some(f)
//       }
//     | Exp(_) => None
//     }
//   }
//   and xs_of_ts = ts => ts->List.keepMap(identifier_of_term)->List.map(unannotate)

//   and printBlock = (ctx: context, xs, b) => {
//     let (ts, e) = b
//     let refs: Js.Array.t<symbol> = []
//     let ys: list<string> = ts->xs_of_ts
//     // extend the outside environment
//     let ctx = {...ctx, env: make_local_env(ctx.env, list{...xs->List.map(unannotate), ...ys})}
//     // now we must be in a local scope
//     let ctx = {...ctx, block: Local}
//     let ctx = {...ctx, refs}
//     // Js.Console.log("Set up a new environment and a new refs!")
//     // Js.Console.log(ctx.env)
//     // Js.Console.log(ctx.refs)
//     switch ctx.node {
//     | Expr(_) => {
//         let block_as_expr = (ctx, ts, e) => {
//           switch ts {
//           | list{} => expToString(ctx, e)
//           | ts => {
//               let is_exp = t =>
//                 switch t {
//                 | Exp(_) => true
//                 | Def(_) => false
//                 }
//               let as_exp = t =>
//                 switch t {
//                 | Exp(e) => e
//                 | Def(_) => raise(Impossible("We have checked!"))
//                 }
//               if List.every(ts, is_exp) {
//                 let es = list{...ts->List.map(as_exp), e}
//                 let es = es->List.map(expToString({...ctx, node: Expr(false)}))
//                 `(${String.concat(", ", es)})[-1]`
//               } else {
//                 raise(
//                   SMoLPrintError(
//                     "Python translator can't translate block that contains definitions and appears in an expression context.",
//                   ),
//                 )
//               }
//             }
//           }
//         }
//         let result = block_as_expr(ctx, ts, e)
//         let refs = refs |> Js.Array.filter(x => {
//           switch Js.Dict.get(ctx.env, x)->Option.getWithDefault(NonLocal) {
//           | Local | BuiltIn => false
//           | _ => true
//           }
//         })

//         if Js.Array.length(refs) == 0 {
//           result
//         } else {
//           `("WARNING: the translation might be inaccurate", ${result})[-1]`
//         }
//       }
//     | _ =>
//       let result = String.concat(
//         "\n",
//         list{...ts->List.map(termToString({...ctx, node: Stat})), expToString(ctx, e)},
//       )
//       let refs = refs |> Js.Array.filter(x => {
//         switch Js.Dict.get(ctx.env, x)->Option.getWithDefault(NonLocal) {
//         | Local | BuiltIn => false
//         | _ => true
//         }
//       })
//       let globals = refs |> Js.Array.filter(x => {
//         switch Js.Dict.get(ctx.env, x)->Option.getWithDefault(NonLocal) {
//         | Global => true
//         | _ => false
//         }
//       })
//       let nonlocals = refs |> Js.Array.filter(x => {
//         switch Js.Dict.get(ctx.env, x)->Option.getWithDefault(NonLocal) {
//         | NonLocal => true
//         | _ => false
//         }
//       })
//       let decl_globals = if Array.length(globals) == 0 {
//         ""
//       } else {
//         `global ${String.concat(", ", globals->List.fromArray)}\n`
//       }
//       let decl_nonlocals = if Array.length(nonlocals) == 0 {
//         ""
//       } else {
//         `nonlocal ${String.concat(", ", nonlocals->List.fromArray)}\n`
//       }
//       `${decl_globals}${decl_nonlocals}${result}`
//     }
//   }
//   and termToString = (ctx, t) => {
//     switch t {
//     | Exp(e) => expToString(ctx, e)
//     | Def(d) => defToString(ctx, d)
//     }
//   }

//   let printTerm = t => {
//     let ctx = {node: Stat, block: Global, env: make_global_env(list{t}->xs_of_ts), refs: []}
//     termToString(ctx, t)
//   }

//   let printProgram = (printTopLevel, ts) => {
//     let ctx = {
//       node: if printTopLevel {
//         TopLevel
//       } else {
//         Stat
//       },
//       block: Global,
//       env: make_global_env(ts->xs_of_ts),
//       refs: [],
//     }
//     String.concat("\n", ts->List.map(termToString(ctx)))
//   }

//   let printBlock = ((ts, e)) => {
//     let ctx = {node: Return, block: Local, env: Js.Dict.empty(), refs: []}
//     printBlock(ctx, list{}, (ts, e))
//   }
// }

// module CommonPrinter = {
//   let consider_context = (e, ctx) => {
//     switch ctx {
//     | Expr(_) => `${e}`
//     | Return => `return ${e}`
//     | Stat | TopLevel => `${e}`
//     }
//   }

//   let constantToString = c => {
//     switch c {
//     | Uni => "null"
//     | Num(n) => Float.toString(n)
//     | Lgc(l) =>
//       if l {
//         "true"
//       } else {
//         "false"
//       }
//     | Str(s) => "\"" ++ String.escaped(s) ++ "\""
//     }
//   }

//   let listToString = ss => {
//     "(" ++ String.concat(", ", ss) ++ ")"
//   }

//   let xToString = x => {
//     if x != "-" {
//       let re = %re("/-/g")
//       let matchFn = (_matchPart, _offset, _wholeString) => {
//         "_"
//       }
//       Js.String2.unsafeReplaceBy0(x, re, matchFn)
//     } else {
//       x
//     }
//   }

//   let defvarToString = (x: string, e) => {
//     `let ${xToString(x)} = ${e}`
//   }

//   let deffunToString = (f, xs, b) => {
//     `fun ${f}${listToString(xs)}:${indentBlock(b, 2)}\nend`
//   }
//   let defgenToString = (f, xs, b) => {
//     `gen fun ${f}${listToString(xs)}:${indentBlock(b, 2)}\nend`
//   }

//   let exprLamToString = (xs, b) => {
//     `lam ${listToString(xs)}:${indentBlock(b, 2)}\nend`
//   }

//   let exprGenToString = (xs, b) => {
//     `gen ${listToString(xs)}:${indentBlock(b, 2)}\nend`
//   }

//   let infix_consider_context = (e, ctx) => {
//     switch ctx {
//     | Expr(true) => `(${e})`
//     | _ => consider_context(e, ctx)
//     }
//   }

//   let assign_consider_context = (e, ctx) => {
//     switch ctx {
//     | Expr(true) => `(${e})`
//     | TopLevel => `${e}`
//     | Return => `${e}\nreturn`
//     | _ => consider_context(e, ctx)
//     }
//   }

//   let error_consider_context = (e, _ctx) => {
//     e
//   }

//   let exprSetToString = (ctx, x, e) => {
//     `${x} = ${e}`->assign_consider_context(ctx)
//   }

//   let exprApp_prmToString = (ctx, p, es) => {
//     switch (p, es) {
//     | (Add, es) => `${String.concat(" + ", es)}`->infix_consider_context(ctx)
//     | (Sub, es) => `${String.concat(" - ", es)}`->infix_consider_context(ctx)
//     | (Mul, es) => `${String.concat(" * ", es)}`->infix_consider_context(ctx)
//     | (Div, es) => `${String.concat(" / ", es)}`->infix_consider_context(ctx)
//     | (Lt, list{e1, e2}) => `${e1} < ${e2}`->infix_consider_context(ctx)
//     | (Eq, list{e1, e2}) => `${e1} == ${e2}`->infix_consider_context(ctx)
//     | (Gt, list{e1, e2}) => `${e1} > ${e2}`->infix_consider_context(ctx)
//     | (Le, list{e1, e2}) => `${e1} <= ${e2}`->infix_consider_context(ctx)
//     | (Ge, list{e1, e2}) => `${e1} >= ${e2}`->infix_consider_context(ctx)
//     | (Ne, list{e1, e2}) => `${e1} != ${e2}`->infix_consider_context(ctx)
//     | (PairRefLeft, list{e1}) => `${e1}[0]`->consider_context(ctx)
//     | (PairRefRight, list{e1}) => `${e1}[1]`->consider_context(ctx)
//     | (PairSetLeft, list{e1, e2}) => `${e1}[0] = ${e2}`->assign_consider_context(ctx)
//     | (PairSetRight, list{e1, e2}) => `${e1}[1] = ${e2}`->assign_consider_context(ctx)
//     | (PairNew, list{e1, e2}) => `vec[${e1}, ${e2}]`->consider_context(ctx)
//     | (VecNew, es) => `vec[${String.concat(", ", es)}]`->consider_context(ctx)
//     | (VecSet, list{e1, e2, e3}) => `${e1}[${e2}] = ${e3}`->assign_consider_context(ctx)
//     | (VecRef, list{e1, e2}) => `${e1}[${e2}]`->consider_context(ctx)
//     | (VecLen, list{e}) => `${e}.length()`->consider_context(ctx)
//     | (Err, list{e}) => `throw ${e}`->error_consider_context(ctx)
//     | (Not, list{e}) => `! ${e}`->infix_consider_context(ctx)
//     | (Print, list{e}) => `print(${e})`->consider_context(ctx)
//     | (Next, list{e}) => `next(${e})`->consider_context(ctx)
//     | _ =>
//       raise(
//         SMoLPrintError(`found a primitive operation (${Primitive.toString(p)}) not supported yet.`),
//       )
//     }
//   }

//   let exprAppToString = (e, es) => {
//     `${e}${listToString(es)}`
//   }

//   let exprBgnToString = (es, e) => {
//     `(${String.concat(", ", list{...es, e})})`
//   }

//   let exprCndToString = (ebs: list<(string, string)>, ob) => {
//     let ob = {
//       switch ob {
//       | None => ""
//       | Some(b) => `else:${indentBlock(b, 2)}\nend`
//       }
//     }
//     let ebs = ebs->List.map(((e, b)) => `if ${e}:${indentBlock(b, 2)}\n`)
//     let ebs = String.concat("else ", ebs)
//     ebs ++ ob
//   }

//   let exprYieldToString = e => `yield ${e}`

//   let exprIfToString = (ctx, e_cnd: string, e_thn: string, e_els: string) => {
//     switch ctx {
//     | Expr(in_infix) => {
//         let e = `${e_thn} ? ${e_cnd} : ${e_els}`
//         if in_infix {
//           `${e}`
//         } else {
//           e
//         }
//       }
//     | _ => `if ${e_cnd}:${indentBlock(e_thn, 2)}\nelse:${indentBlock(e_els, 2)}\nend`
//     }
//   }

//   let exprLetToString = (xes, b) => {
//     exprAppToString(
//       exprLamToString(xes->List.map(((x, _e)) => x), b),
//       xes->List.map(((_x, e)) => e),
//     )
//   }

//   let exprLetrecToString = (xes: list<(string, string)>, b) => {
//     let b = String.concat(
//       "\n",
//       list{...xes->List.map(((x: string, e: string)) => defvarToString(x, e)), b},
//     )
//     exprLetToString(list{}, b)
//   }

//   let contains_space = (p: Primitive.t): bool => {
//     switch p {
//     | Add => true
//     | Sub => true
//     | Mul => true
//     | Div => true
//     | Lt => true
//     | Eq => true
//     | Gt => true
//     | Le => true
//     | Ge => true
//     | Ne => true
//     | PairSetRight => true
//     | PairSetLeft => true
//     | VecSet => true
//     | Not => true
//     | _ => false
//     }
//   }

//   let rec expToString = (ctx: context, e): string => {
//     switch e.it {
//     | Con(c) => constantToString(c)->consider_context(ctx)
//     | Ref(x) => xToString(x.it)->consider_context(ctx)
//     | Set(x, e) => exprSetToString(ctx, x->unannotate->xToString, expToString(Expr(false), e))
//     | Lam(xs, b) =>
//       exprLamToString(
//         xs->List.map(unannotate)->List.map(xToString),
//         printBlock(Return, b),
//       )->consider_context(ctx)
//     | GLam(xs, b) =>
//       exprGenToString(
//         xs->List.map(unannotate)->List.map(xToString),
//         printBlock(Return, b),
//       )->consider_context(ctx)
//     | Yield(e) => exprYieldToString(expToString(Expr(false), e))->consider_context(ctx)
//     | AppPrm(p, es) =>
//       exprApp_prmToString(ctx, p, es->List.map(expToString(Expr(contains_space(p)))))
//     | App(e, es) =>
//       exprAppToString(
//         expToString(Expr(false), e),
//         es->List.map(expToString(Expr(false))),
//       )->consider_context(ctx)
//     | Let(xes, b) =>
//       exprLetToString(xes->List.map(xeToString), printBlock(Return, b))->consider_context(ctx)
//     | Letrec(xes, b) =>
//       exprLetrecToString(xes->List.map(xeToString), printBlock(Return, b))->consider_context(ctx)
//     | Cnd(ebs, ob) => exprCndToString(ebs->List.map(ebToString(ctx)), obToString(ctx, ob))
//     | If(e_cnd, e_thn, e_els) =>
//       exprIfToString(
//         ctx,
//         expToString(Expr(true), e_cnd),
//         expToString(ctx, e_thn),
//         expToString(ctx, e_els),
//       )
//     | Bgn(es, e) =>
//       exprBgnToString(
//         es->List.map(expToString(Expr(false))),
//         expToString(Expr(false), e),
//       )->consider_context(ctx)
//     }
//   }
//   and defToString = (d): string => {
//     switch d.it {
//     | Var(x, e) => defvarToString(x.it, expToString(Expr(false), e))
//     | Fun(f, xs, b) =>
//       deffunToString(
//         f->unannotate->xToString,
//         xs->List.map(unannotate)->List.map(xToString),
//         printBlock(Return, b),
//       )
//     | GFun(f, xs, b) =>
//       defgenToString(
//         f->unannotate->xToString,
//         xs->List.map(unannotate)->List.map(xToString),
//         printBlock(Return, b),
//       )
//     }
//   }
//   and xeToString = xe => {
//     let (x, e) = xe
//     (xToString(x.it), expToString(Expr(false), e))
//   }
//   and ebToString = (ctx, eb) => {
//     let (e, b) = eb
//     (expToString(Expr(false), e), printBlock(ctx, b))
//   }
//   and obToString = (ctx, ob) => {
//     ob->Option.map(printBlock(ctx))
//   }
//   and termAsStat = t => {
//     switch t {
//     | Exp(e) => expToString(Stat, e)
//     | Def(d) => defToString(d)
//     }
//   }
//   and printBlock = (ctx, b) => {
//     let (ts, e) = b
//     String.concat("\n", list{...ts->List.map(termAsStat), expToString(ctx, e)})
//   }
//   and printTerm = t => {
//     switch t {
//     | Exp(e) => expToString(Expr(false), e)
//     | Def(d) => defToString(d)
//     }
//   }

//   let printProgram = (printTopLevel, p) => {
//     let tts = t => {
//       switch t {
//       | Exp(e) =>
//         expToString(
//           if printTopLevel {
//             TopLevel
//           } else {
//             Stat
//           },
//           e,
//         )
//       | Def(d) => defToString(d)
//       }
//     }
//     String.concat("\n", p->List.map(tts))
//   }

//   let printBlock = ((ts, e)) => {
//     String.concat("\n", list{...ts->List.map(termAsStat), expToString(Return, e)})
//   }
// }

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

// module PYTranslator = {
//   let translateTerms = src => {
//     switch Parser.parseTerms(src) {
//     | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
//     | ts =>
//       switch String.concat(" ", ts->List.map(PYPrinter.printTerm)) {
//       | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
//       | dst => dst
//       }
//     }
//   }
//   let translateProgram = (printTopLevel, src) => {
//     switch Parser.parseProgram(src) {
//     | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
//     | p =>
//       switch PYPrinter.printProgram(printTopLevel, p) {
//       | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
//       | dst => dst
//       }
//     }
//   }
// }

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
// module ScalaTranslator = {
//   let translateTerms = src => {
//     switch Parser.parseTerms(src) {
//     | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
//     | ts =>
//       switch String.concat(" ", ts->List.map(ScalaPrinter.printTerm)) {
//       | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
//       | dst => dst
//       }
//     }
//   }
//   let translateProgram = (printTopLevel, src) => {
//     switch Parser.parseProgram(src) {
//     | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
//     | p =>
//       switch ScalaPrinter.printProgram(printTopLevel, p) {
//       | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
//       | dst => dst
//       }
//     }
//   }
// }

// module CommonTranslator = {
//   let translateTerms = src => {
//     switch Parser.parseTerms(src) {
//     | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
//     | ts =>
//       switch String.concat(" ", ts->List.map(CommonPrinter.printTerm)) {
//       | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
//       | dst => dst
//       }
//     }
//   }
//   let translateProgram = (printTopLevel, src) => {
//     switch Parser.parseProgram(src) {
//     | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
//     | p =>
//       switch CommonPrinter.printProgram(printTopLevel, p) {
//       | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
//       | dst => dst
//       }
//     }
//   }
// }
