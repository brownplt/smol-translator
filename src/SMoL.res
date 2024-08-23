open Belt
open SExpression

let mapAnn = (f, {ann, it}: annotated<_, _>): annotated<_, _> => {
  {
    ann,
    it: f(it),
  }
}

type rec printNode<'id> =
  | Plain(string)
  | Group(list<print<'id>>)
and print<'id> = annotated<printNode<'id>, option<'id>>

module Print = {
  type t<'id> = printNode<'id>
  let toSourceMap = (t, id) => {
    let hMap = ref(Map.make(~id))
    let ln = ref(0)
    let ch = ref(0)
    let rec f = ({it, ann}) => {
      let begin = {ln: ln.contents, ch: ch.contents}
      switch it {
      | Group(es) => es->List.forEach(f)
      | Plain(s) =>
        s |> String.iter(c => {
          switch c {
          | '\n' => {
              ln := ln.contents + 1
              ch := 0
            }
          | _ => ch := ch.contents + 1
          }
        })
      }
      let end = {ln: ln.contents, ch: ch.contents}
      ann->Option.forEach(ann => {
        hMap := Map.set(hMap.contents, ann, {begin, end})
      })
    }
    f(t)
    hMap.contents
  }
  let rec toString = it => {
    switch it {
    | Plain(s) => s
    | Group(ts) =>
      String.concat(
        "",
        ts->List.map(({it}) => {
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
  let concat2 = (p1, s, p2) => {
    Group(list{p1, {it: Plain(s), ann: None}, p2})
  }

  let concat = (s: string, ts: list<_>) => {
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
    Group(intersperse({it: Plain(s), ann: None}, ts))
  }

  let string = it => {
    {it: Plain(it), ann: None}
  }

  let pad = (prefix: string, it, suffix: string) => {
    let prefix = string(prefix)
    let suffix = string(suffix)
    Group(list{prefix, it, suffix})
  }

  let dummyAnn = it => {it, ann: None}
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
let group = ss => {it: Group(ss), ann: None}
let group2 = (s1, s2) => Group(list{s1, s2})
let surround = (prefix, s, suffix) => Group(list{
  Print.string(prefix),
  Print.dummyAnn(s),
  Print.string(suffix),
})

type constant =
  | Uni
  | Nil
  | Num(float)
  | Lgc(bool)
  | Str(string)
  | Sym(string)

type rec struct =
  | Lst(list<val>)
  | Vec(list<val>)
and val =
  | Ref(int)
  | Con(constant)
  | Struct(option<int>, struct)

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
and blockNode<'ann> =
  | BRet(expression<'ann>)
  | BCons(term<'ann>, block<'ann>)
and block<'ann> = annotated<blockNode<'ann>, 'ann>

and definitionNode<'ann> =
  | Var(annotated<symbol, 'ann>, expression<'ann>)
  | Fun(annotated<symbol, 'ann>, list<annotated<symbol, 'ann>>, block<'ann>)
  | GFun(annotated<symbol, 'ann>, list<annotated<symbol, 'ann>>, block<'ann>)
and definition<'ann> = annotated<definitionNode<'ann>, 'ann>

and termNode<'ann> = Def(definitionNode<'ann>) | Exp(expressionNode<'ann>)
and term<'ann> = annotated<termNode<'ann>, 'ann>

and programNode<'ann> =
  | PNil
  | PCons(term<'ann>, program<'ann>)
and program<'ann> = annotated<programNode<'ann>, 'ann>

let rec termsOfBlock = ({it}) => {
  switch it {
  | BRet(_e) => list{}
  | BCons(t, b) => list{t, ...termsOfBlock(b)}
  }
}

let rec termsOfProgram = ({it}) => {
  switch it {
  | PNil => list{}
  | PCons(t, b) => list{t, ...termsOfProgram(b)}
  }
}

let xsOfTerm = t => {
  switch t.it {
  | Exp(_) => list{}
  | Def(Var(x, _)) => list{x}
  | Def(Fun(f, _xs, _b)) => list{f}
  | Def(GFun(f, _xs, _b)) => list{f}
  }
}

let xsOfBlock = b => List.flatten(termsOfBlock(b)->List.map(xsOfTerm))
let xsOfProgram = p => List.flatten(termsOfProgram(p)->List.map(xsOfTerm))

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
    | SExprKindError(SExprKind.t, string, sexpr)
    | SExprArityError(Arity.t, string, list<sexpr>)
    | LiteralSymbolError(string)
    | LiteralListError(sexpr)
    | TermKindError(TermKind.t, string, term<sourceLocation>)
  let toString = t => {
    switch t {
    | SExprParseError(msg) => `expecting a (valid) s-expression, but the input is not: ${msg}`
    | SExprKindError(_kind, context, sexpr) =>
      `expecting a ${context}, given ${SExpr.toString(sexpr)}`
    | SExprArityError(_arity_expectation, context, es) =>
      `expecting ${context}, given ${String.concat(" ", es->List.map(SExpr.toString))}`
    | LiteralSymbolError(x) => `expecting a literal value, given a symbol ${x}`
    | LiteralListError(sexpr) => `expecting a constant or a vector, given ${SExpr.toString(sexpr)}`
    | TermKindError(_term_kind, context, term) =>
      // `expecting ${context}, given ${SMoLPrinter.printTerm(term)}`
      `expecting ${context}, given something else at ${SourcePoint.toString(
          term.ann.begin,
        )}-${SourcePoint.toString(term.ann.end)}`
    }
  }
}
exception SMoLParseError(ParseError.t)
let raiseParseError = err => raise(SMoLParseError(err))

let rec makeProgram = (ts: list<term<sourceLocation>>): program<sourceLocation> => {
  switch ts {
  | list{} => {
      ann: {
        begin: {
          ln: 0,
          ch: 0,
        },
        end: {
          ln: 0,
          ch: 0,
        },
      },
      it: PNil,
    }
  | list{t} => {
      let rest: program<sourceLocation> = {
        it: PNil,
        ann: {
          begin: t.ann.end,
          end: t.ann.end,
        },
      }
      {
        ann: t.ann,
        it: PCons(t, rest),
      }
    }
  | list{t, ...ts} => {
      let p = makeProgram(ts)
      {
        ann: {
          begin: t.ann.begin,
          end: p.ann.end,
        },
        it: PCons(t, p),
      }
    }
  }
}

let rec makeBlock = (ts, e: expression<sourceLocation>): block<sourceLocation> => {
  switch ts {
  | list{} => {
      ann: e.ann, // it might looks unusual to copy a source loc. But this is indeed the case in the Lispy syntax, where no `return` keyword is needed. In other languages, this might or might not be the case. I don't want to overfit the AST to the Lispy syntax
      it: BRet(e),
    }
  | list{t, ...ts} => {
      let b = makeBlock(ts, e)
      {
        ann: {
          begin: t.ann.begin,
          end: b.ann.end,
        },
        it: BCons(t, b),
      }
    }
  }
}
module Parser = {
  let constant_of_atom = (atom: atom) => {
    switch atom {
    | Str(s) => Str(s)
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

  let outputletOfSExpr = (e: sexpr): outputlet => {
    let {ann: _, it} = e
    switch it {
    | Atom(Sym("error")) => OErr
    | _ =>
      OVal({
        let rec p = (e: sexpr): val => {
          switch e.it {
          | Atom(atom) => Con(constant_of_atom(atom))
          | Sequence(Vector, _b, es) => Struct(None, Vec(es->List.map(p)))
          | Sequence(List, _b, es) => Struct(None, Lst(es->List.map(p)))
          }
        }
        p(e)
      })
    }
  }

  let rec parseValue = (e: sexpr) => {
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

  let as_id = (context, e: sexpr) => {
    switch e.it {
    | Atom(Sym(x)) => {it: x, ann: e.ann}
    | _ => raiseParseError(SExprKindError(Atom, context, e))
    }
  }

  let as_list = (context, e: sexpr) => {
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

  let expr_of_atom = (atom: atom) => {
    switch atom {
    | Str(s) => Con(Str(s))
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

  let rec letstar = (ann, xes, body: block<sourceLocation>) => {
    switch xes {
    | list{} =>
      switch body.it {
      | BRet(e) => e
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

  let rec parseTerm = (e: sexpr): term<sourceLocation> => {
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
          | list{({it: Atom(Sym("else")), ann: _}: sexpr, terms, result)} => {
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
    switch src->SExpr.fromString->List.map(parseTerm) {
    | terms => terms
    | exception SExpression.SExpressionError(err) =>
      raiseParseError(SExprParseError(SExpression.Error.toString(err)))
    }
  }

  let parseProgram = (src): program<sourceLocation> => {
    let ts = parseTerms(src)
    makeProgram(ts)
  }

  let parseOutput = src => {
    switch src->SExpr.fromString->List.map(outputletOfSExpr) {
    | terms => terms
    | exception SExpression.SExpressionError(err) =>
      raiseParseError(SExprParseError(SExpression.Error.toString(err)))
    }
  }
}

type exn += SMoLPrintError(string)
let raisePrintError = err => raise(SMoLPrintError(err))

type printAnn = {sourceLocation: sourceLocation, print: Print.t<sourceLocation>}
module type Printer = {
  let printName: string => string
  let printOutputlet: outputlet => string
  let printOutput: (~sep: string=?, list<outputlet>) => string
  let printStandAloneTerm: term<sourceLocation> => string
  let printProgram: (bool, program<sourceLocation>) => string
  let printProgramFull: (bool, program<sourceLocation>) => program<printAnn>
}

let getPrint = ({ann: {print, sourceLocation}}) => {
  {it: print, ann: Some(sourceLocation)}
}

let indent = (t: annotated<Print.t<sourceLocation>, option<sourceLocation>>, i): annotated<
  Print.t<sourceLocation>,
  option<sourceLocation>,
> => {
  let pad = Js.String.repeat(i, " ")
  t |> mapAnn(Print.map(s => Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s)))
}
let indentBlock = (s, i) => indent(group(list{Print.string("\n"), s}), i)
let hcat = (s1, s2) => {
  Group(list{s1, indent(s2, String.length(s1.it |> Print.toString))})
}

module SMoLPrinter = {
  let printName = x => x

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
    | Sym(s) => s
    }
  }

  let listToString = ss => {
    open Print
    pad("(", Print.dummyAnn(concat(" ", ss)), ")")
  }

  let defvarLike = (op, x: annotated<_, _>, e: annotated<_, _>) => {
    if containsNL(e.it) || op == "deffun" || op == "defgen" {
      Group(list{
        Print.string("("),
        Print.string(op),
        Print.string(" "),
        x,
        indentBlock(e, 2),
        Print.string(")"),
      })
    } else {
      listToString(list{Print.string(op), x, e})
    }
  }

  let defvarToString = (x, e) => {
    defvarLike("defvar", x, e)
  }

  let deffunToString = (f, xs, b) => {
    defvarLike("deffun", Print.dummyAnn(listToString(list{f, ...xs})), b)
  }

  let defgenToString = (f, xs, b) => {
    defvarLike("defgen", Print.dummyAnn(listToString(list{f, ...xs})), b)
  }

  let exprSetToString = (x, e) => {
    defvarLike("set!", x, e)
  }

  let exprLamToString = (xs, b) => {
    defvarLike("lambda", Print.dummyAnn(listToString(xs)), b)
  }
  let exprGenToString = (xs, b) => {
    defvarLike("generator", Print.dummyAnn(listToString(xs)), b)
  }
  let exprYieldToString = e => Group({list{Print.string("(yield "), e, Print.string(")")}})

  let exprAppToString = (e, es) => {
    listToString(list{e, ...es})
  }

  let beginLike = (op, ts) => {
    Group(list{
      Print.string("("),
      Print.string(op),
      indentBlock(Print.concat("\n", ts) |> Print.dummyAnn, 2),
      Print.string(")"),
    })
    // `(${op}${)`
  }
  let exprBgnToString = (es, e) => {
    beginLike("begin", list{...es, e})
  }

  let exprCndToString = (ebs: list<(annotated<_, _>, annotated<_, _>)>, ob) => {
    let ebs = {
      switch ob {
      | None => ebs
      | Some(b) => list{...ebs, (Print.string("else"), b)}
      }
    }
    let ebs =
      ebs->List.map(((e, b)) =>
        group(list{Print.string("["), e, indentBlock(b, 1), Print.string("]")})
      )
    beginLike("cond", ebs)
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    hcat(
      Print.string(`(if `),
      group2(
        Print.concat("\n", list{e_cnd, e_thn, e_els}) |> Print.dummyAnn,
        Print.string(")"),
      )->Print.dummyAnn,
    )
    // hcat(Print.string(`(if `), `${String.concat("\n", list{e_cnd, e_thn, e_els})})`)
  }

  let letLike = (op: string, xes: list<_>, b: _) => {
    let xes = Print.concat("\n", xes) |> Print.dummyAnn
    let xes = group(list{Print.string("("), indent(xes, 1), Print.string(")")})
    Group(list{
      hcat(
        group(list{Print.string("("), Print.string(op), Print.string(" ")}),
        xes,
      ) |> Print.dummyAnn,
      indentBlock(b, 2),
      Print.string(")"),
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
        sourceLocation: ann,
        print: Plain(it),
      },
    }
  }

  let rec printExp = ({it, ann: sourceLocation}: expression<sourceLocation>): expression<
    printAnn,
  > => {
    let e: annotated<expressionNode<printAnn>, Print.t<sourceLocation>> = switch it {
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
          ann: exprAppToString(Print.string(Primitive.toString(p)), es->List.map(e => getPrint(e))),
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
    {ann: {print, sourceLocation}, it}
  }
  and printDef = ({ann: sourceLocation, it: d}: definition<sourceLocation>): definition<
    printAnn,
  > => {
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
    {ann: {print, sourceLocation}, it}
  }
  and xeToString = ({it: xe, ann: sourceLocation}: bind<sourceLocation>): bind<printAnn> => {
    let (x, e) = xe
    let (x, e) = (symbolToString(x), printExp(e))
    let print = hcat(
      group2(Print.string("["), getPrint(x)) |> Print.dummyAnn,
      group2(getPrint(e), Print.string("]")) |> Print.dummyAnn,
    )
    {
      it: (x, e),
      ann: {
        print,
        sourceLocation,
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
  and printBlock = ({ann: sourceLocation, it: b}) => {
    switch b {
    | BRet(e) => {
        let e = printExp(e)
        {
          ann: e.ann,
          it: BRet(e),
        }
      }
    | BCons(t, b) => {
        let t = printTerm(t)
        let b = printBlock(b)
        let print = Group(list{getPrint(t), Print.string("\n"), getPrint(b)})
        {
          ann: {print, sourceLocation},
          it: BCons(t, b),
        }
      }
    }
  }
  and printTerm = ({ann: sourceLocation, it: t}: term<sourceLocation>): term<printAnn> => {
    switch t {
    | Exp(it) => printExp({ann: sourceLocation, it}) |> mapAnn(v => Exp(v))
    | Def(it) => printDef({ann: sourceLocation, it}) |> mapAnn(v => Def(v))
    }
  }

  let printOutputlet = o => {
    let rec p = (v: val): string => {
      switch v {
      | Ref(i) => `#${i |> Int.toString}#`
      | Con(c) => constantToString(c)
      | Struct(i, content) => {
          let i = switch i {
          | None => ""
          | Some(i) => `#${i |> Int.toString}=`
          }
          let content = switch content {
          | Lst(es) => `(${String.concat(" ", es->List.map(p))})`
          | Vec(es) => `#(${String.concat(" ", es->List.map(p))})`
          }
          `${i}${content}`
        }
      }
    }
    switch o {
    | OErr => "error"
    | OVal(v) => p(v)
    }
  }

  let printOutput = (~sep=" ", os): string => {
    os->List.map(printOutputlet) |> String.concat(sep)
  }

  let printProgramFull = (_insertPrintTopLevel, p: program<sourceLocation>) => {
    let rec print = ({it, ann: sourceLocation}: program<sourceLocation>): program<printAnn> => {
      switch it {
      | PNil => {it: PNil, ann: {print: Group(list{}), sourceLocation}}
      | PCons(t, p) => {
          let t = printTerm(t)
          switch p {
          | {it: PNil} => {
              it: PCons(
                t,
                {
                  it: PNil,
                  ann: {
                    print: Plain(""),
                    sourceLocation: {
                      begin: sourceLocation.end,
                      end: sourceLocation.end,
                    },
                  },
                },
              ),
              ann: {
                print: getPrint(t).it,
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(getPrint(t), "\n", getPrint(p)),
                  sourceLocation,
                },
              }
            }
          }
        }
      }
    }
    print(p)
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print->Print.toString
  }

  let printStandAloneTerm = t => {
    printTerm(t).ann.print->Print.toString
  }
}

let moveBeginChByOne = (sourceLocation: sourceLocation): sourceLocation => {
  {
    ...sourceLocation,
    begin: {
      ...sourceLocation.begin,
      ch: sourceLocation.begin.ch + 1,
    },
  }
}

let rec insertTopLevelPrint = (p: program<sourceLocation>): program<sourceLocation> => {
  {
    ...p,
    it: switch p.it {
    | PNil => PNil
    | PCons(t, p) => {
        let t = {
          ...t,
          it: switch t.it {
          | Exp(e) => {
              let rec ie = e => {
                let iae = ae => {...ae, it: ie(ae.it)}
                let rec ib = (b: block<sourceLocation>): block<sourceLocation> => {
                  ...b,
                  it: switch b.it {
                  | BRet(e) => BRet({...e, it: ie(e.it)})
                  | BCons(t, b) => BCons(t, ib(b))
                  },
                }
                let ieb = ((e, b)) => (e, ib(b))
                let iebs = ebs => List.map(ebs, ieb)
                let iob = ob => Option.map(ob, ib)
                switch e {
                | Set(x, e) => Set(x, e)
                | Bgn(es, e) => Bgn(es, iae(e))
                | If(ec, et, el) => If(ec, iae(et), iae(el))
                | Cnd(ebs, ob) => Cnd(iebs(ebs), iob(ob))
                | Let(bs, b) => Let(bs, ib(b))
                | Letrec(bs, b) => Let(bs, ib(b))
                | AppPrm(VecSet, es) => AppPrm(VecSet, es)
                | AppPrm(PairSetLeft, es) => AppPrm(PairSetLeft, es)
                | AppPrm(PairSetRight, es) => AppPrm(PairSetRight, es)
                | AppPrm(Err, es) => AppPrm(Err, es)
                | AppPrm(Print, es) => AppPrm(Print, es)
                | Yield(e) => Yield(e)
                | e => AppPrm(Print, list{{it: e, ann: moveBeginChByOne(t.ann)}})
                }
              }
              Exp(ie(e))
            }
          | it => it
          },
        }
        PCons(t, insertTopLevelPrint(p))
      }
    },
  }
}

type statContext =
  | Step
  | Return

type contexted<'a, 'b> = {
  expr: bool => 'a, // the bool indicates whether we are in an infix operation
  stat: statContext => 'b,
}
type contexted1<'a> = contexted<'a, 'a>

type consumer = printNode<sourceLocation> => contexted<
  printNode<sourceLocation>,
  (string, printNode<sourceLocation>, string),
>

let asExpr = (e: contexted<'a, 'b>, ctx): 'a => e.expr(ctx)
let asStat = (e: contexted<'a, 'b>, ctx): 'b => e.stat(ctx)

let op1 = (s1, p1, s2) => Group(list{Print.string(s1), p1, Print.string(s2)})
let op2 = (s1, p1, s2, p2, s3) => Group(list{
  Print.string(s1),
  p1,
  Print.string(s2),
  p2,
  Print.string(s3),
})
let op3 = (s1, p1, s2, p2, s3, p3, s4) => Group(list{
  Print.string(s1),
  p1,
  Print.string(s2),
  p2,
  Print.string(s3),
  p3,
  Print.string(s4),
})

module JSPrinter: Printer = {
  let printName = x => {
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
    | Nil => raisePrintError("Lists are not supported in JavaScript.")
    | Num(n) => Float.toString(n)
    | Lgc(l) =>
      if l {
        "true"
      } else {
        "false"
      }
    | Str(s) => "\"" ++ String.escaped(s) ++ "\""
    | Sym(s) => s
    }
  }

  let listToString = es => {
    if es->List.some(e => containsNL(e.it)) {
      Group(list{
        Print.string("("),
        indentBlock(Print.dummyAnn(Print.concat(",\n", es)), 2),
        Print.string(")"),
      })
    } else {
      Group(list{Print.string("("), Print.dummyAnn(Print.concat(", ", es)), Print.string(")")})
    }
  }

  let defvarLike = (op, x, e) => {
    Group(list{Print.string(op), x, Print.string(" = "), indent(e, 2)})
  }

  let exprAppToString = (e, es) => {
    group2(e, listToString(es) |> Print.dummyAnn)
  }

  let consumeContext: consumer = e => {
    {
      expr: _ => surround("", e, ""),
      stat: ctx =>
        switch ctx {
        | Step => ("", e, ";")
        | Return => ("return ", e, ";") //`return ${e}`
        },
    }
  }

  let consumeContextWrap: consumer = e => {
    {
      expr: ctx =>
        if ctx {
          surround("(", e, ")")
        } else {
          consumeContext(e).expr(ctx)
        },
      stat: consumeContext(e).stat,
    }
  }

  let consumeContextVoid: consumer = e => {
    {
      stat: ctx =>
        switch ctx {
        | Return => ("", e, ";\nreturn;")
        | ctx => consumeContext(e).stat(ctx)
        },
      expr: consumeContext(e).expr,
    }
  }

  let consumeContextStat: consumer = e => {
    {
      expr: _ =>
        raisePrintError(`${e |> Print.toString} can't be used as a expression in JavaScript`),
      stat: consumeContextVoid(e).stat,
    }
  }

  let exprAppPrmToString = (p: Primitive.t, es: list<bool => expression<printAnn>>): annotated<
    _,
    contexted<Print.t<sourceLocation>, _>,
  > => {
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
          ann: Print.concat(` ${os} `, es->List.map(e => getPrint(e)))->consumeContextWrap,
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
          ann: op2("", getPrint(e1), ` ${os} `, getPrint(e2), "")->consumeContext,
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("[ ", getPrint(e1), ", ", getPrint(e2), " ]")->consumeContext,
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("", getPrint(e1), "[0]")->consumeContext,
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("", getPrint(e1), "[1]")->consumeContext,
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[0] = ", getPrint(e2), "")->consumeContextStat,
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[1] = ", getPrint(e2), "")->consumeContextStat,
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          ann: op1(
            "[ ",
            Print.concat(`, `, es->List.map(e => getPrint(e))) |> Print.dummyAnn,
            " ]",
          )->consumeContext,
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[", getPrint(e2), "]")->consumeContext,
          it: (VecRef, list{e1, e2}),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          // ann: `${getPrint(e1)}[${getPrint(e2)}] = ${getPrint(e3)}`->consumeContextStat,
          ann: op3(
            "",
            getPrint(e1),
            "[",
            getPrint(e2),
            "] = ",
            getPrint(e3),
            "",
          )->consumeContextStat,
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("", getPrint(e1), ".length")->consumeContext,
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("throw ", getPrint(e1), "")->consumeContextWrap,
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("! ", getPrint(e1), "")->consumeContextWrap,
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("console.log(", getPrint(e1), ")")->consumeContextVoid,
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("", getPrint(e1), ".next()")->consumeContextVoid,
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
    op2(`${op} `, exprAppToString(x, xs) |> Print.dummyAnn, " {", indentBlock(e, 2), "\n}")
    // `${op} ${exprAppToString(x, xs)} {${}\n}`
  }

  let defvarToString = (x, e) => {
    defvarLike("let ", x, e)
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
    funLike("function", Print.string(""), xs, b)
  }
  let exprGenToString = (xs, b) => {
    funLike("function*", Print.string(""), xs, b)
  }
  let exprYieldToString = e => op1("yield ", e, "")

  let exprBgnToString = (es, e) => {
    listToString(list{...es, e})
  }

  let exprCndToString = (ebs: list<(_, _)>, ob) => {
    let ebs = {
      switch ob {
      | None => ebs
      | Some(b) => list{...ebs, (Print.string(""), b)}
      }
    }
    let ebs =
      ebs->List.map(((e, b)) => op2("if (", e, ") {", indentBlock(b, 2), "\n}") |> Print.dummyAnn)
    Print.concat(" else ", ebs)
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    op3("", e_cnd, " ? ", e_thn, " : ", e_els, "")
    // `${e_cnd} ? ${e_thn} : ${e_els}`
  }

  let symbolToString = ({it, ann}) => {
    {
      it,
      ann: {
        sourceLocation: ann,
        print: Plain(printName(it)),
      },
    }
  }

  let rec printExp = ({it, ann: sourceLocation}) => {
    let lift = ({it, ann: print}, sourceLocation) => {
      {
        expr: ctx => {it, ann: {sourceLocation, print: print.expr(ctx)}},
        stat: ctx => {
          let (prefix, print, suffix) = print.stat(ctx)
          (prefix, {it, ann: {sourceLocation, print}}, suffix)
        },
      }
    }
    let e: sourceLocation => contexted<
      expression<printAnn>,
      (string, expression<printAnn>, string),
    > = switch it {
    | Con(c) =>
      {
        it: Con(c),
        ann: Plain(constantToString(c))->consumeContext,
      }->lift
    | Ref(x) =>
      {
        it: Ref(x),
        ann: Plain(x->printName)->consumeContext,
      }->lift
    | Set(x, e) =>
      {
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp->asExpr(false)
        {
          ann: exprSetToString(getPrint(x), getPrint(e))->consumeContextStat,
          it: Set(x, e),
        }
      }->lift
    | Lam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprLamToString(xs->List.map(x => getPrint(x)), getPrint(b))->consumeContextWrap,
          it: Lam(xs, b),
        }
      }->lift
    | GLam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprGenToString(xs->List.map(x => getPrint(x)), getPrint(b))->consumeContextWrap,
          it: GLam(xs, b),
        }
      }->lift
    | Yield(e) =>
      {
        let e = e->printExp->asExpr(false)
        {
          ann: exprYieldToString(getPrint(e))->consumeContextWrap,
          it: Yield(e),
        }
      }->lift
    | AppPrm(p, es) =>
      {
        let es = es->List.map((e, b) => e->printExp->asExpr(b))
        let {ann: print, it: (p, es)} = exprAppPrmToString(p, es)
        {
          it: AppPrm(p, es),
          ann: print,
        }
      }->lift
    | App(e, es) =>
      {
        let e = e->printExp->asExpr(true)
        let es = es->List.map(e => e->printExp->asExpr(false))
        {
          ann: exprAppToString(getPrint(e), es->List.map(e => getPrint(e)))->consumeContext,
          it: App(e, es),
        }
      }->lift
    | Let(_xes, _b) => raisePrintError("let-expressions are not supported by JavaScript")
    | Letrec(xes, b) =>
      sourceLocation => {
        expr: _ => raisePrintError("letrec-expressions are not supported by JavaScript"),
        stat: ctx => {
          let xes = xes->List.map(xeToString)
          let b = b->printBlock(ctx)
          let {it: print} = indentBlock(
            Print.concat(
              "\n",
              list{...xes->List.map(xe => getPrint(xe)), getPrint(b)},
            ) |> Print.dummyAnn,
            2,
          )
          (
            "{\n",
            {
              ann: {
                print,
                sourceLocation,
              },
              it: Letrec(xes, b),
            },
            "\n}",
          )
        },
      }
    | Cnd(ebs, ob) =>
      sourceLocation => {
        expr: _ =>
          raisePrintError(
            "Multi-armed conditionals in JavaScript is not supported by the translator yet.",
          ),
        stat: ctx => {
          let ebs: list<(expression<printAnn>, block<printAnn>)> =
            ebs->List.map(eb => eb->ebToString(ctx))
          let ob = ob->obToString(ctx)
          (
            "",
            {
              ann: {
                sourceLocation,
                print: exprCndToString(
                  ebs->List.map(((e, b)) => (getPrint(e), getPrint(b))),
                  ob->Option.map(b => getPrint(b)),
                ),
              },
              it: Cnd(ebs, ob),
            },
            "",
          )
        },
      }
    | If(e_cnd, e_thn, e_els) =>
      {
        let e_cnd = e_cnd->printExp->asExpr(true)
        let e_thn = e_thn->printExp->asExpr(true)
        let e_els = e_els->printExp->asExpr(true)
        {
          ann: exprIfToString(
            getPrint(e_cnd),
            getPrint(e_thn),
            getPrint(e_els),
          )->consumeContextWrap,
          it: If(e_cnd, e_thn, e_els),
        }
      }->lift
    | Bgn(es, e) =>
      {
        let es = es->List.map(e => e->printExp->asExpr(false))
        let e = e->printExp->asExpr(false)
        {
          ann: exprBgnToString(es->List.map(e => getPrint(e)), getPrint(e))->consumeContext,
          it: Bgn(es, e),
        }
      }->lift
    }
    e(sourceLocation)
  }
  and printDef = ({ann: sourceLocation, it: d}) => {
    let (prefix, d, suffix) = switch d {
    | Var(x, e) => {
        let x = x->symbolToString
        let e = e->printExp->asExpr(false)
        (
          "",
          {
            ann: defvarToString(getPrint(x), getPrint(e)),
            it: Var(x, e),
          },
          ";",
        )
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        (
          "",
          {
            ann: deffunToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
            it: Fun(f, xs, b),
          },
          "",
        )
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        (
          "",
          {
            ann: defgenToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
            it: GFun(f, xs, b),
          },
          "",
        )
      }
    }
    let {ann: print, it} = d
    (prefix, {ann: {print, sourceLocation}, it}, suffix)
  }
  and xeToString = ({it: xe, ann: sourceLocation}: bind<sourceLocation>): bind<printAnn> => {
    let (x, e) = xe
    let (x, e) = (symbolToString(x), e->printExp->asExpr(false))
    let print = defvarToString(getPrint(x), getPrint(e))
    {
      it: (x, e),
      ann: {
        print,
        sourceLocation,
      },
    }
  }
  and ebToString = (eb, ctx: statContext) => {
    let (e, b) = eb
    (e->printExp->asExpr(false), b->printBlock(ctx))
  }
  and obToString = (ob, ctx: statContext) => {
    ob->Option.map(b => b->printBlock(ctx))
  }
  and printBlock = ({ann, it: b}, context: statContext) => {
    switch b {
    | BRet(e) => {
        let (prefix, e, suffix) = printExp(e)->asStat(context)
        let print = Group(list{Print.string(prefix), getPrint(e), Print.string(suffix)})
        {
          ann: {print, sourceLocation: ann},
          it: BRet(e),
        }
      }
    | BCons(t, b) => {
        let (prefix, t, suffix) = printTerm(t, Step)
        let b = printBlock(b, context)
        let print = Group(list{
          Print.string(prefix),
          getPrint(t),
          Print.string(suffix),
          Print.string("\n"),
          getPrint(b),
        })
        {
          ann: {print, sourceLocation: ann},
          it: BCons(t, b),
        }
      }
    }
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>, ctx): (
    string,
    term<printAnn>,
    string,
  ) => {
    switch it {
    | Exp(it) => {
        let (prefix, it, suffix) = printExp({ann: sourceLocation, it})->asStat(ctx)
        let it = it |> mapAnn(it => Exp(it))
        (prefix, it, suffix)
      }
    | Def(it) => {
        let (prefix, it, suffix) = printDef({ann: sourceLocation, it})
        let it = it |> mapAnn(it => Def(it))
        (prefix, it, suffix)
      }
    }
  }

  let printOutputlet = o => {
    let rec p = (v: val): string => {
      switch v {
      | Ref(i) => `[Circular *${i |> Int.toString}]`
      | Con(c) => constantToString(c)
      | Struct(i, content) => {
          let i = switch i {
          | None => ""
          | Some(i) => `<ref *${i |> Int.toString}> `
          }
          let content = switch content {
          | Lst(_) => raisePrintError("Lists are not supported in JavaScript.")
          | Vec(es) => `[ ${String.concat(", ", es->List.map(p))} ]`
          }
          `${i}${content}`
        }
      }
    }
    switch o {
    | OErr => "error"
    | OVal(v) => p(v)
    }
  }

  let printOutput = (~sep=" ", os): string => {
    os->List.map(printOutputlet) |> String.concat(sep)
  }

  let printProgramFull = (insertPrintTopLevel, p) => {
    let p = if insertPrintTopLevel {
      insertTopLevelPrint(p)
    } else {
      p
    }
    let rec print = ({it, ann: sourceLocation}: program<sourceLocation>): program<printAnn> => {
      switch it {
      | PNil => {it: PNil, ann: {print: Group(list{}), sourceLocation}}
      | PCons(t, p) => {
          let (prefix, t, suffix) = printTerm(t, Step)
          switch p {
          | {it: PNil} => {
              it: PCons(
                t,
                {
                  it: PNil,
                  ann: {
                    print: Plain(""),
                    sourceLocation: {
                      begin: sourceLocation.end,
                      end: sourceLocation.end,
                    },
                  },
                },
              ),
              ann: {
                print: op1(prefix, getPrint(t), suffix),
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(
                    op1(prefix, getPrint(t), suffix) |> Print.dummyAnn,
                    "\n",
                    getPrint(p),
                  ),
                  sourceLocation,
                },
              }
            }
          }
        }
      }
    }
    print(p)
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print |> Print.toString
  }

  let printStandAloneTerm = ({it, ann}: term<sourceLocation>): string => {
    switch it {
    | Def(it) => {
        let (_, it, _) = printDef({it, ann})
        it.ann.print
      }
    | Exp(it) => {
        let (_, it, _) = printExp({it, ann})->asStat(Step)
        it.ann.print
      }
    } |> Print.toString
  }
}

module PYPrinter: Printer = {
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

  let printName = x => {
    let re = %re("/-/g")
    let matchFn = (_matchPart, _offset, _wholeString) => {
      "_"
    }
    Js.String2.unsafeReplaceBy0(x, re, matchFn)
  }

  let constantToString = c => {
    switch c {
    | Uni => "None"
    | Nil => raisePrintError("Lists are not supported in Python.")
    | Num(n) => Float.toString(n)
    | Lgc(l) =>
      if l {
        "True"
      } else {
        "False"
      }
    | Str(s) => "\"" ++ String.escaped(s) ++ "\""
    | Sym(s) => s
    }
  }

  let listToString = es => {
    if es->List.some(e => containsNL(e.it)) {
      Group(list{
        Print.string("("),
        indentBlock(Print.dummyAnn(Print.concat(",\n", es)), 4),
        Print.string(")"),
      })
    } else {
      Group(list{Print.string("("), Print.dummyAnn(Print.concat(", ", es)), Print.string(")")})
    }
  }

  let defvarLike = (op, x, e) => {
    Group(list{Print.string(op), x, Print.string(" = "), indent(e, 2)})
  }

  let exprAppToString = (e, es) => {
    group2(e, listToString(es) |> Print.dummyAnn)
  }

  let consumeContext: consumer = e => {
    {
      expr: _ => surround("", e, ""),
      stat: ctx =>
        switch ctx {
        | Step => ("", e, "")
        | Return => ("return ", e, "")
        },
    }
  }

  let consumeContextWrap: consumer = e => {
    {
      expr: ctx =>
        if ctx {
          surround("(", e, ")")
        } else {
          consumeContext(e).expr(ctx)
        },
      stat: consumeContext(e).stat,
    }
  }

  let consumeContextVoid: consumer = e => {
    {
      stat: ctx =>
        switch ctx {
        | Return => ("", e, "\nreturn")
        | ctx => consumeContext(e).stat(ctx)
        },
      expr: consumeContext(e).expr,
    }
  }

  let consumeContextStat: consumer = e => {
    {
      expr: _ => raisePrintError(`${e |> Print.toString} can't be used as a expression in Python`),
      stat: consumeContextVoid(e).stat,
    }
  }

  let exprAppPrmToString = (p: Primitive.t, es: list<bool => expression<printAnn>>): annotated<
    _,
    contexted<Print.t<sourceLocation>, _>,
  > => {
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
          ann: Print.concat(` ${os} `, es->List.map(e => getPrint(e)))->consumeContextWrap,
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
          ann: op2("", getPrint(e1), ` ${os} `, getPrint(e2), "")->consumeContext,
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("[ ", getPrint(e1), ", ", getPrint(e2), " ]")->consumeContext,
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("", getPrint(e1), "[0]")->consumeContext,
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("", getPrint(e1), "[1]")->consumeContext,
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[0] = ", getPrint(e2), "")->consumeContextStat,
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[1] = ", getPrint(e2), "")->consumeContextStat,
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          ann: op1(
            "[ ",
            Print.concat(`, `, es->List.map(e => getPrint(e))) |> Print.dummyAnn,
            " ]",
          )->consumeContext,
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: op2("", getPrint(e1), "[", getPrint(e2), "]")->consumeContext,
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
          )->consumeContextStat,
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("len(", getPrint(e1), ")")->consumeContext,
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("raise ", getPrint(e1), "")->consumeContextWrap,
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: op1("not ", getPrint(e1), "")->consumeContextWrap,
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("print(", getPrint(e1), ")")->consumeContextVoid,
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: op1("next(", getPrint(e1), ")")->consumeContextVoid,
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
    op2(`${op} `, exprAppToString(x, xs) |> Print.dummyAnn, ":", indentBlock(e, 4), "")
    // `${op} ${exprAppToString(x, xs)} {${}\n}`
  }

  let defvarToString = (x, e) => {
    defvarLike("", x, e)
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
    op2("lambda ", xs, ": ", b, "")
  }
  let exprGenToString = exprLamToString
  let exprYieldToString = e => op1("yield ", e, "")

  let exprCndToString = (ebs: list<(_, _)>, ob) => {
    let ebs = {
      switch ob {
      | None => ebs
      | Some(b) => list{...ebs, (Print.string("se:"), b)}
      }
    }
    let ebs =
      ebs->List.map(((e, b)) => op2("if ", e, ":", indentBlock(b, 4), "\n") |> Print.dummyAnn)
    Print.concat(" el", ebs)
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    op3("", e_thn, " if ", e_cnd, " else ", e_els, "")
  }

  let symbolToString = ({it, ann}) => {
    {
      it,
      ann: {
        sourceLocation: ann,
        print: Plain(printName(it)),
      },
    }
  }

  let rec printExp = ({it, ann: sourceLocation}, env) => {
    let lift = ({it, ann: print}, sourceLocation) => {
      {
        expr: ctx => {it, ann: {sourceLocation, print: print.expr(ctx)}},
        stat: ctx => {
          let (prefix, print, suffix) = print.stat(ctx)
          (prefix, {it, ann: {sourceLocation, print}}, suffix)
        },
      }
    }
    let e: sourceLocation => contexted<
      expression<printAnn>,
      (string, expression<printAnn>, string),
    > = switch it {
    | Con(c) =>
      {
        it: Con(c),
        ann: Plain(constantToString(c))->consumeContext,
      }->lift
    | Ref(x) =>
      {
        it: Ref(x),
        ann: Plain(x->printName)->consumeContext,
      }->lift
    | Set(x, e) =>
      {
        refMut(env, x.it)
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp(env)->asExpr(false)
        {
          ann: exprSetToString(getPrint(x), getPrint(e))->consumeContextStat,
          it: Set(x, e),
        }
      }->lift
    | Lam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printLamBody(xs, env)
        {
          ann: exprLamToString(
            Print.concat(",", xs->List.map(getPrint))->Print.dummyAnn,
            getPrint(b),
          )->consumeContextWrap,
          it: Lam(xs, b),
        }
      }->lift
    | GLam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printLamBody(xs, env)
        {
          ann: exprGenToString(
            Print.concat(",", xs->List.map(getPrint))->Print.dummyAnn,
            getPrint(b),
          )->consumeContextWrap,
          it: GLam(xs, b),
        }
      }->lift
    | Yield(e) =>
      {
        let e = e->printExp(env)->asExpr(false)
        {
          ann: exprYieldToString(getPrint(e))->consumeContextWrap,
          it: Yield(e),
        }
      }->lift
    | AppPrm(p, es) =>
      {
        let es = es->List.map((e, b) => e->printExp(env)->asExpr(b))
        let {ann: print, it: (p, es)} = exprAppPrmToString(p, es)
        {
          it: AppPrm(p, es),
          ann: print,
        }
      }->lift
    | App(e, es) =>
      {
        let e = e->printExp(env)->asExpr(true)
        let es = es->List.map(e => e->printExp(env)->asExpr(false))
        {
          ann: exprAppToString(getPrint(e), es->List.map(e => getPrint(e)))->consumeContext,
          it: App(e, es),
        }
      }->lift
    | Let(_xes, _b) => raisePrintError("let-expressions are not supported by Python")
    | Letrec(_xes, _b) => raisePrintError("letrec-expressions are not supported by Python")
    | Cnd(ebs, ob) =>
      sourceLocation => {
        expr: _ =>
          raisePrintError(
            "Multi-armed conditionals in Python is not supported by the translator yet.",
          ),
        stat: ctx => {
          let ebs: list<(expression<printAnn>, block<printAnn>)> =
            ebs->List.map(eb => eb->ebToString(ctx, env))
          let ob = ob->obToString(ctx, env)
          (
            "",
            {
              ann: {
                sourceLocation,
                print: exprCndToString(
                  ebs->List.map(((e, b)) => (getPrint(e), getPrint(b))),
                  ob->Option.map(b => getPrint(b)),
                ),
              },
              it: Cnd(ebs, ob),
            },
            "",
          )
        },
      }
    | If(e_cnd, e_thn, e_els) =>
      {
        let e_cnd = e_cnd->printExp(env)->asExpr(true)
        let e_thn = e_thn->printExp(env)->asExpr(true)
        let e_els = e_els->printExp(env)->asExpr(true)
        {
          ann: exprIfToString(
            getPrint(e_cnd),
            getPrint(e_thn),
            getPrint(e_els),
          )->consumeContextWrap,
          it: If(e_cnd, e_thn, e_els),
        }
      }->lift
    | Bgn(_es, _e) => raisePrintError("`begin` expressions are not supported by Python")
    }
    e(sourceLocation)
  }
  and printDef = ({ann: sourceLocation, it: d}, env) => {
    let (prefix, d, suffix) = switch d {
    | Var(x, e) => {
        let x = x->symbolToString
        let e = e->printExp(env)->asExpr(false)
        (
          "",
          {
            ann: defvarToString(getPrint(x), getPrint(e)),
            it: Var(x, e),
          },
          "",
        )
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printDefBody(xs, env)
        (
          "",
          {
            ann: deffunToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
            it: Fun(f, xs, b),
          },
          "",
        )
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printDefBody(xs, env)
        (
          "",
          {
            ann: defgenToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
            it: GFun(f, xs, b),
          },
          "",
        )
      }
    }
    let {ann: print, it} = d
    (prefix, {ann: {print, sourceLocation}, it}, suffix)
  }
  and ebToString = (eb, ctx: statContext, env) => {
    let (e, b) = eb
    (e->printExp(env)->asExpr(false), b->printBlock(env)->asStat(ctx))
  }
  and obToString = (ob, ctx: statContext, env) => {
    ob->Option.map(b => b->printBlock(env)->asStat(ctx))
  }
  and printBlockHelper = ({ann, it: b}, env, context) => {
    switch b {
    | BRet(e) => {
        let (prefix, e, suffix) = e->printExp(env)->asStat(context)
        let print = Group(list{Print.string(prefix), getPrint(e), Print.string(suffix)})
        {
          ann: {print, sourceLocation: ann},
          it: BRet(e),
        }
      }
    | BCons(t, b) => {
        let (prefix, t, suffix) = printTerm(t, env, Step)
        let b = b->printBlock(env)->asStat(context)
        let print = Group(list{
          Print.string(prefix),
          getPrint(t),
          Print.string(suffix),
          Print.string("\n"),
          getPrint(b),
        })
        {
          ann: {print, sourceLocation: ann},
          it: BCons(t, b),
        }
      }
    }
  }
  and printBlock = (b, env): contexted1<block<printAnn>> => {
    {
      expr: ctx => {
        switch b.it {
        | BRet(e) => {
            let {it, ann} = e->printExp(env)->asExpr(ctx)
            {
              it: BRet({it, ann}),
              ann,
            }
          }
        | _ => raisePrintError("Python blocks can't be used as expressions in general")
        }
      },
      stat: ctx => {
        let xs = xsOfBlock(b)
        if xs != list{} {
          raisePrintError("Python blocks can't declare local variables")
        }
        printBlockHelper(b, env, ctx)
      },
    }
  }
  and printLamBody = (b, args, env): block<printAnn> => {
    switch b.it {
    | BRet(e) => {
        let args: list<string> = args->List.map(x => x.it)
        let (_refs, env) = extend(args, env)
        // todo: there should be some check after we support set! expression (#30)
        let {it, ann} = e->printExp(env)->asExpr(false)
        {
          it: BRet({it, ann}),
          ann,
        }
      }
    | _ => raisePrintError("In Python, a lambda body must contain exactly one expression")
    }
  }
  and printDefBody = (b, args, env): block<printAnn> => {
    let locs: list<string> = xsOfBlock(b)->List.map(x => x.it)
    let args: list<string> = args->List.map(x => x.it)
    let (refs, env) = extend(List.concat(locs, args), env)
    let b = printBlockHelper(b, env, Return)
    let print = Print.concat(
      "\n",
      list{
        ...refs
        ->HashMap.String.toArray
        ->Js.Array2.map(((x, r)) => Print.string(`${r->RefDecl.toString} ${x}`))
        ->List.fromArray,
        getPrint(b),
      },
    )
    {
      ann: {print, sourceLocation: b.ann.sourceLocation},
      it: b.it,
    }
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>, env, ctx) => {
    switch it {
    | Exp(it) => {
        let (prefix, it, suffix) = printExp({ann: sourceLocation, it}, env)->asStat(ctx)
        let it = it |> mapAnn(it => Exp(it))
        (prefix, it, suffix)
      }
    | Def(it) => {
        let (prefix, it, suffix) = printDef({ann: sourceLocation, it}, env)
        let it = it |> mapAnn(it => Def(it))
        (prefix, it, suffix)
      }
    }
  }

  let printOutputlet = o => {
    let rec p = (v: val): string => {
      switch v {
      | Ref(_) => `[...]`
      | Con(c) => constantToString(c)
      | Struct(i, content) => {
          let i = switch i {
          | None => ""
          | Some(_) => ""
          }
          let content = switch content {
          | Lst(_) => raisePrintError("Lists are not supported in Python.")
          | Vec(es) => `[${String.concat(", ", es->List.map(p))}]`
          }
          `${i}${content}`
        }
      }
    }
    switch o {
    | OErr => "error"
    | OVal(v) => p(v)
    }
  }

  let printOutput = (~sep=" ", os): string => {
    os->List.map(printOutputlet) |> String.concat(sep)
  }

  let printProgramFull = (insertPrintTopLevel, p) => {
    let p = if insertPrintTopLevel {
      insertTopLevelPrint(p)
    } else {
      p
    }
    let xs = xsOfProgram(p)
    let env = G(xs->List.map(x => x.it)->List.toArray->HashSet.String.fromArray)
    let rec print = ({it, ann: sourceLocation}: program<sourceLocation>): program<printAnn> => {
      switch it {
      | PNil => {it: PNil, ann: {print: Group(list{}), sourceLocation}}
      | PCons(t, p) => {
          let (prefix, t, suffix) = printTerm(t, env, Step)
          switch p {
          | {it: PNil} => {
              it: PCons(
                t,
                {
                  it: PNil,
                  ann: {
                    print: Plain(""),
                    sourceLocation: {
                      begin: sourceLocation.end,
                      end: sourceLocation.end,
                    },
                  },
                },
              ),
              ann: {
                print: op1(prefix, getPrint(t), suffix),
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(
                    op1(prefix, getPrint(t), suffix) |> Print.dummyAnn,
                    "\n",
                    getPrint(p),
                  ),
                  sourceLocation,
                },
              }
            }
          }
        }
      }
    }
    print(p)
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print |> Print.toString
  }

  let printStandAloneTerm = ({it, ann}: term<sourceLocation>): string => {
    let globalEnv = G(HashSet.String.fromArray([]))
    switch it {
    | Def(it) => {
        let (_, it, _) = printDef({it, ann}, globalEnv)
        it.ann.print
      }
    | Exp(it) => {
        let (_, it, _) = printExp({it, ann}, globalEnv)->asStat(Step)
        it.ann.print
      }
    } |> Print.toString
  }
}

// module PCPrinter: Printer = {
//   let printName = x => {
//     x
//     // let re = %re("/-./g")
//     // let matchFn = (matchPart, _offset, _wholeString) => {
//     //   Js.String2.toUpperCase(Js.String2.substringToEnd(matchPart, ~from=1))
//     // }
//     // let x = Js.String2.unsafeReplaceBy0(x, re, matchFn)

//     // // add `$` to the beginning of reserved words
//     // if x == "var" {
//     //   "$var"
//     // } else {
//     //   x
//     // }
//   }

//   let constantToString = c => {
//     switch c {
//     | Uni => "null"
//     | Nil => "list[]"
//     | Num(n) => Float.toString(n)
//     | Lgc(l) =>
//       if l {
//         "true"
//       } else {
//         "false"
//       }
//     | Str(s) => "\"" ++ String.escaped(s) ++ "\""
//     | Sym(s) => s
//     }
//   }

//   let listToString = es => {
//     if es->List.some(e => containsNL(e.it)) {
//       Group(list{
//         Print.string("("),
//         indentBlock(Print.dummyAnn(Print.concat(",\n", es)), 2),
//         Print.string(")"),
//       })
//     } else {
//       Group(list{Print.string("("), Print.dummyAnn(Print.concat(", ", es)), Print.string(")")})
//     }
//   }

//   let defvarLike = (op, x, e) => {
//     group(list{Print.string(op), x, Print.string(" = "), indent(e, 2)})
//   }

//   let exprAppToString = (e, es) => {
//     group2(e, listToString(es) |> Print.dummyAnn)
//   }

//   let consumeContext = (e: annotated<_, _>, context) => {
//     switch context {
//     | Expr(_) => surround("", e, "")
//     | Stat(ctx) =>
//       switch ctx {
//       | Step => surround("", e, "")
//       | Return => surround("return ", e, "")
//       }
//     }
//   }

//   let consumeContextWrap = (e: annotated<_, _>, context: context) => {
//     switch context {
//     | Expr(true) => surround("(", e, ")")
//     | _ => consumeContext(e, context)
//     }
//   }

//   let consumeContextVoid = (e: annotated<_, _>, context) => {
//     switch context {
//     | Stat(Return) => surround("", e, "\nreturn")
//     | _ => consumeContext(e, context)
//     }
//   }

//   let consumeContextWrapVoid = (e: annotated<_, _>, context) => {
//     switch context {
//     | Expr(true) => surround("(", e, ")")
//     | Stat(Return) => surround("", e, "\nreturn")
//     | _ => consumeContext(e, context)
//     }
//   }

//   let exprAppPrmToString = (
//     p: Primitive.t,
//     es: list<bool => expression<printAnn>>,
//     context: context,
//   ): annotated<_, Print.t<sourceLocation>> => {
//     switch (p, es) {
//     | (Arith(o), es) => {
//         let os = switch o {
//         | Add => "+"
//         | Sub => "-"
//         | Mul => "*"
//         | Div => "/"
//         }
//         let es = es->List.map(e => e(true))
//         {
//           ann: Print.concat(` ${os} `, es->List.map(e => getPrint(e)))
//           ->Print.dummyAnn
//           ->consumeContextWrap(context),
//           it: (Arith(o), es),
//         }
//       }
//     | (Cmp(o), list{e1, e2}) => {
//         let os = switch o {
//         | Lt => "<"
//         | Eq => "=="
//         | Gt => ">"
//         | Le => "<="
//         | Ge => ">="
//         | Ne => "!="
//         }
//         let e1 = e1(true)
//         let e2 = e2(true)
//         {
//           ann: op2("", getPrint(e1), ` ${os} `, getPrint(e2), "")->consumeContextWrap(context),
//           it: (Cmp(o), list{e1, e2}),
//         }
//       }
//     | (PairNew, list{e1, e2}) => {
//         let e1 = e1(false)
//         let e2 = e2(false)
//         {
//           ann: op2("vec[", getPrint(e1), ", ", getPrint(e2), "]")->consumeContext(context),
//           it: (PairNew, list{e1, e2}),
//         }
//       }
//     | (PairRefLeft, list{e1}) => {
//         let e1 = e1(true)
//         {
//           ann: op1("", getPrint(e1), "[0]")->consumeContext(context),
//           it: (PairRefLeft, list{e1}),
//         }
//       }
//     | (PairRefRight, list{e1}) => {
//         let e1 = e1(true)
//         {
//           ann: op1("", getPrint(e1), "[1]")->consumeContext(context),
//           it: (PairRefRight, list{e1}),
//         }
//       }
//     | (PairSetLeft, list{e1, e2}) => {
//         let e1 = e1(false)
//         let e2 = e2(false)
//         {
//           ann: op2("", getPrint(e1), "[0] = ", getPrint(e2), "")->consumeContextWrapVoid(context),
//           it: (PairSetLeft, list{e1, e2}),
//         }
//       }
//     | (PairSetRight, list{e1, e2}) => {
//         let e1 = e1(false)
//         let e2 = e2(false)
//         {
//           ann: op2("", getPrint(e1), "[1] = ", getPrint(e2), "")->consumeContextWrapVoid(context),
//           it: (PairSetRight, list{e1, e2}),
//         }
//       }
//     | (VecNew, es) => {
//         let es = es->List.map(e => e(false))
//         {
//           ann: op1(
//             "vec[",
//             Print.concat(`, `, es->List.map(e => getPrint(e))) |> Print.dummyAnn,
//             "]",
//           )->consumeContext(context),
//           it: (VecNew, es),
//         }
//       }
//     | (VecRef, list{e1, e2}) => {
//         let e1 = e1(true)
//         let e2 = e2(false)
//         {
//           ann: op2("", getPrint(e1), "[", getPrint(e2), "]")->consumeContext(context),
//           it: (VecRef, list{e1, e2}),
//         }
//       }
//     | (VecSet, list{e1, e2, e3}) => {
//         let e1 = e1(true)
//         let e2 = e2(false)
//         let e3 = e3(false)
//         {
//           ann: op3(
//             "",
//             getPrint(e1),
//             "[",
//             getPrint(e2),
//             "] = ",
//             getPrint(e3),
//             "",
//           )->consumeContextWrapVoid(context),
//           it: (VecSet, list{e1, e2, e3}),
//         }
//       }
//     | (VecLen, list{e1}) => {
//         let e1 = e1(false)
//         {
//           ann: op1("", getPrint(e1), ".length")->consumeContext(context),
//           it: (VecLen, list{e1}),
//         }
//       }
//     | (Err, list{e1}) => {
//         let e1 = e1(true)
//         {
//           ann: op1("throw ", getPrint(e1), "")->consumeContextWrap(context),
//           it: (Err, list{e1}),
//         }
//       }
//     | (Not, list{e1}) => {
//         let e1 = e1(true)
//         {
//           ann: op1("¬ ", getPrint(e1), "")->consumeContextWrap(context),
//           it: (Not, list{e1}),
//         }
//       }
//     | (Print, list{e1}) => {
//         let e1 = e1(false)
//         {
//           ann: op1("print(", getPrint(e1), ")")->consumeContextVoid(context),
//           it: (Print, list{e1}),
//         }
//       }
//     | (Next, list{e1}) => {
//         let e1 = e1(false)
//         {
//           ann: op1("next(", getPrint(e1), ")")->consumeContextVoid(context),
//           it: (Next, list{e1}),
//         }
//       }
//     | (Cons, _) => raisePrintError("List is not supported by the pseudo-code syntax")
//     | _ =>
//       raisePrintError(
//         `Our pseudo-code syntax doesn't let you use ${Primitive.toString(p)} on ${List.length(
//             es,
//           ) |> Int.toString} parameter(s).`,
//       )
//     }
//   }

//   let funLike = (op, x, xs, e) => {
//     op2(`${op} `, exprAppToString(x, xs), ":", indentBlock(e, 2), "\nend")
//     // `${op} ${exprAppToString(x, xs)} {${}\n}`
//   }

//   let defvarToString = (x, e) => {
//     op1("", defvarLike("let ", x, e), "")
//   }

//   let deffunToString = (f, xs, b) => {
//     funLike("fun", f, xs, b)
//   }

//   let defgenToString = (f, xs, b) => {
//     funLike("gen fun", f, xs, b)
//   }

//   let exprSetToString = (x, e) => {
//     op2("", x, " = ", e, "")
//   }

//   let exprLamToString = (xs, b) => {
//     funLike("lam", Print.string(""), xs, b)
//   }
//   let exprGenToString = (xs, b) => {
//     funLike("gen lam", Print.string(""), xs, b)
//   }
//   let exprYieldToString = e => op1("yield ", e, "")

//   let exprBgnToString = (es, e) => {
//     listToString(list{...es, e}) |> Print.dummyAnn
//   }

//   let exprCndToString = (ebs: list<(_, _)>, ob) => {
//     let ebs = {
//       switch ob {
//       | None => ebs
//       | Some(b) => list{...ebs, (Print.string(""), b)}
//       }
//     }
//     let ebs = ebs->List.map(((e, b)) => op2("if ", e, ":", indentBlock(b, 2), "\nend"))
//     Print.concat(" else ", ebs)
//   }

//   let exprIfToString = (e_cnd, e_thn, e_els) => {
//     op3("if ", e_cnd, ": ", e_thn, " else: ", e_els, " end")
//     // `${e_cnd} ? ${e_thn} : ${e_els}`
//   }

//   let symbolToString = ({it, ann}) => {
//     {
//       it,
//       ann: {
//         sourceLocation: ann,
//         print: Plain(it->printName),
//       },
//     }
//   }

//   let rec printExp = ({it, ann: sourceLocation}, context) => {
//     let e: annotated<expressionNode<printAnn>, Print.t<sourceLocation>> = switch it {
//     | Con(c) => {
//         it: Con(c),
//         ann: Print.string(constantToString(c))->consumeContext(context),
//       }
//     | Ref(x) => {
//         it: Ref(x),
//         ann: Print.string(x->printName)->consumeContext(context),
//       }
//     | Set(x, e) => {
//         let x = symbolToString(x)
//         let e: expression<printAnn> = e->printExp->asExpr(false)
//         {
//           ann: exprSetToString(getPrint(x), getPrint(e))->consumeContextWrapVoid(context),
//           it: Set(x, e),
//         }
//       }
//     | Lam(xs, b) => {
//         let xs = xs->List.map(symbolToString)
//         let b = b->printBlock(Return)
//         {
//           ann: exprLamToString(xs->List.map(x => getPrint(x)), getPrint(b))->consumeContextWrap(
//             context,
//           ),
//           it: Lam(xs, b),
//         }
//       }
//     | GLam(xs, b) => {
//         let xs = xs->List.map(symbolToString)
//         let b = b->printBlock(Return)
//         {
//           ann: exprGenToString(xs->List.map(x => getPrint(x)), getPrint(b))->consumeContextWrap(
//             context,
//           ),
//           it: Lam(xs, b),
//         }
//       }
//     | Yield(e) => {
//         let e = e->printExp->asExpr(false)
//         {
//           ann: exprYieldToString(getPrint(e))->consumeContextWrap(context),
//           it: Yield(e),
//         }
//       }
//     | AppPrm(p, es) => {
//         let es = es->List.map((e, b) => e->printExp->asExpr(b))
//         let {ann: print, it: (p, es)} = exprAppPrmToString(p, es, context)
//         {
//           it: AppPrm(p, es),
//           ann: print,
//         }
//       }
//     | App(e, es) => {
//         let e = e->printExp->asExpr(true)
//         let es = es->List.map(e => e->printExp->asExpr(false))
//         {
//           ann: exprAppToString(getPrint(e), es->List.map(e => getPrint(e)))->consumeContext(
//             context,
//           ),
//           it: App(e, es),
//         }
//       }
//     | Let(_xes, _b) =>
//       raisePrintError("let-expressions are not supported by our pseudo-code syntax")
//     | Letrec(xes, b) =>
//       switch context {
//       | Expr(_) => raisePrintError("letrec-expressions are not supported by our pseudo-code syntax")
//       | Stat(ctx) => {
//           let xes = xes->List.map(xeToString)
//           let b = b->printBlock(ctx)
//           {
//             ann: Group(list{
//               Print.string("{\n"),
//               indentBlock(
//                 Print.concat(
//                   "\n",
//                   list{...xes->List.map(xe => getPrint(xe)), getPrint(b)},
//                 ) |> Print.dummyAnn,
//                 2,
//               ),
//               Print.string("\n}"),
//             }),
//             it: Letrec(xes, b),
//           }
//         }
//       }
//     | Cnd(ebs, ob) =>
//       switch context {
//       | Expr(_) =>
//         raisePrintError(
//           "Multi-armed conditionals in our pseudo-code syntax is not supported by the translator yet.",
//         )
//       | Stat(context) => {
//           let ebs: list<(expression<printAnn>, block<printAnn>)> =
//             ebs->List.map(eb => eb->ebToString(context))
//           let ob = ob->obToString(context)
//           {
//             ann: exprCndToString(
//               ebs->List.map(((e, b)) => (getPrint(e), getPrint(b))),
//               ob->Option.map(b => getPrint(b)),
//             ),
//             it: Cnd(ebs, ob),
//           }
//         }
//       }
//     | If(e_cnd, e_thn, e_els) => {
//         let e_cnd = e_cnd->printExp->asExpr(true)
//         let e_thn = e_thn->printExp->asExpr(true)
//         let e_els = e_els->printExp->asExpr(true)
//         {
//           ann: exprIfToString(
//             getPrint(e_cnd),
//             getPrint(e_thn),
//             getPrint(e_els),
//           )->consumeContextWrap(context),
//           it: If(e_cnd, e_thn, e_els),
//         }
//       }
//     | Bgn(es, e) => {
//         let es = es->List.map(e => e->printExp->asExpr(false))
//         let e = e->printExp->asExpr(false)
//         {
//           ann: exprBgnToString(es->List.map(e => getPrint(e)), getPrint(e))->consumeContext(
//             context,
//           ),
//           it: Bgn(es, e),
//         }
//       }
//     }
//     let {ann: print, it} = e
//     {ann: {print, sourceLocation}, it}
//   }
//   and defToString = ({ann: sourceLocation, it: d}: definition<sourceLocation>): definition<
//     printAnn,
//   > => {
//     let d = switch d {
//     | Var(x, e) => {
//         let x = x->symbolToString
//         let e = e->printExp->asExpr(false)
//         {
//           ann: defvarToString(getPrint(x), getPrint(e)),
//           it: Var(x, e),
//         }
//       }
//     | Fun(f, xs, b) => {
//         let f = f->symbolToString
//         let xs = xs->List.map(symbolToString)
//         let b = b->printBlock(Return)
//         {
//           ann: deffunToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
//           it: Fun(f, xs, b),
//         }
//       }
//     | GFun(f, xs, b) => {
//         let f = f->symbolToString
//         let xs = xs->List.map(symbolToString)
//         let b = b->printBlock(Return)
//         {
//           ann: defgenToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
//           it: GFun(f, xs, b),
//         }
//       }
//     }
//     let {ann: print, it} = d
//     {ann: {print: print.it, sourceLocation}, it}
//   }
//   and xeToString = ({it: xe, ann: sourceLocation}: bind<sourceLocation>): bind<printAnn> => {
//     let (x, e) = xe
//     let (x, e) = (symbolToString(x), e->printExp->asExpr(false))
//     let print = defvarToString(getPrint(x), getPrint(e)).it
//     {
//       it: (x, e),
//       ann: {
//         print,
//         sourceLocation,
//       },
//     }
//   }
//   and ebToString = (eb, ctx: statContext) => {
//     let (e, b) = eb
//     (e->printExp->asExpr(false), b->printBlock(ctx))
//   }
//   and obToString = (ob, ctx: statContext) => {
//     ob->Option.map(b => b->printBlock(ctx))
//   }
//   and printBlock = ({ann: sourceLocation, it: b}, context: statContext) => {
//     switch b {
//     | BRet(e) => printExp({it: e, ann: sourceLocation}, Stat(context)) |> mapAnn(e => BRet(e))
//     | BCons(t, b) => {
//         let t = printTerm(t, Step)
//         let b = printBlock(b, context)
//         let print = Group(list{getPrint(t), Print.string("\n"), getPrint(b)})
//         {
//           ann: {print, sourceLocation},
//           it: BCons(t, b),
//         }
//       }
//     }
//   }
//   and printTerm = ({ann: sourceLocation, it: t}: term<sourceLocation>, ctx): term<printAnn> => {
//     switch t {
//     | Exp(it) => printExp({ann: sourceLocation, it}, Stat(ctx)) |> mapAnn(v => Exp(v))
//     | Def(it) => defToString({ann: sourceLocation, it}) |> mapAnn(v => Def(v))
//     }
//   }

//   let printOutputlet = (o): string => {
//     let rec p = (v: val): string => {
//       switch v {
//       | Ref(i) => `#${i |> Int.toString}#`
//       | Con(c) => constantToString(c)
//       | Struct(i, content) => {
//           let i = switch i {
//           | None => ""
//           | Some(i) => `#${i |> Int.toString}=`
//           }
//           let content = switch content {
//           | Lst(es) => `list[${String.concat(", ", es->List.map(p))}]`
//           | Vec(es) => `vec[${String.concat(", ", es->List.map(p))}]`
//           }
//           `${i}${content}`
//         }
//       }
//     }
//     switch o {
//     | OErr => "error"
//     | OVal(v) => p(v)
//     }
//   }

//   let printOutput = (~sep=" ", os): string => {
//     os->List.map(printOutputlet) |> String.concat(sep)
//   }

//   let printProgramFull = (insertPrintTopLevel, p) => {
//     let p = if insertPrintTopLevel {
//       insertTopLevelPrint(p)
//     } else {
//       p
//     }
//     let rec print = ({it, ann: sourceLocation}: program<sourceLocation>): program<printAnn> => {
//       switch it {
//       | PNil => {it: PNil, ann: {print: Group(list{}), sourceLocation}}
//       | PCons(t, p) => {
//           let t = printTerm(t, Step)
//           switch p {
//           | {it: PNil} => {
//               it: PCons(
//                 t,
//                 {
//                   it: PNil,
//                   ann: {
//                     print: Plain(""),
//                     sourceLocation: {
//                       begin: sourceLocation.end,
//                       end: sourceLocation.end,
//                     },
//                   },
//                 },
//               ),
//               ann: {
//                 print: getPrint(t).it,
//                 sourceLocation,
//               },
//             }
//           | _ => {
//               let p = print(p)
//               {
//                 it: PCons(t, p),
//                 ann: {
//                   print: Print.concat2(getPrint(t), "\n", getPrint(p)),
//                   sourceLocation,
//                 },
//               }
//             }
//           }
//         }
//       }
//     }
//     print(p)
//   }

//   let printProgram = (insertPrintTopLevel, p) => {
//     printProgramFull(insertPrintTopLevel, p).ann.print |> Print.toString
//   }

//   let printStandAloneTerm = ({it, ann}: term<sourceLocation>): string => {
//     switch it {
//     | Def(it) => defToString({it, ann}).ann.print
//     | Exp(it) => printExp({it, ann}, Stat(Step)).ann.print
//     } |> Print.toString
//   }
// }

// module SCPrinter: Printer = {
//   let printName = x => {
//     let re = %re("/-./g")
//     let matchFn = (matchPart, _offset, _wholeString) => {
//       Js.String2.toUpperCase(Js.String2.substringToEnd(matchPart, ~from=1))
//     }
//     let x = Js.String2.unsafeReplaceBy0(x, re, matchFn)

//     // add `$` to the beginning of reserved words
//     if x == "var" {
//       "$var"
//     } else {
//       x
//     }
//   }

//   let constantToString = c => {
//     switch c {
//     | Uni => "null"
//     | Nil => raisePrintError("Lists are not supported in Scala.")
//     | Num(n) => Float.toString(n)
//     | Lgc(l) =>
//       if l {
//         "true"
//       } else {
//         "false"
//       }
//     | Str(s) => "\"" ++ String.escaped(s) ++ "\""
//     | Sym(s) => s
//     }
//   }

//   let listToString = es => {
//     if es->List.some(e => containsNL(e.it)) {
//       Group(list{
//         Print.string("("),
//         indentBlock(Print.dummyAnn(Print.concat(",\n", es)), 2),
//         Print.string(")"),
//       })
//     } else {
//       Group(list{Print.string("("), Print.dummyAnn(Print.concat(", ", es)), Print.string(")")})
//     }
//   }

//   let defvarLike = (op, x, e) => {
//     group(list{Print.string(op), x, Print.string(" = "), indent(e, 2)})
//   }

//   let exprAppToString = (e, es) => {
//     group2(
//       e,
//       if es == list{} {
//         Print.string("")
//       } else {
//         listToString(es) |> Print.dummyAnn
//       },
//     )
//   }

//   let containsVarMutation = ref(false)
//   let containsVecMutation = ref(false)

//   let consumeContext = (e: annotated<_, _>, context) => {
//     switch context {
//     | Expr(_) => surround("", e, "")
//     | Stat(ctx) =>
//       switch ctx {
//       | Step => surround("", e, "")
//       | Return => surround("", e, "")
//       }
//     }
//   }

//   let consumeContextWrap = (e: annotated<_, _>, context: context) => {
//     switch context {
//     | Expr(true) => surround("(", e, ")")
//     | _ => consumeContext(e, context)
//     }
//   }

//   let consumeContextVoid = (e: annotated<_, _>, context) => {
//     switch context {
//     | Stat(Return) => surround("", e, "")
//     | _ => consumeContext(e, context)
//     }
//   }

//   let exprAppPrmToString = (
//     p: Primitive.t,
//     es: list<bool => expression<printAnn>>,
//     context: context,
//   ): annotated<_, Print.t<sourceLocation>> => {
//     switch (p, es) {
//     | (Arith(o), es) => {
//         let os = switch o {
//         | Add => "+"
//         | Sub => "-"
//         | Mul => "*"
//         | Div => "/"
//         }
//         let es = es->List.map(e => e(true))
//         {
//           ann: Print.concat(` ${os} `, es->List.map(e => getPrint(e)))
//           ->Print.dummyAnn
//           ->consumeContextWrap(context),
//           it: (Arith(o), es),
//         }
//       }
//     | (Cmp(o), list{e1, e2}) => {
//         let os = switch o {
//         | Lt => "<"
//         | Eq => "=="
//         | Gt => ">"
//         | Le => "<="
//         | Ge => ">="
//         | Ne => "!="
//         }
//         let e1 = e1(true)
//         let e2 = e2(true)
//         {
//           ann: op2("", getPrint(e1), ` ${os} `, getPrint(e2), "")->consumeContextWrap(context),
//           it: (Cmp(o), list{e1, e2}),
//         }
//       }
//     | (PairNew, list{e1, e2}) => {
//         let e1 = e1(false)
//         let e2 = e2(false)
//         let vecKeyword = if containsVecMutation.contents || containsVarMutation.contents {
//           "Buffer"
//         } else {
//           ""
//         }
//         {
//           ann: op2(`${vecKeyword}(`, getPrint(e1), ", ", getPrint(e2), ")")->consumeContext(
//             context,
//           ),
//           it: (PairNew, list{e1, e2}),
//         }
//       }
//     | (PairRefLeft, list{e1}) => {
//         let e1 = e1(true)
//         {
//           ann: op1("", getPrint(e1), "(0)")->consumeContext(context),
//           it: (PairRefLeft, list{e1}),
//         }
//       }
//     | (PairRefRight, list{e1}) => {
//         let e1 = e1(true)
//         {
//           ann: op1("", getPrint(e1), "(1)")->consumeContext(context),
//           it: (PairRefRight, list{e1}),
//         }
//       }
//     | (PairSetLeft, list{e1, e2}) => {
//         let e1 = e1(false)
//         let e2 = e2(false)
//         {
//           ann: op2("", getPrint(e1), "(0) = ", getPrint(e2), "")->consumeContextVoid(context),
//           it: (PairSetLeft, list{e1, e2}),
//         }
//       }
//     | (PairSetRight, list{e1, e2}) => {
//         let e1 = e1(false)
//         let e2 = e2(false)
//         {
//           ann: op2("", getPrint(e1), "(1) = ", getPrint(e2), "")->consumeContextVoid(context),
//           it: (PairSetRight, list{e1, e2}),
//         }
//       }
//     | (VecNew, es) => {
//         let es = es->List.map(e => e(false))
//         let vecKeyword = if containsVecMutation.contents || containsVarMutation.contents {
//           "Buffer"
//         } else {
//           ""
//         }
//         {
//           ann: op1(
//             `${vecKeyword}(`,
//             Print.concat(`, `, es->List.map(e => getPrint(e))) |> Print.dummyAnn,
//             ")",
//           )->consumeContext(context),
//           it: (VecNew, es),
//         }
//       }
//     | (VecRef, list{e1, e2}) => {
//         let e1 = e1(true)
//         let e2 = e2(false)
//         {
//           ann: op2("", getPrint(e1), "(", getPrint(e2), ")")->consumeContext(context),
//           it: (VecRef, list{e1, e2}),
//         }
//       }
//     | (VecSet, list{e1, e2, e3}) => {
//         let e1 = e1(true)
//         let e2 = e2(false)
//         let e3 = e3(false)
//         {
//           // ann: `${getPrint(e1)}[${getPrint(e2)}] = ${getPrint(e3)}`->consumeContextStat(context),
//           ann: op3(
//             "",
//             getPrint(e1),
//             "(",
//             getPrint(e2),
//             ") = ",
//             getPrint(e3),
//             "",
//           )->consumeContextVoid(context),
//           it: (VecSet, list{e1, e2, e3}),
//         }
//       }
//     | (VecLen, list{e1}) => {
//         let e1 = e1(false)
//         {
//           ann: op1("", getPrint(e1), ".length")->consumeContext(context),
//           it: (VecLen, list{e1}),
//         }
//       }
//     | (Err, list{e1}) => {
//         let e1 = e1(true)
//         {
//           ann: op1("throw ", getPrint(e1), "")->consumeContextWrap(context),
//           it: (Err, list{e1}),
//         }
//       }
//     | (Not, list{e1}) => {
//         let e1 = e1(true)
//         {
//           ann: op1("¬ ", getPrint(e1), "")->consumeContextWrap(context),
//           it: (Not, list{e1}),
//         }
//       }
//     | (Print, list{e1}) => {
//         let e1 = e1(false)
//         {
//           ann: op1("println(", getPrint(e1), ")")->consumeContextVoid(context),
//           it: (Print, list{e1}),
//         }
//       }
//     | (Next, list{e1}) => {
//         let e1 = e1(false)
//         {
//           ann: op1("next(", getPrint(e1), ")")->consumeContextVoid(context),
//           it: (Next, list{e1}),
//         }
//       }
//     | (Cons, _) => raisePrintError("List is not supported by JavaScript")
//     | _ =>
//       raisePrintError(
//         `Our pseudo-code syntax doesn't let you use ${Primitive.toString(p)} on ${List.length(
//             es,
//           ) |> Int.toString} parameter(s).`,
//       )
//     }
//   }

//   let funLike = (op, x, xs, e) => {
//     op2(`${op} `, exprAppToString(x, xs), " =", indentBlock(e, 2), "")
//   }

//   let defvarToString = (x, e) => {
//     let keyword = if containsVarMutation.contents {
//       "var"
//     } else {
//       "val"
//     }
//     op1("", defvarLike(`${keyword} `, x, e), "")
//   }

//   let deffunToString = (f, xs, b) => {
//     funLike("def", f, xs->List.map(x => group2(x, Print.string(" : Int"))), b)
//   }

//   let defgenToString = (f, xs, b) => {
//     funLike("gen def", f, xs->List.map(x => group2(x, Print.string(" : Int"))), b)
//   }

//   let exprSetToString = (x, e) => {
//     op2("", x, " = ", e, "")
//   }

//   let exprLamToString = (xs, b) => {
//     op2(
//       "(",
//       Print.concat(", ", xs->List.map(x => group2(x, Print.string(" : Int")))) |> Print.dummyAnn,
//       ") =>",
//       indentBlock(b, 2),
//       "",
//     )
//   }
//   let exprGenToString = (_xs, _b) => {
//     raisePrintError("generators are not supported yet in Scala translation.")
//   }
//   let exprYieldToString = e => op1("yield ", e, "")

//   let exprBgnToString = (es, e) => {
//     listToString(list{...es, e}) |> Print.dummyAnn
//   }

//   let exprCndToString = (ebs: list<(_, _)>, ob) => {
//     let ebs = {
//       switch ob {
//       | None => ebs
//       | Some(b) => list{...ebs, (Print.string(""), b)}
//       }
//     }
//     let ebs = ebs->List.map(((e, b)) => op2("if ", e, ":", indentBlock(b, 2), "\nend"))
//     Print.concat(" else ", ebs)
//   }

//   let exprIfToString = (e_cnd, e_thn, e_els) => {
//     op3("if ", e_cnd, " then ", e_thn, " else ", e_els, "")
//     // `${e_cnd} ? ${e_thn} : ${e_els}`
//   }

//   let symbolToString = ({it, ann}) => {
//     {
//       it,
//       ann: {
//         sourceLocation: ann,
//         print: Plain(printName(it)),
//       },
//     }
//   }

//   let rec printExp = ({it, ann: sourceLocation}, context) => {
//     let e: annotated<expressionNode<printAnn>, Print.t<sourceLocation>> = switch it {
//     | Con(c) => {
//         it: Con(c),
//         ann: Print.string(constantToString(c))->consumeContext(context),
//       }
//     | Ref(x) => {
//         it: Ref(x),
//         ann: Print.string(x->printName)->consumeContext(context),
//       }
//     | Set(x, e) => {
//         let x = symbolToString(x)
//         let e: expression<printAnn> = e->printExp->asExpr(false)
//         {
//           ann: exprSetToString(getPrint(x), getPrint(e))->consumeContextVoid(context),
//           it: Set(x, e),
//         }
//       }
//     | Lam(xs, b) => {
//         let xs = xs->List.map(symbolToString)
//         let b = b->printBlock(Return)
//         {
//           ann: exprLamToString(xs->List.map(x => getPrint(x)), getPrint(b))->consumeContextWrap(
//             context,
//           ),
//           it: Lam(xs, b),
//         }
//       }
//     | GLam(xs, b) => {
//         let xs = xs->List.map(symbolToString)
//         let b = b->printBlock(Return)
//         {
//           ann: exprGenToString(xs->List.map(x => getPrint(x)), getPrint(b))->consumeContextWrap(
//             context,
//           ),
//           it: Lam(xs, b),
//         }
//       }
//     | Yield(e) => {
//         let e = e->printExp->asExpr(false)
//         {
//           ann: exprYieldToString(getPrint(e))->consumeContextWrap(context),
//           it: Yield(e),
//         }
//       }
//     | AppPrm(p, es) => {
//         let es = es->List.map((e, b) => e->printExp->asExpr(b))
//         let {ann: print, it: (p, es)} = exprAppPrmToString(p, es, context)
//         {
//           it: AppPrm(p, es),
//           ann: print,
//         }
//       }
//     | App(e, es) => {
//         let e = e->printExp->asExpr(true)
//         let es = es->List.map(e => e->printExp->asExpr(false))
//         {
//           ann: exprAppToString(getPrint(e), es->List.map(e => getPrint(e)))->consumeContext(
//             context,
//           ),
//           it: App(e, es),
//         }
//       }
//     | Let(_xes, _b) =>
//       raisePrintError("let-expressions are not supported by our pseudo-code syntax")
//     | Letrec(_xes, _b) =>
//       raisePrintError("letrec-expressions are not supported by our pseudo-code syntax")
//     | Cnd(ebs, ob) =>
//       switch context {
//       | Expr(_) =>
//         raisePrintError(
//           "Multi-armed conditionals in our pseudo-code syntax is not supported by the translator yet.",
//         )
//       | Stat(context) => {
//           let ebs: list<(expression<printAnn>, block<printAnn>)> =
//             ebs->List.map(eb => eb->ebToString(context))
//           let ob = ob->obToString(context)
//           {
//             ann: exprCndToString(
//               ebs->List.map(((e, b)) => (getPrint(e), getPrint(b))),
//               ob->Option.map(b => getPrint(b)),
//             ),
//             it: Cnd(ebs, ob),
//           }
//         }
//       }
//     | If(e_cnd, e_thn, e_els) => {
//         let e_cnd = e_cnd->printExp->asExpr(true)
//         let e_thn = e_thn->printExp->asExpr(true)
//         let e_els = e_els->printExp->asExpr(true)
//         {
//           ann: exprIfToString(
//             getPrint(e_cnd),
//             getPrint(e_thn),
//             getPrint(e_els),
//           )->consumeContextWrap(context),
//           it: If(e_cnd, e_thn, e_els),
//         }
//       }
//     | Bgn(es, e) => {
//         let es = es->List.map(e => e->printExp->asExpr(false))
//         let e = e->printExp->asExpr(false)
//         {
//           ann: exprBgnToString(es->List.map(e => getPrint(e)), getPrint(e))->consumeContext(
//             context,
//           ),
//           it: Bgn(es, e),
//         }
//       }
//     }
//     let {ann: print, it} = e
//     {ann: {print, sourceLocation}, it}
//   }
//   and defToString = ({ann: sourceLocation, it: d}: definition<sourceLocation>): definition<
//     printAnn,
//   > => {
//     let d = switch d {
//     | Var(x, e) => {
//         let x = x->symbolToString
//         let e = e->printExp->asExpr(false)
//         {
//           ann: defvarToString(getPrint(x), getPrint(e)),
//           it: Var(x, e),
//         }
//       }
//     | Fun(f, xs, b) => {
//         let f = f->symbolToString
//         let xs = xs->List.map(symbolToString)
//         let b = b->printBlock(Return)
//         {
//           ann: deffunToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
//           it: Fun(f, xs, b),
//         }
//       }
//     | GFun(f, xs, b) => {
//         let f = f->symbolToString
//         let xs = xs->List.map(symbolToString)
//         let b = b->printBlock(Return)
//         {
//           ann: defgenToString(getPrint(f), xs->List.map(x => getPrint(x)), getPrint(b)),
//           it: GFun(f, xs, b),
//         }
//       }
//     }
//     let {ann: print, it} = d
//     {ann: {print: print.it, sourceLocation}, it}
//   }
//   // and xeToString = ({it: xe, ann: sourceLocation}: bind<sourceLocation>): bind<printAnn> => {
//   //   let (x, e) = xe
//   //   let (x, e) = (symbolToString(x), e->printExp->asExpr(false))
//   //   let print = defvarToString(getPrint(x), getPrint(e)).it
//   //   {
//   //     it: (x, e),
//   //     ann: {
//   //       print,
//   //       sourceLocation,
//   //     },
//   //   }
//   // }
//   and ebToString = (eb, ctx: statContext) => {
//     let (e, b) = eb
//     (e->printExp->asExpr(false), b->printBlock(ctx))
//   }
//   and obToString = (ob, ctx: statContext) => {
//     ob->Option.map(b => b->printBlock(ctx))
//   }
//   and printBlock = ({ann: sourceLocation, it: b}, context: statContext) => {
//     switch b {
//     | BRet(e) => printExp({it: e, ann: sourceLocation}, Stat(context)) |> mapAnn(e => BRet(e))
//     | BCons(t, b) => {
//         let t = printTerm(t, Step)
//         let b = printBlock(b, context)
//         let print = Group(list{getPrint(t), Print.string("\n"), getPrint(b)})
//         {
//           ann: {print, sourceLocation},
//           it: BCons(t, b),
//         }
//       }
//     }
//   }
//   and printTerm = ({ann: sourceLocation, it: t}: term<sourceLocation>, ctx): term<printAnn> => {
//     switch t {
//     | Exp(it) => printExp({ann: sourceLocation, it}, Stat(ctx)) |> mapAnn(v => Exp(v))
//     | Def(it) => defToString({ann: sourceLocation, it}) |> mapAnn(v => Def(v))
//     }
//   }

//   let printOutputlet = (o): string => {
//     let rec p = (v: val): string => {
//       switch v {
//       | Ref(_) => raisePrintError("Can't print circular data structure in Scala")
//       | Con(c) => constantToString(c)
//       | Struct(i, content) => {
//           let i = switch i {
//           | None => ""
//           | Some(_) => raisePrintError("Can't print circular data structure in Scala")
//           }
//           let content = switch content {
//           | Lst(_es) => raisePrintError("Lists are not supported in Scala.")
//           | Vec(es) => `Buffer(${String.concat(", ", es->List.map(p))})`
//           }
//           `${i}${content}`
//         }
//       }
//     }
//     switch o {
//     | OErr => "error"
//     | OVal(v) => p(v)
//     }
//   }

//   let printOutput = (~sep=" ", os): string => {
//     os->List.map(printOutputlet) |> String.concat(sep)
//   }

//   let printProgramFull = (insertPrintTopLevel, p: program<sourceLocation>) => {
//     let p = if insertPrintTopLevel {
//       insertTopLevelPrint(p)
//     } else {
//       p
//     }
//     let s = SMoLPrinter.printProgram(insertPrintTopLevel, p)
//     containsVarMutation := Js.String.includes("(set!", s)
//     containsVecMutation :=
//       Js.String.includes("vec-set!", s) ||
//       Js.String.includes("set-left!", s) ||
//       Js.String.includes("set-right!", s)

//     let rec print = ({it, ann: sourceLocation}: program<sourceLocation>): program<printAnn> => {
//       switch it {
//       | PNil => {it: PNil, ann: {print: Group(list{}), sourceLocation}}
//       | PCons(t, p) => {
//           let t = printTerm(t, Step)
//           switch p {
//           | {it: PNil} => {
//               it: PCons(
//                 t,
//                 {
//                   it: PNil,
//                   ann: {
//                     print: Plain(""),
//                     sourceLocation: {
//                       begin: sourceLocation.end,
//                       end: sourceLocation.end,
//                     },
//                   },
//                 },
//               ),
//               ann: {
//                 print: getPrint(t).it,
//                 sourceLocation,
//               },
//             }
//           | _ => {
//               let p = print(p)
//               {
//                 it: PCons(t, p),
//                 ann: {
//                   print: Print.concat2(getPrint(t), "\n", getPrint(p)),
//                   sourceLocation,
//                 },
//               }
//             }
//           }
//         }
//       }
//     }
//     print(p)
//   }

//   let printProgram = (insertPrintTopLevel, p) => {
//     printProgramFull(insertPrintTopLevel, p).ann.print |> Print.toString
//   }

//   let printStandAloneTerm = ({it, ann}: term<sourceLocation>): string => {
//     switch it {
//     | Def(it) => defToString({it, ann}).ann.print
//     | Exp(it) => printExp({it, ann}, Stat(Step)).ann.print
//     } |> Print.toString
//   }
// }

module type Translator = {
  let translateName: string => string
  // print terms, interleaved with whitespace
  let translateOutput: string => string
  let translateStandAloneTerm: string => string
  // print runnable full programs
  let translateProgram: (bool, string) => string
  let translateProgramFull: (bool, string) => program<printAnn>
}
module TranslateError = {
  type t =
    | ParseError(ParseError.t)
    | PrintError(string)
    | KindError(string)
  let toString = t => {
    switch t {
    | ParseError(err) => ParseError.toString(err)
    | PrintError(err) => err
    | KindError(err) => err
    }
  }
}
exception SMoLTranslateError(TranslateError.t)
let raiseTranslateError = e => raise(SMoLTranslateError(e))

let programAsTerm = (p: program<_>): term<_> => {
  switch p.it {
  | PCons(t, {it: PNil}) => t
  | _ => raiseTranslateError(KindError("Expecting a term, given a program"))
  }
}

module MakeTranslator = (P: Printer) => {
  let translateName = P.printName
  let translateOutput = src => {
    switch Parser.parseOutput(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | output =>
      switch P.printOutput(output) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | output => output
      }
    }
  }
  let translateStandAloneTerm = src => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch P.printStandAloneTerm(p |> programAsTerm) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | p => p
      }
    }
  }
  let translateProgram = (printTopLevel, src) => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch P.printProgram(printTopLevel, p) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | p => p
      }
    }
  }
  let translateProgramFull = (printTopLevel, src) => {
    switch Parser.parseProgram(src) {
    | exception SMoLParseError(err) => raise(SMoLTranslateError(ParseError(err)))
    | p =>
      switch P.printProgramFull(printTopLevel, p) {
      | exception SMoLPrintError(err) => raise(SMoLTranslateError(PrintError(err)))
      | p => p
      }
    }
  }
}

module SMoLTranslator = MakeTranslator(SMoLPrinter)
module JSTranslator = MakeTranslator(JSPrinter)
module PYTranslator = MakeTranslator(PYPrinter)
// module PCTranslator = MakeTranslator(PCPrinter)
// module SCTranslator = MakeTranslator(SCPrinter)
