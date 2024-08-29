open SExpression

let mapAnn = f => ({ann, it}: annotated<_, _>): annotated<_, _> => {
  {
    ann,
    it: f(it),
  }
}

type rec printNode<'id> =
  | Plain(string)
  | Group(list<print<'id>>)
and print<'id> = annotated<printNode<'id>, option<'id>>

let concat = (s, ss) => Array.join(ss, s)

module Print = {
  type t<'id> = printNode<'id>
  let toSourceMap = t => {
    let hMap = Map.make()
    let ln = ref(0)
    let ch = ref(0)
    let rec f = ({it, ann}) => {
      let begin = {ln: ln.contents, ch: ch.contents}
      switch it {
      | Group(es) => es->List.forEach(f)
      | Plain(s) =>
        s
        ->String.split("")
        ->Array.forEach(c => {
          switch c {
          | "\n" => {
              ln := ln.contents + 1
              ch := 0
            }
          | _ => ch := ch.contents + 1
          }
        })
      }
      let end = {ln: ln.contents, ch: ch.contents}
      ann->Option.forEach(ann => {
        Map.set(hMap, ann, {begin, end})
      })
    }
    f(t)
    hMap
  }
  let rec toString = it => {
    switch it {
    | Plain(s) => s
    | Group(ts) =>
      concat(
        "",
        ts
        ->List.map(({it}) => {
          toString(it)
        })
        ->List.toArray,
      )
    }
  }
  let rec map = f => it => {
    switch it {
    | Plain(s) => Plain(f(s))
    | Group(ts) => Group(ts->List.map(mapAnn(t => map(f)(t))))
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

  let dummy = it => {it, ann: None}

  let s = (strings: array<string>, parameters: array<print<'id>>): t<'id> => {
    let ih = switch Array.last(strings)->Option.getExn {
    | "" => list{}
    | s => list{string(s)}
    }
    Group(
      parameters->Array.reduceRightWithIndex(ih, (ih, parameter, i) => {
        let ih = list{parameter, ...ih}
        let s = strings[i]->Option.getExn
        if s == "" {
          ih
        } else {
          list{string(s), ...ih}
        }
      }),
    )
  }
}

let wrap = (prefix, p, suffix) => Print.s`${Print.string(prefix)}${p}${Print.string(suffix)}`

let rec containsNL = it => {
  switch it {
  | Plain(s) => String.includes(s, "\n")
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
  Print.dummy(s),
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
    | Equal
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
    | Cmp(Equal) => "equal?"
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

and termNode<'ann> = Def(definition<'ann>) | Exp(expression<'ann>)
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

let xsOfDef = d => {
  switch d.it {
  | Var(x, _) => list{x}
  | Fun(f, _xs, _b) => list{f}
  | GFun(f, _xs, _b) => list{f}
  }
}

let xsOfTerm = t => {
  switch t.it {
  | Exp(_) => list{}
  | Def(d) => xsOfDef(d)
  }
}

let xsOfBlock = b => List.flat(termsOfBlock(b)->List.map(xsOfTerm))
let xsOfProgram = p => List.flat(termsOfProgram(p)->List.map(xsOfTerm))

type nodeKind =
  | Name
  | Expression
  | Bind
  | Block
  | Definition
  | Term
  | Program

module NodeKind = {
  type t = nodeKind
  let toString: nodeKind => string = t => {
    switch t {
    | Name => "name"
    | Expression => "expression"
    | Bind => "bind"
    | Block => "block"
    | Definition => "definition"
    | Term => "term"
    | Program => "program"
    }
  }
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
      `expecting ${context}, given ${concat(" ", es->List.map(SExpr.toString)->List.toArray)}`
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
          | Sequence({sequenceKind: Vector, content}) => Struct(None, Vec(content->List.map(p)))
          | Sequence({sequenceKind: List, content}) => Struct(None, Lst(content->List.map(p)))
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
    | Sequence({sequenceKind: Vector, content}) => {
        let content = content->List.map(parseValue)
        {ann, it: AppPrm(VecNew, content)}
      }
    | Sequence({sequenceKind: List}) => raiseParseError(LiteralListError(e))
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
    | Sequence({sequenceKind: List, content}) => {ann, it: content}
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
    switch e.it {
    | Exp(it) => it
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
        tryNum->Option.getOr(Ref(x))
      }
      e
    }
  }

  let rec letstar = (ann, xes, body: block<sourceLocation>) => {
    switch xes {
    | list{} =>
      switch body.it {
      | BRet(e) => e
      | _ => ann(Let(list{}, body))
      }
    | list{xe} => ann(Let(list{xe}, body))
    | list{xe, ...xes} => ann(Let(list{xe}, makeBlock(list{}, letstar(it => {
              {
                ann: {
                  begin: xes
                  ->List.head
                  ->Option.map(xe => xe.ann.begin)
                  ->Option.getOr(body.ann.begin),
                  end: body.ann.end,
                },
                it,
              }
            }, xes, body))))
    }
  }

  let rec parseTerm = (e: sexpr): term<sourceLocation> => {
    let ann = it => {ann: e.ann, it}
    ann(
      switch e.it {
      | Sequence({sequenceKind: Vector, content}) => {
          let content = content->List.map(parseValue)
          Exp(ann(AppPrm(VecNew, content)))
        }
      | Sequence({content: list{{it: Atom(Sym("quote")), ann: _}, ...rest}}) => {
          let e = as_one("a quoted value", rest)
          Exp(parseValue(e))
        }
      | Sequence({content: list{{it: Atom(Sym("defvar")), ann: _}, ...rest}}) => {
          let (x, e) = as_two("a variable and an expression", rest)
          let x = as_id("a variable name", x)
          let e = as_expr("an expression", parseTerm(e))
          Def(ann(Var(x, e)))
        }

      | Sequence({content: list{{it: Atom(Sym("deffun")), ann: _}, ...rest}}) => {
          let (head, terms, result) = as_one_then_many_then_one(
            "a function header and a body",
            rest,
          )
          let (fun, args) = as_one_then_many(
            "function name followed by parameters",
            as_list("function name and parameters", head).it,
          )
          let fun = as_id("a function name", fun)
          let args = List.map(args, arg => as_id("a parameter", arg))
          let terms = Belt.List.map(terms, parseTerm)
          let result = as_expr("an expression to be returned", parseTerm(result))
          Def(ann(Fun(fun, args, makeBlock(terms, result))))
        }

      | Sequence({content: list{{it: Atom(Sym("defgen")), ann: _}, ...rest}}) => {
          let (head, terms, result) = as_one_then_many_then_one(
            "a generator header and a body",
            rest,
          )
          let (fun, args) = as_one_then_many(
            "generator name followed by parameters",
            as_list("generator name and parameters", head).it,
          )
          let fun = as_id("a generator name", fun)
          let args = List.map(args, arg => as_id("a parameter", arg))
          let terms = Belt.List.map(terms, parseTerm)
          let result = as_expr("an expression to be returned", parseTerm(result))
          Def(ann(GFun(fun, args, makeBlock(terms, result))))
        }

      | Sequence({content: list{{it: Atom(Sym("lambda")), ann: _}, ...rest}}) => {
          let (args, terms, result) = as_one_then_many_then_one(
            "the function signature followed by the function body",
            rest,
          )
          let args =
            as_list("function parameters", args).it->List.map(arg => as_id("a parameter", arg))
          let terms = terms->List.map(parseTerm)
          let result = as_expr("an expression to be returned", parseTerm(result))
          Exp(ann(Lam(args, makeBlock(terms, result))))
        }

      | Sequence({content: list{{it: Atom(Sym("generator")), ann: _}, ...rest}}) => {
          let (args, terms, result) = as_one_then_many_then_one(
            "the generator signature followed by the function body",
            rest,
          )
          let args =
            as_list("generator parameters", args).it->List.map(arg => as_id("a parameter", arg))
          let terms = terms->List.map(parseTerm)
          let result = as_expr("an expression to be returned", parseTerm(result))
          Exp(ann(GLam(args, makeBlock(terms, result))))
        }

      | Sequence({content: list{{it: Atom(Sym("yield")), ann: _}, ...rest}}) => {
          let e = as_one("an expression", rest)
          let e = as_expr("an expression", parseTerm(e))
          Exp(ann(Yield(e)))
        }

      | Sequence({content: list{{it: Atom(Sym("λ")), ann: _}, ...rest}}) => {
          let (args, terms, result) = as_one_then_many_then_one(
            "the function signature followed by the function body",
            rest,
          )
          let args =
            as_list("function parameters", args).it->List.map(arg => as_id("a parameter", arg))
          let terms = terms->List.map(parseTerm)
          let result = as_expr("an expression to be returned", parseTerm(result))
          Exp(ann(Lam(args, makeBlock(terms, result))))
        }

      | Sequence({content: list{{it: Atom(Sym("begin")), ann: _}, ...rest}}) => {
          let (terms, result) = as_many_then_one("one or more expressions", rest)
          let terms = terms->List.map(parseTerm)->List.map(t => as_expr("an expression", t))
          let result = as_expr("an expression", result->parseTerm)
          Exp(ann(Bgn(terms, result)))
        }

      | Sequence({content: list{{it: Atom(Sym("set!")), ann: _}, ...rest}}) => {
          let (x, e) = as_two("a variable and an expression", rest)
          let x = as_id("a variable to be set", x)
          let e = as_expr("an expression", parseTerm(e))
          Exp(ann(Set(x, e)))
        }

      | Sequence({content: list{{it: Atom(Sym("if")), ann: _}, ...rest}}) => {
          let (e_cnd, e_thn, e_els) = as_three(
            "three expressions (i.e., a condition, the \"then\" branch, and the \"else\" branch)",
            rest,
          )
          let e_cnd = as_expr("a (conditional) expression", parseTerm(e_cnd))
          let e_thn = as_expr("an expression", parseTerm(e_thn))
          let e_els = as_expr("an expression", parseTerm(e_els))
          Exp(ann(If(e_cnd, e_thn, e_els)))
        }

      | Sequence({content: list{{it: Atom(Sym("cond")), ann: _}, ...branches}}) => {
          let branches =
            branches
            ->List.map(branch => as_list("a `cond` branch", branch).it)
            ->List.map(branch =>
              as_one_then_many_then_one("the condition followed by the branch", branch)
            )
          let rec loop = (parsed, branches) => {
            switch branches {
            | list{} => Exp(ann(Cnd(List.reverse(parsed), None)))
            | list{({it: Atom(Sym("else")), ann: _}: sexpr, terms, result)} => {
                let terms = terms->List.map(parseTerm)
                let result = as_expr("an expression", parseTerm(result))
                Exp(ann(Cnd(List.reverse(parsed), Some(makeBlock(terms, result)))))
              }

            | list{(case, terms, result), ...branches} => {
                let case = as_expr("a (conditional) expression", case->parseTerm)
                let terms = terms->List.map(parseTerm)
                let result = as_expr("an expression", parseTerm(result))
                loop(list{(case, makeBlock(terms, result)), ...parsed}, branches)
              }
            }
          }
          loop(list{}, branches)
        }

      | Sequence({content: list{{it: Atom(Sym("let")), ann: _}, ...rest}}) => {
          let (xes, ts, result) = as_one_then_many_then_one(
            "the bindings followed by the body",
            rest,
          )
          let xes =
            as_list("variable-expression pairs", xes).it
            ->List.map(xe => as_list("a variable and an expression", xe))
            ->List.map(mapAnn(xe => as_two("a variable and an expression", xe)))
          let xes = xes->List.map(
            mapAnn(((x, e)) => {
              let x = as_id("a variable to be bound", x)
              let e = as_expr("an expression", parseTerm(e))
              (x, e)
            }),
          )
          let ts = ts->List.map(parseTerm)
          let result = as_expr("an expression to be return", parseTerm(result))
          Exp(ann(Let(xes, makeBlock(ts, result))))
        }

      | Sequence({content: list{{it: Atom(Sym("let*")), ann: _}, ...rest}}) => {
          let (xes, ts, result) = as_one_then_many_then_one(
            "the bindings followed by the body",
            rest,
          )
          let xes =
            as_list("variable-expression pairs", xes).it
            ->List.map(xe => as_list("a variable and an expression", xe))
            ->List.map(mapAnn(xe => as_two("a variable and an expression", xe)))
          let xes = xes->List.map(
            mapAnn(((x, e)) => {
              let x = as_id("a variable to be bound", x)
              let e = as_expr("an expression", parseTerm(e))
              (x, e)
            }),
          )
          let ts = ts->List.map(parseTerm)
          let result = as_expr("an expression to be return", parseTerm(result))
          Exp(letstar(ann, xes, makeBlock(ts, result)))
        }

      | Sequence({content: list{{it: Atom(Sym("letrec")), ann: _}, ...rest}}) => {
          let (xes, ts, result) = as_one_then_many_then_one(
            "the bindings followed by the body",
            rest,
          )
          let xes =
            as_list("variable-expression pairs", xes).it
            ->List.map(xe => as_list("a variable and an expression", xe))
            ->List.map(mapAnn(xe => as_two("a variable and an expression", xe)))
          let xes = xes->List.map(
            mapAnn(((x, e)) => {
              let x = as_id("a variable to be bound", x)
              let e = as_expr("an expression", parseTerm(e))
              (x, e)
            }),
          )
          let ts = ts->List.map(parseTerm)
          let result = as_expr("an expression to be return", parseTerm(result))
          Exp(ann(Letrec(xes, makeBlock(ts, result))))
        }

      | Atom(atom) => Exp(ann(expr_of_atom(atom)))
      | Sequence({content: list{{it: Atom(Sym("next")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Next, es)
      | Sequence({content: list{{it: Atom(Sym("+")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Arith(Add), es)
      | Sequence({content: list{{it: Atom(Sym("-")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Arith(Sub), es)
      | Sequence({content: list{{it: Atom(Sym("*")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Arith(Mul), es)
      | Sequence({content: list{{it: Atom(Sym("/")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Arith(Div), es)
      | Sequence({content: list{{it: Atom(Sym("<")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Cmp(Lt), es)
      | Sequence({content: list{{it: Atom(Sym("=")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Cmp(Eq), es)
      | Sequence({content: list{{it: Atom(Sym(">")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Cmp(Gt), es)
      | Sequence({content: list{{it: Atom(Sym("<=")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Cmp(Le), es)
      | Sequence({content: list{{it: Atom(Sym(">=")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Cmp(Ge), es)
      | Sequence({content: list{{it: Atom(Sym("!=")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Cmp(Ne), es)
      | Sequence({content: list{{it: Atom(Sym("pair")), ann: _}, ...es}}) =>
        makeAppPrm(ann, PairNew, es)
      | Sequence({content: list{{it: Atom(Sym("mpair")), ann: _}, ...es}}) =>
        makeAppPrm(ann, PairNew, es)
      | Sequence({content: list{{it: Atom(Sym("left")), ann: _}, ...es}}) =>
        makeAppPrm(ann, PairRefLeft, es)
      | Sequence({content: list{{it: Atom(Sym("right")), ann: _}, ...es}}) =>
        makeAppPrm(ann, PairRefRight, es)
      | Sequence({content: list{{it: Atom(Sym("set-left!")), ann: _}, ...es}}) =>
        makeAppPrm(ann, PairSetLeft, es)
      | Sequence({content: list{{it: Atom(Sym("set-right!")), ann: _}, ...es}}) =>
        makeAppPrm(ann, PairSetRight, es)
      | Sequence({content: list{{it: Atom(Sym("mvec")), ann: _}, ...es}}) =>
        makeAppPrm(ann, VecNew, es)
      | Sequence({content: list{{it: Atom(Sym("vec")), ann: _}, ...es}}) =>
        makeAppPrm(ann, VecNew, es)
      | Sequence({content: list{{it: Atom(Sym("vec-ref")), ann: _}, ...es}}) =>
        makeAppPrm(ann, VecRef, es)
      | Sequence({content: list{{it: Atom(Sym("vref")), ann: _}, ...es}}) =>
        makeAppPrm(ann, VecRef, es)
      | Sequence({content: list{{it: Atom(Sym("vec-set!")), ann: _}, ...es}}) =>
        makeAppPrm(ann, VecSet, es)
      | Sequence({content: list{{it: Atom(Sym("vset!")), ann: _}, ...es}}) =>
        makeAppPrm(ann, VecSet, es)
      | Sequence({content: list{{it: Atom(Sym("vec-len")), ann: _}, ...es}}) =>
        makeAppPrm(ann, VecLen, es)
      | Sequence({content: list{{it: Atom(Sym("vlen")), ann: _}, ...es}}) =>
        makeAppPrm(ann, VecLen, es)
      | Sequence({content: list{{it: Atom(Sym("eq?")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Cmp(Eq), es)
      | Sequence({content: list{{it: Atom(Sym("equal?")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Cmp(Equal), es)
      | Sequence({content: list{{it: Atom(Sym("error")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Err, es)
      | Sequence({content: list{{it: Atom(Sym("not")), ann: _}, ...es}}) => makeAppPrm(ann, Not, es)
      | Sequence({content: list{{it: Atom(Sym("print")), ann: _}, ...es}}) =>
        makeAppPrm(ann, Print, es)
      | Sequence({content: es}) => {
          let (e, es) = as_one_then_many(
            "a function call/application, which includes a function and then zero or more arguments",
            es,
          )
          let e = as_expr("a function", e->parseTerm)
          let es = es->List.map(parseTerm)->List.map(e => as_expr("an argument", e))
          Exp(ann(App(e, es)))
        }
      },
    )
  }
  and makeAppPrm = (ann, p, es) => {
    let es = es->List.map(parseTerm)->List.map(e => as_expr("an argument", e))
    Exp(ann(AppPrm(p, es)))
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

// Infer Type
// type values
type rec ty =
  | Top
  | TUni
  | Num
  | Boolean
  | String
  | Vecof(ty)
  | Listof(ty)
  | Funof({args: list<ty>, out: ty})
// type expressions
type typed<'ann> = {ty: ty, ann: 'ann}
type rec tye =
  | TVar(string)
  | Num
  | TUni
  | Boolean
  | String
  | Vecof(tye)
  | Listof(tye)
  | Funof({args: list<tye>, out: tye})
type rec typeEnv = list<Map.t<string, tye>>

type typeError =
  | RecursiveType
  | UnboundId(string)
  | NotSupported(string)
  | Incompatible(tye, tye)
type exn += SMoLTypeError(typeError)
let raiseTypeError = err => raise(SMoLTypeError(err))

let inferTypes: (program<'ann>, 'ann => string) => program<typed<'ann>> = (
  p,
  getKey: 'ann => string,
) => {
  let rec lookup = (env: typeEnv, x): tye => {
    switch env {
    | list{} => raiseTypeError(UnboundId(x))
    | list{f, ...env} =>
      switch Map.get(f, x) {
      | None => lookup(env, x)
      | Some(tye) => tye
      }
    }
  }
  let extend = (env: typeEnv, xs): typeEnv => {
    list{
      xs
      ->List.map(x => (x.it, TVar(getKey(x.ann))))
      ->List.toArray
      ->Map.fromArray,
      ...env,
    }
  }
  let constraints = {
    let constraints = ref(list{})
    let addConstraint = (t1: tye, t2: tye) => {
      constraints := list{(t1, t2), ...constraints.contents}
    }
    let freshVar = {
      let i = ref(0)
      () => {
        let t = TVar(`fresh-${Int.toString(i.contents)}`)
        i := 1 + i.contents
        t
      }
    }
    let asVec = t1 => {
      switch t1 {
      | TVar(_) => {
          let t2 = freshVar()
          addConstraint(t1, Vecof(t2))
          t2
        }
      | Vecof(t) => t
      | _ => raiseTypeError(Incompatible(t1, Vecof(freshVar())))
      }
    }
    let rec gatherConstraints = (p: program<'ann>) => {
      let xs = xsOfProgram(p)
      let globalEnv: typeEnv = list{
        Map.fromArray(
          xs
          ->List.toArray
          ->Array.map(x => {
            (x.it, TVar(getKey(x.ann)))
          }),
        ),
      }
      gp(globalEnv, p)
    }
    and gp = (env, p: program<'ann>) => {
      switch p.it {
      | PNil => ()
      | PCons(t, p) =>
        gt(env, t)
        gp(env, p)
      }
    }
    and gt = (env, t: term<'ann>) => {
      switch t.it {
      | Def(d) => gd(env, d)
      | Exp(e) => ignore(ge(env, e))
      }
    }
    and gd = (env, d: definition<'ann>) => {
      switch d.it {
      | Var(x, e) => addConstraint(lookup(env, x.it), ge(env, e))
      | Fun(f, xs, b) =>
        addConstraint(
          lookup(env, f.it),
          Funof({
            args: xs->List.map(x => TVar(getKey(x.ann))),
            out: gb(extend(env, list{...xs, ...xsOfBlock(b)}), b),
          }),
        )
      | GFun(_, _, _) => raiseTypeError(NotSupported("generator"))
      }
    }
    and ge = (env, e: expression<'ann>): tye => {
      let t = switch e.it {
      | Con(c) => gc(c)
      | Ref(x) => lookup(env, x)
      | Set(x, e) =>
        addConstraint(TVar(getKey(x.ann)), lookup(env, x.it))
        addConstraint(lookup(env, x.it), ge(env, e))
        TUni
      | Lam(xs, b) =>
        Funof({
          args: xs->List.map(x => TVar(getKey(x.ann))),
          out: gb(extend(env, list{...xs, ...xsOfBlock(b)}), b),
        })
      | Let(_bs, _b) => raiseTypeError(NotSupported("let"))
      | Letrec(_bs, _b) => raiseTypeError(NotSupported("letrec"))
      | AppPrm(p, es) => ga(p, es->List.map(e => ge(env, e)))
      | App(f, args) => {
          let out = TVar(getKey(e.ann))
          addConstraint(
            ge(env, f),
            Funof({
              args: args->List.map(arg => ge(env, arg)),
              out,
            }),
          )
          out
        }
      | Bgn(es, e) =>
        es->List.forEach(e => ignore(ge(env, e)))
        ge(env, e)
      | If(e_cnd, e_thn, e_els) => {
          addConstraint(Boolean, ge(env, e_cnd))
          let t_thn = ge(env, e_thn)
          let t_els = ge(env, e_els)
          addConstraint(t_thn, t_els)
          t_els
        }
      | Cnd(ebs, ob) => {
          let t = switch ob {
          | None => TUni
          | Some(b) => gb(env, b)
          }
          ebs->List.forEach(((e, b)) => {
            addConstraint(Boolean, ge(env, e))
            addConstraint(t, gb(extend(env, xsOfBlock(b)), b))
          })
          t
        }
      | GLam(_, _) => raiseTypeError(NotSupported("generators"))
      | Yield(_) => raiseTypeError(NotSupported("generators"))
      }
      addConstraint(TVar(getKey(e.ann)), t)
      t
    }
    and ga = (p, vs): tye =>
      switch (p, vs) {
      | (Arith(_), vs) => {
          vs->List.forEach(v => addConstraint(Num, v))
          Num
        }
      | (Cmp(_), vs) => {
          vs->List.forEach(v => addConstraint(Num, v))
          Boolean
        }
      | (VecNew, vs) => {
          let te = freshVar()
          vs->List.forEach(v => addConstraint(te, v))
          Vecof(te)
        }
      | (VecRef, list{v_vec, v_ind}) => {
          let te = asVec(v_vec)
          addConstraint(Num, v_ind)
          te
        }

      | (VecLen, list{v}) => {
          ignore(asVec(v))
          Num
        }

      | (VecSet, list{v_vec, v_ind, v_val}) => {
          addConstraint(v_vec, Vecof(v_val))
          addConstraint(v_ind, Num)
          TUni
        }

      | (Err, list{v}) => {
          addConstraint(String, v)
          freshVar()
        }

      | (Not, list{v}) => {
          addConstraint(Boolean, v)
          Boolean
        }

      | (PairNew, list{v1, v2}) => {
          addConstraint(v1, v2)
          Vecof(v1)
        }

      | (PairRefLeft, list{v}) => asVec(v)

      | (PairRefRight, list{v}) => asVec(v)

      | (PairSetLeft, list{v_vec, v_val}) => {
          addConstraint(v_vec, Vecof(v_val))
          TUni
        }

      | (PairSetRight, list{v_vec, v_val}) => {
          addConstraint(v_vec, Vecof(v_val))
          TUni
        }

      | (Print, list{_}) => TUni

      | (Next, list{_}) => raiseTypeError(NotSupported("generator"))

      | _otherwise => raiseTypeError(NotSupported(`Internal error with ${Primitive.toString(p)}`))
      }

    and gc = (c: constant): tye => {
      switch c {
      | Uni => TUni
      | Nil => raiseTypeError(NotSupported("lists"))
      | Num(_) => Num
      | Lgc(_) => Boolean
      | Str(_) => String
      | Sym(_) => raiseTypeError(NotSupported("symbol"))
      }
    }
    and gb = (env, b: block<'ann>): tye => {
      switch b.it {
      | BRet(e) => ge(env, e)
      | BCons(t, b) => {
          gt(env, t)
          gb(env, b)
        }
      }
    }
    gatherConstraints(p)
    constraints.contents
  }
  let solution: Map.t<string, ty> = {
    let map: Map.t<string, tye> = Map.make()
    let rec walk = e => {
      // find the head-normal form
      switch e {
      | TVar(x) =>
        switch Map.get(map, x) {
        | None => e
        | Some(e) => walk(e)
        }
      | e => e
      }
    }
    let rec unify = (e1: tye, e2: tye) => {
      let e1 = walk(e1)
      let e2 = walk(e2)
      switch (e1, e2) {
      | (TVar(x1), TVar(x2)) =>
        if x1 != x2 {
          Map.set(map, x1, e2)
        }
      | (TVar(x), e) => Map.set(map, x, e)
      | (e, TVar(x)) => Map.set(map, x, e)
      | (Num, Num) => ()
      | (TUni, TUni) => ()
      | (Boolean, Boolean) => ()
      | (String, String) => ()
      | (Vecof(e1), Vecof(e2)) => unify(e1, e2)
      | (Listof(e1), Listof(e2)) => unify(e1, e2)
      | (Funof(e1), Funof(e2)) =>
        if List.length(e1.args) == List.length(e1.args) {
          List.forEach2(e1.args, e2.args, unify)
          unify(e1.out, e2.out)
        } else {
          raiseTypeError(Incompatible(Funof(e1), Funof(e2)))
        }
      | _ => raiseTypeError(Incompatible(e1, e2))
      }
    }
    constraints->List.forEach(((e1, e2)) => unify(e1, e2))
    let ground = (e: tye): ty => {
      let visited = Set.make()
      let rec g = e => {
        switch e {
        | TVar(x) =>
          if Set.has(visited, x) {
            raiseTypeError(RecursiveType)
          } else {
            Set.add(visited, x)
            switch Map.get(map, x) {
            | None => Top
            | Some(e) => g(e)
            }
          }
        | TUni => TUni
        | Num => Num
        | Boolean => Boolean
        | String => String
        | Vecof(e) => Vecof(g(e))
        | Listof(e) => Listof(g(e))
        | Funof({args, out}) =>
          Funof({
            args: args->List.map(g),
            out: g(out),
          })
        }
      }
      g(e)
    }
    map
    ->Map.entries
    ->Array.fromIterator
    ->Array.map(((k, v)) => (k, ground(v)))
    ->Map.fromArray
  }

  // attach type
  let ax = ({ann, it}) => {
    {
      ann: {ann, ty: Map.get(solution, getKey(ann))->Option.getExn},
      it,
    }
  }
  let rec ap = ({ann, it}: program<'ann>): program<typed<'ann>> => {
    {
      ann: {ann, ty: TUni},
      it: switch it {
      | PNil => PNil
      | PCons(t, p) => PCons(at(t), ap(p))
      },
    }
  }
  and at = ({ann, it}) => {
    {
      ann: {ann, ty: TUni},
      it: switch it {
      | Def(d) => Def(ad(d))
      | Exp(e) => Exp(ae(e))
      },
    }
  }
  and ad = ({ann, it}) => {
    {
      ann: {ann, ty: TUni},
      it: switch it {
      | Var(x, e) => Var(ax(x), ae(e))
      | Fun(f, xs, b) => Fun(ax(f), List.map(xs, ax), ab(b))
      | GFun(f, xs, b) => GFun(ax(f), List.map(xs, ax), ab(b))
      },
    }
  }
  and ae = ({ann, it}) => {
    {
      ann: {ann, ty: Map.get(solution, getKey(ann))->Option.getExn},
      it: switch it {
      | Con(c) => Con(c)
      | Ref(x) => Ref(x)
      | Set(x, e) => Set(ax(x), ae(e))
      | Lam(xs, b) => Lam(List.map(xs, ax), ab(b))
      | Let(xes, b) => Let(List.map(xes, axe), ab(b))
      | Letrec(xes, b) => Letrec(List.map(xes, axe), ab(b))
      | AppPrm(p, es) => AppPrm(p, List.map(es, ae))
      | App(e, es) => App(ae(e), List.map(es, ae))
      | Bgn(es, e) => Bgn(List.map(es, ae), ae(e))
      | If(e_cnd, e_thn, e_els) => If(ae(e_cnd), ae(e_thn), ae(e_els))
      | Cnd(ebs, ob) => Cnd(List.map(ebs, aeb), Option.map(ob, ab))
      | GLam(xs, b) => GLam(List.map(xs, ax), ab(b))
      | Yield(e) => Yield(ae(e))
      },
    }
  }
  and axe = ({ann, it: (x, e)}) => {
    {
      ann: {ann, ty: TUni},
      it: (ax(x), ae(e)),
    }
  }
  and aeb = ((e, b)) => {
    (ae(e), ab(b))
  }
  and ab = ({ann, it}) => {
    switch it {
    | BRet(e) => {
        let e = ae(e)
        {
          ann: {ann, ty: e.ann.ty},
          it: BRet(e),
        }
      }
    | BCons(t, b) => {
        let t = at(t)
        let b = ab(b)
        {
          ann: {ann, ty: b.ann.ty},
          it: BCons(t, b),
        }
      }
    }
  }
  ap(p)
}

type exn += SMoLPrintError(string)
let raisePrintError = err => raise(SMoLPrintError(err))

type kindedSourceLocation = {
  nodeKind: nodeKind,
  sourceLocation: sourceLocation,
}
type printAnn = {sourceLocation: sourceLocation, print: Print.t<kindedSourceLocation>}

module type Printer = {
  let printName: string => string
  let printOutputlet: outputlet => string
  let printOutput: (~sep: string=?, list<outputlet>) => string
  let printStandAloneTerm: term<sourceLocation> => string
  let printProgram: (bool, program<sourceLocation>) => string
  let printProgramFull: (bool, program<sourceLocation>) => program<printAnn>
}

let getPrint = ({ann: {print, sourceLocation}}: expression<printAnn>) => {
  {it: print, ann: Some({sourceLocation, nodeKind: Expression})}
}
let getNamePrint = ({ann: {print, sourceLocation}}: annotated<string, printAnn>) => {
  {it: print, ann: Some({sourceLocation, nodeKind: Name})}
}
let getBindPrint = ({ann: {print, sourceLocation}}: bind<printAnn>) => {
  {it: print, ann: Some({sourceLocation, nodeKind: Bind})}
}
let getBlockPrint = ({ann: {print, sourceLocation}}: block<printAnn>) => {
  {it: print, ann: Some({sourceLocation, nodeKind: Block})}
}
let getDefinitionPrint = ({ann: {print, sourceLocation}}: definition<printAnn>) => {
  {it: print, ann: Some({sourceLocation, nodeKind: Definition})}
}
let getTermPrint = ({ann: {print, sourceLocation}}: term<printAnn>) => {
  {it: print, ann: Some({sourceLocation, nodeKind: Term})}
}
let getProgramPrint = ({ann: {print, sourceLocation}}: program<printAnn>) => {
  {it: print, ann: Some({sourceLocation, nodeKind: Program})}
}

let indent = (t: annotated<Print.t<'id>, option<'id>>, i): annotated<Print.t<'id>, option<'id>> => {
  let pad = Js.String.repeat(i, " ")
  mapAnn(Print.map(s => Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s)))(t)
}
let indentBlock = (s, i) => indent(group(list{Print.string("\n"), s}), i)
let hcat = (s1, s2) => {
  Group(list{s1, indent(s2, String.length(Print.toString(s1.it)))})
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
    | Str(s) => JSON.stringify(String(s))
    | Sym(s) => s
    }
  }

  let listToString = ss => {
    open! Print
    pad("(", Print.dummy(concat(" ", ss)), ")")
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
    defvarLike("deffun", Print.dummy(listToString(list{f, ...xs})), b)
  }

  let defgenToString = (f, xs, b) => {
    defvarLike("defgen", Print.dummy(listToString(list{f, ...xs})), b)
  }

  let exprSetToString = (x: print<kindedSourceLocation>, e) => {
    defvarLike("set!", x, e)
  }

  let exprLamToString = (xs, b) => {
    defvarLike("lambda", Print.dummy(listToString(xs)), b)
  }
  let exprGenToString = (xs, b) => {
    defvarLike("generator", Print.dummy(listToString(xs)), b)
  }
  let exprYieldToString = e => Group({list{Print.string("(yield "), e, Print.string(")")}})

  let exprAppToString = (e, es) => {
    listToString(list{e, ...es})
  }

  let beginLike = (op, ts) => {
    Group(list{
      Print.string("("),
      Print.string(op),
      indentBlock(Print.dummy(Print.concat("\n", ts)), 2),
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
        Print.dummy(Print.concat("\n", list{e_cnd, e_thn, e_els})),
        Print.string(")"),
      )->Print.dummy,
    )
    // hcat(Print.string(`(if `), `${concat("\n", list{e_cnd, e_thn, e_els})})`)
  }

  let letLike = (op: string, xes: list<_>, b: _) => {
    let xes = Print.dummy(Print.concat("\n", xes))
    let xes = group(list{Print.string("("), indent(xes, 1), Print.string(")")})
    Group(list{
      Print.dummy(hcat(group(list{Print.string("("), Print.string(op), Print.string(" ")}), xes)),
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
    let e = switch it {
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
          ann: exprSetToString(getNamePrint(x), getPrint(e)),
          it: Set(x, e),
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: exprLamToString(xs->List.map(x => getNamePrint(x)), getBlockPrint(b)),
          it: Lam(xs, b),
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: exprGenToString(xs->List.map(x => getNamePrint(x)), getBlockPrint(b)),
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
          ann: exprLetToString(xes->List.map(xe => getBindPrint(xe)), getBlockPrint(b)),
          it: Let(xes, b),
        }
      }
    | Letrec(xes, b) => {
        let xes = xes->List.map(xeToString)
        let b = b->printBlock
        {
          ann: exprLetrecToString(xes->List.map(xe => getBindPrint(xe)), getBlockPrint(b)),
          it: Letrec(xes, b),
        }
      }
    | Cnd(ebs, ob) => {
        let ebs = ebs->List.map(ebToString)
        let ob = ob->obToString
        {
          ann: exprCndToString(
            ebs->List.map(((e, b)) => (getPrint(e), getBlockPrint(b))),
            ob->Option.map(b => getBlockPrint(b)),
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
          ann: defvarToString(getNamePrint(x), getPrint(e)),
          it: Var(x, e),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: deffunToString(
            getNamePrint(f),
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          ),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: defgenToString(
            getNamePrint(f),
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          ),
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
      Print.dummy(group2(Print.string("["), getNamePrint(x))),
      Print.dummy(group2(getPrint(e), Print.string("]"))),
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
        let print = Group(list{getTermPrint(t), Print.string("\n"), getBlockPrint(b)})
        {
          ann: {print, sourceLocation},
          it: BCons(t, b),
        }
      }
    }
  }
  and printTerm = ({ann: sourceLocation, it: t}: term<sourceLocation>): term<printAnn> => {
    switch t {
    | Exp(it) => {
        let it = printExp(it)
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: Group(list{getPrint(it)}),
          },
        }
      }
    | Def(it) => {
        let it = printDef(it)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: Group(list{getDefinitionPrint(it)}),
          },
        }
      }
    }
  }

  let printOutputlet = o => {
    let rec p = (v: val): string => {
      switch v {
      | Ref(i) => `#${Int.toString(i)}#`
      | Con(c) => constantToString(c)
      | Struct(i, content) => {
          let i = switch i {
          | None => ""
          | Some(i) => `#${Int.toString(i)}=`
          }
          let content = switch content {
          | Lst(es) => `(${concat(" ", es->List.map(p)->List.toArray)})`
          | Vec(es) => `#(${concat(" ", es->List.map(p)->List.toArray)})`
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
    concat(sep, os->List.map(printOutputlet)->List.toArray)
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
                print: getTermPrint(t).it,
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(getTermPrint(t), "\n", getProgramPrint(p)),
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

module TypedSMoLPrinter = {
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
    | Str(s) => JSON.stringify(String(s))
    | Sym(s) => s
    }
  }

  let listToString = ss => {
    open! Print
    pad("(", Print.dummy(concat(" ", ss)), ")")
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
    defvarLike("deffun", Print.dummy(listToString(list{f, ...xs})), b)
  }

  let defgenToString = (f, xs, b) => {
    defvarLike("defgen", Print.dummy(listToString(list{f, ...xs})), b)
  }

  let exprSetToString = (x: print<kindedSourceLocation>, e) => {
    defvarLike("set!", x, e)
  }

  let exprLamToString = (xs, b) => {
    defvarLike("lambda", Print.dummy(listToString(xs)), b)
  }
  let exprGenToString = (xs, b) => {
    defvarLike("generator", Print.dummy(listToString(xs)), b)
  }
  let exprYieldToString = e => Group({list{Print.string("(yield "), e, Print.string(")")}})

  let exprAppToString = (e, es) => {
    listToString(list{e, ...es})
  }

  let beginLike = (op, ts) => {
    Group(list{
      Print.string("("),
      Print.string(op),
      indentBlock(Print.dummy(Print.concat("\n", ts)), 2),
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
        Print.dummy(Print.concat("\n", list{e_cnd, e_thn, e_els})),
        Print.string(")"),
      )->Print.dummy,
    )
    // hcat(Print.string(`(if `), `${concat("\n", list{e_cnd, e_thn, e_els})})`)
  }

  let letLike = (op: string, xes: list<_>, b: _) => {
    let xes = Print.dummy(Print.concat("\n", xes))
    let xes = group(list{Print.string("("), indent(xes, 1), Print.string(")")})
    Group(list{
      Print.dummy(hcat(group(list{Print.string("("), Print.string(op), Print.string(" ")}), xes)),
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

  let rec tyToString = ty => {
    switch ty {
    | Top => "Any"
    | TUni => "Nothing"
    | Num => "Number"
    | Boolean => "Boolean"
    | String => "String"
    | Vecof(ty) => `(Vectorof ${tyToString(ty)})`
    | Listof(ty) => `(Listof ${tyToString(ty)})`
    | Funof(ty) =>
      `(=> ${Array.join(List.map(list{...ty.args, ty.out}, tyToString)->List.toArray, " ")})`
    }
  }

  let symbolToString = ({it, ann}: annotated<symbol, typed<sourceLocation>>) => {
    {
      it,
      ann: {
        sourceLocation: ann.ann,
        print: Print.s`${Print.string(it)} : ${Print.string(tyToString(ann.ty))}`,
      },
    }
  }

  let rec printExp = (
    {it, ann: {ann: sourceLocation}}: expression<typed<sourceLocation>>,
  ): expression<printAnn> => {
    let e = switch it {
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
          ann: exprSetToString(getNamePrint(x), getPrint(e)),
          it: Set(x, e),
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: exprLamToString(xs->List.map(x => getNamePrint(x)), getBlockPrint(b)),
          it: Lam(xs, b),
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: exprGenToString(xs->List.map(x => getNamePrint(x)), getBlockPrint(b)),
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
          ann: exprLetToString(xes->List.map(xe => getBindPrint(xe)), getBlockPrint(b)),
          it: Let(xes, b),
        }
      }
    | Letrec(xes, b) => {
        let xes = xes->List.map(xeToString)
        let b = b->printBlock
        {
          ann: exprLetrecToString(xes->List.map(xe => getBindPrint(xe)), getBlockPrint(b)),
          it: Letrec(xes, b),
        }
      }
    | Cnd(ebs, ob) => {
        let ebs = ebs->List.map(ebToString)
        let ob = ob->obToString
        {
          ann: exprCndToString(
            ebs->List.map(((e, b)) => (getPrint(e), getBlockPrint(b))),
            ob->Option.map(b => getBlockPrint(b)),
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
  and printDef = (
    {ann: {ann: sourceLocation}, it: d}: definition<typed<sourceLocation>>,
  ): definition<printAnn> => {
    let d = switch d {
    | Var(x, e) => {
        let x = symbolToString(x)
        let e = e->printExp
        {
          ann: defvarToString(getNamePrint(x), getPrint(e)),
          it: Var(x, e),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: deffunToString(
            getNamePrint(f),
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          ),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock
        {
          ann: defgenToString(
            getNamePrint(f),
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          ),
          it: GFun(f, xs, b),
        }
      }
    }
    let {ann: print, it} = d
    {ann: {print, sourceLocation}, it}
  }
  and xeToString = ({it: xe, ann: {ann: sourceLocation}}: bind<typed<sourceLocation>>): bind<
    printAnn,
  > => {
    let (x, e) = xe
    let (x, e) = (symbolToString(x), printExp(e))
    let print = hcat(
      Print.dummy(group2(Print.string("["), getNamePrint(x))),
      Print.dummy(group2(getPrint(e), Print.string("]"))),
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
  and printBlock = ({ann: {ann: sourceLocation}, it: b}) => {
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
        let print = Group(list{getTermPrint(t), Print.string("\n"), getBlockPrint(b)})
        {
          ann: {print, sourceLocation},
          it: BCons(t, b),
        }
      }
    }
  }
  and printTerm = ({ann: {ann: sourceLocation}, it: t}: term<typed<sourceLocation>>): term<
    printAnn,
  > => {
    switch t {
    | Exp(it) => {
        let it = printExp(it)
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: Group(list{getPrint(it)}),
          },
        }
      }
    | Def(it) => {
        let it = printDef(it)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: Group(list{getDefinitionPrint(it)}),
          },
        }
      }
    }
  }

  let printOutputlet = o => {
    let rec p = (v: val): string => {
      switch v {
      | Ref(i) => `#${Int.toString(i)}#`
      | Con(c) => constantToString(c)
      | Struct(i, content) => {
          let i = switch i {
          | None => ""
          | Some(i) => `#${Int.toString(i)}=`
          }
          let content = switch content {
          | Lst(es) => `(${concat(" ", es->List.map(p)->List.toArray)})`
          | Vec(es) => `#(${concat(" ", es->List.map(p)->List.toArray)})`
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
    concat(sep, os->List.map(printOutputlet)->List.toArray)
  }

  let printProgramFull = (_insertPrintTopLevel, p: program<sourceLocation>) => {
    let rec print = ({it, ann: {ann: sourceLocation}}: program<typed<sourceLocation>>): program<
      printAnn,
    > => {
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
                print: getTermPrint(t).it,
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(getTermPrint(t), "\n", getProgramPrint(p)),
                  sourceLocation,
                },
              }
            }
          }
        }
      }
    }
    print(inferTypes(p, SourceLocation.toString))
  }

  let printProgram = (insertPrintTopLevel, p) => {
    printProgramFull(insertPrintTopLevel, p).ann.print->Print.toString
  }

  let printStandAloneTerm = _t => {
    raisePrintError("Can't print stand-alone term of typed language")
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
              let rec ie = (e: expression<sourceLocation>): expression<sourceLocation> => {
                let rec ib = (b: block<sourceLocation>): block<sourceLocation> => {
                  ...b,
                  it: switch b.it {
                  | BRet(e) => BRet(ie(e))
                  | BCons(t, b) => BCons(t, ib(b))
                  },
                }
                let ieb = ((e, b)) => (e, ib(b))
                let iebs = ebs => List.map(ebs, ieb)
                let iob = ob => Option.map(ob, ib)
                {
                  ...e,
                  it: switch e.it {
                  | Set(x, e) => Set(x, e)
                  | Bgn(es, e) => Bgn(es, ie(e))
                  | If(ec, et, el) => If(ec, ie(et), ie(el))
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
                  },
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

type consumer = printNode<kindedSourceLocation> => contexted<
  printNode<kindedSourceLocation>,
  (string, printNode<kindedSourceLocation>, string),
>

let asExpr = (e: contexted<'a, 'b>, ctx): 'a => e.expr(ctx)
let asStat = (e: contexted<'a, 'b>, ctx): 'b => e.stat(ctx)

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
    | Str(s) => JSON.stringify(String(s))
    | Sym(s) => s
    }
  }

  let listToString = es => {
    if es->List.some(e => containsNL(e.it)) {
      Group(list{
        Print.string("("),
        indentBlock(Print.dummy(Print.concat(",\n", es)), 2),
        Print.string(")"),
      })
    } else {
      Group(list{Print.string("("), Print.dummy(Print.concat(", ", es)), Print.string(")")})
    }
  }

  let defvarLike = (op, x, e) => {
    Group(list{Print.string(op), x, Print.string(" = "), indent(e, 2)})
  }

  let exprAppToString = (e, es) => {
    group2(e, Print.dummy(listToString(es)))
  }

  let consumeContext: consumer = e => {
    {
      expr: _ => surround("", e, ""),
      stat: ctx =>
        switch ctx {
        | Step => ("", e, ";")
        | Return => ("return ", e, ";")
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

  let consumeContextEscape: consumer = e => {
    {
      stat: ctx =>
        switch ctx {
        | Return => ("", e, ";")
        | ctx => consumeContext(e).stat(ctx)
        },
      expr: consumeContextWrap(e).expr,
    }
  }

  let consumeContextStat: consumer = e => {
    {
      expr: _ =>
        raisePrintError(`${Print.toString(e)} can't be used as a expression in JavaScript`),
      stat: consumeContextVoid(e).stat,
    }
  }

  let exprAppPrmToString = (p: Primitive.t, es: list<bool => expression<printAnn>>) => {
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
        | Equal => raisePrintError("JavaScript has limited support for equality")
        }
        let e1 = e1(true)
        let e2 = e2(true)
        {
          ann: (Print.s`${getPrint(e1)} ${Print.string(os)} ${getPrint(e2)}`)->consumeContext,
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`[ ${getPrint(e1)}, ${getPrint(e2)} ]`)->consumeContext,
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`${getPrint(e1)}[0]`)->consumeContext,
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`${getPrint(e1)}[1]`)->consumeContext,
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}[0] = ${getPrint(e2)}`)->consumeContextStat,
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}[1] = ${getPrint(e2)}`)->consumeContextStat,
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          ann: (
            Print.s`[ ${Print.dummy(Print.concat(`, `, es->List.map(e => getPrint(e))))} ]`
          )->consumeContext,
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}[${getPrint(e2)}]`)->consumeContext,
          it: (VecRef, list{e1, e2}),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          ann: (Print.s`${getPrint(e1)}[${getPrint(e2)}] = ${getPrint(e3)}`)->consumeContextStat,
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`${getPrint(e1)}.length`)->consumeContext,
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`throw ${getPrint(e1)}`)->consumeContextEscape,
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`! ${getPrint(e1)}`)->consumeContextWrap,
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`console.log(${getPrint(e1)})`)->consumeContextVoid,
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`${getPrint(e1)}.next()`)->consumeContextVoid,
          it: (Next, list{e1}),
        }
      }
    | (Cons, _) => raisePrintError("List is not supported by JavaScript")
    | _ =>
      raisePrintError(
        `JavaScript doesn't let you use ${Primitive.toString(p)} on ${Int.toString(
            List.length(es),
          )} parameter(s).`,
      )
    }
  }

  let funLike = (op, x, xs, e) => {
    Print.s`${Print.string(op)} ${Print.dummy(exprAppToString(x, xs))} {${indentBlock(e, 2)}\n}`
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
  let exprYieldToString = e => Print.s`yield ${e}`

  let exprBgnToString = (es, e) => {
    listToString(list{...es, e})
  }

  let ifStat = (cnd, thn, els) => {
    Print.s`if (${cnd}) {${indentBlock(thn, 2)}\n}${switch els {
    | None => Print.s``
    | Some(els) => Print.s` else {${indentBlock(els, 2)}\n}`
    }->Print.dummy}`
  }

  let exprCndToString = (ebs: list<(_, _)>, ob) => {
    let ebs = ebs->List.map(((e, b)) => ifStat(e, b, None)->Print.dummy)
    let ebs = switch ob {
    | None => ebs
    | Some(b) => list{...ebs, (Print.s`{${indentBlock(b, 2)}\n}`)->Print.dummy}
    }
    Print.concat(" else ", ebs)
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    Print.s`${e_cnd} ? ${e_thn} : ${e_els}`
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
    let lift = ({it, ann: print}) => sourceLocation => {
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
          ann: exprSetToString(getNamePrint(x), getPrint(e))->consumeContextStat,
          it: Set(x, e),
        }
      }->lift
    | Lam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprLamToString(
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          )->consumeContextWrap,
          it: Lam(xs, b),
        }
      }->lift
    | GLam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprGenToString(
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          )->consumeContextWrap,
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
      // let es = es->List.map(%todo(""))

      {
        let es = es->List.map(e => b => e->printExp->asExpr(b))
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
            Print.dummy(
              Print.concat("\n", list{...xes->List.map(xe => getBindPrint(xe)), getBlockPrint(b)}),
            ),
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
                  ebs->List.map(((e, b)) => (getPrint(e), getBlockPrint(b))),
                  ob->Option.map(b => getBlockPrint(b)),
                ),
              },
              it: Cnd(ebs, ob),
            },
            "",
          )
        },
      }
    | If(e_cnd, e_thn, e_els) => {
        let e_cnd = e_cnd->printExp
        let e_thn = e_thn->printExp
        let e_els = e_els->printExp
        sourceLocation => {
          expr: ctx => {
            let e_cnd = e_cnd->asExpr(true)
            let e_thn = e_thn->asExpr(true)
            let e_els = e_els->asExpr(true)
            {
              ann: {
                sourceLocation,
                print: exprIfToString(getPrint(e_cnd), getPrint(e_thn), getPrint(e_els))
                ->consumeContextWrap
                ->asExpr(ctx),
              },
              it: If(e_cnd, e_thn, e_els),
            }
          },
          stat: ctx => {
            let e_cnd = e_cnd->asExpr(false)
            let (e_thn, e_thn_print) = {
              let (prefix, e_thn, suffix) = e_thn->asStat(ctx)
              (e_thn, wrap(prefix, getPrint(e_thn), suffix)->Print.dummy)
            }
            let (e_els, e_els_print) = {
              let (prefix, e_els, suffix) = e_els->asStat(ctx)
              (e_els, wrap(prefix, getPrint(e_els), suffix)->Print.dummy)
            }
            (
              "",
              {
                ann: {
                  sourceLocation,
                  print: ifStat(getPrint(e_cnd), e_thn_print, Some(e_els_print)),
                },
                it: If(e_cnd, e_thn, e_els),
              },
              "",
            )
          },
        }
      }
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
            ann: defvarToString(getNamePrint(x), getPrint(e)),
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
            ann: deffunToString(
              getNamePrint(f),
              xs->List.map(x => getNamePrint(x)),
              getBlockPrint(b),
            ),
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
            ann: defgenToString(
              getNamePrint(f),
              xs->List.map(x => getNamePrint(x)),
              getBlockPrint(b),
            ),
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
    let print = defvarToString(getNamePrint(x), getPrint(e))
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
        let t = printTerm(t, Step)
        let b = printBlock(b, context)
        let print = Group(list{getTermPrint(t), Print.string("\n"), getBlockPrint(b)})
        {
          ann: {print, sourceLocation: ann},
          it: BCons(t, b),
        }
      }
    }
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>, ctx): term<printAnn> => {
    switch it {
    | Exp(it) => {
        let (prefix, it, suffix) = printExp(it)->asStat(ctx)
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: wrap(prefix, getPrint(it), suffix),
          },
        }
      }
    | Def(it) => {
        let (prefix, it, suffix) = printDef(it)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: wrap(prefix, getDefinitionPrint(it), suffix),
          },
        }
      }
    }
  }

  let printOutputlet = o => {
    let rec p = (v: val): string => {
      switch v {
      | Ref(i) => `[Circular *${Int.toString(i)}]`
      | Con(c) => constantToString(c)
      | Struct(i, content) => {
          let i = switch i {
          | None => ""
          | Some(i) => `<ref *${Int.toString(i)}> `
          }
          let content = switch content {
          | Lst(_) => raisePrintError("Lists are not supported in JavaScript.")
          | Vec(es) => `[ ${concat(", ", es->List.map(p)->List.toArray)} ]`
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
    concat(sep, os->List.map(printOutputlet)->List.toArray)
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
          let t = printTerm(t, Step)
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
                print: wrap("", getTermPrint(t), ""),
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(getTermPrint(t), "\n", getProgramPrint(p)),
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
    Print.toString(printProgramFull(insertPrintTopLevel, p).ann.print)
  }

  let printStandAloneTerm = ({it}: term<sourceLocation>): string => {
    Print.toString(
      switch it {
      | Def(it) => {
          let (_, it, _) = printDef(it)
          it.ann.print
        }
      | Exp(it) => {
          let (_, it, _) = printExp(it)->asStat(Step)
          it.ann.print
        }
      },
    )
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
  open! Belt

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
    | Str(s) => JSON.stringify(String(s))
    | Sym(s) => s
    }
  }

  let listToString = es => {
    if es->List.some(e => containsNL(e.it)) {
      Group(list{
        Print.string("("),
        indentBlock(Print.dummy(Print.concat(",\n", es)), 4),
        Print.string(")"),
      })
    } else {
      Group(list{Print.string("("), Print.dummy(Print.concat(", ", es)), Print.string(")")})
    }
  }

  let defvarLike = (op, x, e) => {
    Group(list{Print.string(op), x, Print.string(" = "), indent(e, 2)})
  }

  let exprAppToString = (e, es) => {
    group2(e, Print.dummy(listToString(es)))
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

  let consumeContextWrapEvenReturn: consumer = e => {
    {
      expr: ctx =>
        if ctx {
          surround("(", e, ")")
        } else {
          consumeContext(e).expr(ctx)
        },
      stat: ctx =>
        switch ctx {
        | Step => ("", e, "")
        | Return => ("return (", e, ")")
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

  let consumeContextEscapeWrap: consumer = e => {
    {
      stat: ctx =>
        switch ctx {
        | Return => ("", e, "")
        | ctx => consumeContext(e).stat(ctx)
        },
      expr: consumeContextWrap(e).expr,
    }
  }

  let consumeContextStat: consumer = e => {
    {
      expr: _ => raisePrintError(`${Print.toString(e)} can't be used as a expression in Python`),
      stat: consumeContextVoid(e).stat,
    }
  }

  let exprAppPrmToString = (p: Primitive.t, es: list<bool => expression<printAnn>>) => {
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
        | Eq => "is"
        | Gt => ">"
        | Le => "<="
        | Ge => ">="
        | Ne => "!="
        | Equal => "=="
        }
        let e1 = e1(true)
        let e2 = e2(true)
        {
          ann: (Print.s`${getPrint(e1)} ${Print.string(os)} ${getPrint(e2)}`)->consumeContext,
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`[ ${getPrint(e1)}, ${getPrint(e2)} ]`)->consumeContext,
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`${getPrint(e1)}[0]`)->consumeContext,
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`${getPrint(e1)}[1]`)->consumeContext,
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}[0] = ${getPrint(e2)}`)->consumeContextStat,
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}[1] = ${getPrint(e2)}`)->consumeContextStat,
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          ann: (
            Print.s`[ ${Print.dummy(Print.concat(`, `, es->List.map(e => getPrint(e))))} ]`
          )->consumeContext,
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}[${getPrint(e2)}]`)->consumeContext,
          it: (VecRef, list{e1, e2}),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          ann: (Print.s`${getPrint(e1)}[${getPrint(e2)}] = ${getPrint(e3)}`)->consumeContextStat,
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`len(${getPrint(e1)})`)->consumeContext,
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`raise ${getPrint(e1)}`)->consumeContextEscapeWrap,
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`not ${getPrint(e1)}`)->consumeContextWrap,
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`print(${getPrint(e1)})`)->consumeContextVoid,
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`next(${getPrint(e1)})`)->consumeContextVoid,
          it: (Next, list{e1}),
        }
      }
    | (Cons, _) => raisePrintError("List is not supported by Python")
    | _ =>
      raisePrintError(
        `Python doesn't let you use ${Primitive.toString(p)} on ${Int.toString(
            List.length(es),
          )} parameter(s).`,
      )
    }
  }

  let funLike = (op, x, xs, e) => {
    Print.s`${Print.string(op)} ${Print.dummy(exprAppToString(x, xs))}:${indentBlock(e, 4)}`
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
    Print.s`lambda ${xs}: ${b}`
  }
  let exprGenToString = exprLamToString
  let exprYieldToString = e => Print.s`yield ${e}`

  let ifStat = (cnd, thn, els) => {
    Print.s`if ${cnd}:${indentBlock(thn, 2)}${switch els {
    | None => Print.s``
    | Some(els) => Print.s`\nelse:${indentBlock(els, 2)}`
    }->Print.dummy}`
  }

  let exprCndToString = (ebs: list<(_, _)>, ob) => {
    if ebs == list{} {
      raisePrintError("`else`-only conditional is not supported by Python.")
    }
    let ebs = ebs->List.map(((e, b)) => ifStat(e, b, None)->Print.dummy)
    let ebs = switch ob {
    | None => ebs
    | Some(b) => list{...ebs, (Print.s`se:${indentBlock(b, 2)}`)->Print.dummy}
    }
    Print.concat("\nel", ebs)
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    Print.s`${e_thn} if ${e_cnd} else ${e_els}`
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
    let lift = ({it, ann: print}) => sourceLocation => {
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
          ann: exprSetToString(getNamePrint(x), getPrint(e))->consumeContextStat,
          it: Set(x, e),
        }
      }->lift
    | Lam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printLamBody(xs, env)
        {
          ann: exprLamToString(
            Print.concat(",", xs->List.map(getNamePrint))->Print.dummy,
            getBlockPrint(b),
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
            Print.concat(",", xs->List.map(getNamePrint))->Print.dummy,
            getBlockPrint(b),
          )->consumeContextWrap,
          it: GLam(xs, b),
        }
      }->lift
    | Yield(e) =>
      {
        let e = e->printExp(env)->asExpr(false)
        {
          ann: exprYieldToString(getPrint(e))->consumeContextWrapEvenReturn,
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
                  ebs->List.map(((e, b)) => (getPrint(e), getBlockPrint(b))),
                  ob->Option.map(b => getBlockPrint(b)),
                ),
              },
              it: Cnd(ebs, ob),
            },
            "",
          )
        },
      }
    | If(e_cnd, e_thn, e_els) => {
        let e_cnd = e_cnd->printExp(env)
        let e_thn = e_thn->printExp(env)
        let e_els = e_els->printExp(env)
        sourceLocation => {
          expr: ctx => {
            let e_cnd = e_cnd->asExpr(true)
            let e_thn = e_thn->asExpr(true)
            let e_els = e_els->asExpr(true)
            {
              ann: {
                sourceLocation,
                print: exprIfToString(getPrint(e_cnd), getPrint(e_thn), getPrint(e_els))
                ->consumeContextWrap
                ->asExpr(ctx),
              },
              it: If(e_cnd, e_thn, e_els),
            }
          },
          stat: ctx => {
            let e_cnd = e_cnd->asExpr(false)
            let (e_thn, e_thn_print) = {
              let (prefix, e_thn, suffix) = e_thn->asStat(ctx)
              (e_thn, wrap(prefix, getPrint(e_thn), suffix)->Print.dummy)
            }
            let (e_els, e_els_print) = {
              let (prefix, e_els, suffix) = e_els->asStat(ctx)
              (e_els, wrap(prefix, getPrint(e_els), suffix)->Print.dummy)
            }
            (
              "",
              {
                ann: {
                  sourceLocation,
                  print: ifStat(getPrint(e_cnd), e_thn_print, Some(e_els_print)),
                },
                it: If(e_cnd, e_thn, e_els),
              },
              "",
            )
          },
        }
      }
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
            ann: defvarToString(getNamePrint(x), getPrint(e)),
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
            ann: deffunToString(
              getNamePrint(f),
              xs->List.map(x => getNamePrint(x)),
              getBlockPrint(b),
            ),
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
            ann: defgenToString(
              getNamePrint(f),
              xs->List.map(x => getNamePrint(x)),
              getBlockPrint(b),
            ),
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
        let t = printTerm(t, env, Step)
        let b = b->printBlockHelper(env, context)
        let print = Group(list{getTermPrint(t), Print.string("\n"), getBlockPrint(b)})
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
        getBlockPrint(b),
      },
    )
    {
      ann: {print, sourceLocation: b.ann.sourceLocation},
      it: b.it,
    }
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>, env, ctx): term<printAnn> => {
    switch it {
    | Exp(it) => {
        let (prefix, it, suffix) = printExp(it, env)->asStat(ctx)
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: wrap(prefix, getPrint(it), suffix),
          },
        }
      }
    | Def(it) => {
        let (prefix, it, suffix) = printDef(it, env)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: wrap(prefix, getDefinitionPrint(it), suffix),
          },
        }
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
          | Vec(es) => `[${concat(", ", es->List.map(p)->List.toArray)}]`
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
    concat(sep, os->List.map(printOutputlet)->List.toArray)
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
          let t = printTerm(t, env, Step)
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
                print: Print.s`${getTermPrint(t)}`,
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(getTermPrint(t), "\n", getProgramPrint(p)),
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
    Print.toString(printProgramFull(insertPrintTopLevel, p).ann.print)
  }

  let printStandAloneTerm = ({it}: term<sourceLocation>): string => {
    let globalEnv = G(HashSet.String.fromArray([]))
    Print.toString(
      switch it {
      | Def(it) => {
          let (_, it, _) = printDef(it, globalEnv)
          it.ann.print
        }
      | Exp(it) => {
          let (_, it, _) = printExp(it, globalEnv)->asStat(Step)
          it.ann.print
        }
      },
    )
  }
}

module PCPrinter: Printer = {
  let printName = x => x

  let constantToString = c => {
    switch c {
    | Uni => "Unit"
    | Nil => "Nil"
    | Num(n) => Float.toString(n)
    | Lgc(l) =>
      if l {
        "True"
      } else {
        "False"
      }
    | Str(s) => JSON.stringify(String(s))
    | Sym(s) => s
    }
  }

  let listToString = es => {
    if es->List.some(e => containsNL(e.it)) {
      Group(list{
        Print.string("("),
        indentBlock(Print.dummy(Print.concat(",\n", es)), 2),
        Print.string(")"),
      })
    } else {
      Group(list{Print.string("("), Print.dummy(Print.concat(", ", es)), Print.string(")")})
    }
  }

  let defvarLike = (op, x, e) => {
    Group(list{Print.string(op), x, Print.string(" = "), indent(e, 2)})
  }

  let exprAppToString = (e, es) => {
    group2(e, Print.dummy(listToString(es)))
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

  let consumeContextEscapeWrap: consumer = e => {
    {
      stat: ctx =>
        switch ctx {
        | Return => ("", e, "")
        | ctx => consumeContext(e).stat(ctx)
        },
      expr: consumeContextWrap(e).expr,
    }
  }

  let consumeContextWrapVoid: consumer = e => {
    {
      stat: ctx =>
        switch ctx {
        | Return => ("", e, "\nreturn")
        | ctx => consumeContext(e).stat(ctx)
        },
      expr: consumeContextWrap(e).expr,
    }
  }

  let exprAppPrmToString = (p: Primitive.t, es: list<bool => expression<printAnn>>) => {
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
        | Eq => "==="
        | Gt => ">"
        | Le => "<="
        | Ge => ">="
        | Ne => "!="
        | Equal => "=="
        }
        let e1 = e1(true)
        let e2 = e2(true)
        {
          ann: (Print.s`${getPrint(e1)} ${Print.string(os)} ${getPrint(e2)}`)->consumeContext,
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`vec[${getPrint(e1)}, ${getPrint(e2)}]`)->consumeContext,
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`${getPrint(e1)}[0]`)->consumeContext,
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`${getPrint(e1)}[1]`)->consumeContext,
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}[0] = ${getPrint(e2)}`)->consumeContextWrapVoid,
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}[1] = ${getPrint(e2)}`)->consumeContextWrapVoid,
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          ann: (
            Print.s`vec[${Print.dummy(Print.concat(`, `, es->List.map(e => getPrint(e))))}]`
          )->consumeContext,
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}[${getPrint(e2)}]`)->consumeContext,
          it: (VecRef, list{e1, e2}),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          ann: (
            Print.s`${getPrint(e1)}[${getPrint(e2)}] = ${getPrint(e3)}`
          )->consumeContextWrapVoid,
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`length(${getPrint(e1)})`)->consumeContext,
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`raise ${getPrint(e1)}`)->consumeContextEscapeWrap,
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`! ${getPrint(e1)}`)->consumeContextWrap,
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`print(${getPrint(e1)})`)->consumeContextVoid,
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`next(${getPrint(e1)})`)->consumeContextVoid,
          it: (Next, list{e1}),
        }
      }
    | (Cons, _) => raisePrintError("List is not supported by Pseudo")
    | _ =>
      raisePrintError(
        `Pseudo doesn't let you use ${Primitive.toString(p)} on ${Int.toString(
            List.length(es),
          )} parameter(s).`,
      )
    }
  }

  let funLike = (op, x, xs, e) => {
    Print.s`${Print.string(op)} ${Print.dummy(exprAppToString(x, xs))}:${indentBlock(e, 2)}\nend`
  }

  let defvarToString = (x, e) => {
    defvarLike("let ", x, e)
  }

  let deffunToString = (f, xs, b) => {
    funLike("fun", f, xs, b)
  }

  let defgenToString = (f, xs, b) => {
    funLike("gen fun", f, xs, b)
  }

  let exprSetToString = (x, e) => {
    defvarLike("", x, e)
  }

  let exprLamToString = (xs, b) => {
    funLike("lam", Print.string(""), xs, b)
  }
  let exprGenToString = (xs, b) => {
    funLike("gen lam", Print.string(""), xs, b)
  }
  let exprYieldToString = e => Print.s`yield ${e}`

  let exprBgnToString = (es, e) => {
    listToString(list{...es, e})
  }

  let ifStat = (cnd, thn, els) => {
    Print.s`if ${cnd}:${indentBlock(thn, 2)}${switch els {
    | None => Print.s``
    | Some(els) => Print.s`\nelse:${indentBlock(els, 2)}\nend`
    }->Print.dummy}`
  }

  let exprCndToString = (ebs: list<(_, _)>, ob) => {
    if ebs == list{} {
      raisePrintError("`else`-only conditional is not supported by Pseudo.")
    }
    let ebs = ebs->List.map(((e, b)) => ifStat(e, b, None)->Print.dummy)
    let ebs = Print.concat("\nelse ", ebs)
    group2(
      ebs->Print.dummy,
      switch ob {
      | None => Print.string("\nend")
      | Some(b) => (Print.s`\nelse:${indentBlock(b, 2)}\nend`)->Print.dummy
      },
    )
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    Print.s`if ${e_cnd} then ${e_thn} else ${e_els}`
  }

  let letLike = (op: string, xes: list<_>, b: _) => {
    let xes = Print.dummy(Print.concat("\n", xes))
    let xes = group(list{Print.string("("), indent(xes, 1), Print.string(")")})
    Group(list{
      Print.dummy(hcat(group(list{Print.string("("), Print.string(op), Print.string(" ")}), xes)),
      indentBlock(b, 2),
      Print.string(")"),
    })
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
        print: Plain(printName(it)),
      },
    }
  }

  let rec printExp = ({it, ann: sourceLocation}) => {
    let lift = ({it, ann: print}) => sourceLocation => {
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
          ann: exprSetToString(getNamePrint(x), getPrint(e))->consumeContextWrapVoid,
          it: Set(x, e),
        }
      }->lift
    | Lam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprLamToString(
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          )->consumeContextWrap,
          it: Lam(xs, b),
        }
      }->lift
    | GLam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprGenToString(
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          )->consumeContextWrap,
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
        let es = es->List.map(e => b => e->printExp->asExpr(b))
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
    | Let(xes, b) => {
        let xes = xes->List.map(xeToString)
        sourceLocation => {
          {
            expr: _ => {
              let b = b->printBlock(Step)
              {
                ann: {
                  print: exprLetToString(xes->List.map(xe => getBindPrint(xe)), getBlockPrint(b)),
                  sourceLocation,
                },
                it: Let(xes, b),
              }
            },
            stat: ctx => {
              let b = b->printBlock(ctx)
              (
                "",
                {
                  ann: {
                    print: exprLetToString(xes->List.map(xe => getBindPrint(xe)), getBlockPrint(b)),
                    sourceLocation,
                  },
                  it: Let(xes, b),
                },
                "",
              )
            },
          }
        }
      }
    | Letrec(xes, b) => {
        let xes = xes->List.map(xeToString)
        sourceLocation => {
          {
            expr: _ => {
              let b = b->printBlock(Step)
              {
                ann: {
                  print: exprLetrecToString(
                    xes->List.map(xe => getBindPrint(xe)),
                    getBlockPrint(b),
                  ),
                  sourceLocation,
                },
                it: Letrec(xes, b),
              }
            },
            stat: ctx => {
              let b = b->printBlock(ctx)
              (
                "",
                {
                  ann: {
                    print: exprLetrecToString(
                      xes->List.map(xe => getBindPrint(xe)),
                      getBlockPrint(b),
                    ),
                    sourceLocation,
                  },
                  it: Letrec(xes, b),
                },
                "",
              )
            },
          }
        }
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
                  ebs->List.map(((e, b)) => (getPrint(e), getBlockPrint(b))),
                  ob->Option.map(b => getBlockPrint(b)),
                ),
              },
              it: Cnd(ebs, ob),
            },
            "",
          )
        },
      }
    | If(e_cnd, e_thn, e_els) => {
        let e_cnd = e_cnd->printExp
        let e_thn = e_thn->printExp
        let e_els = e_els->printExp
        sourceLocation => {
          expr: ctx => {
            let e_cnd = e_cnd->asExpr(true)
            let e_thn = e_thn->asExpr(true)
            let e_els = e_els->asExpr(true)
            {
              ann: {
                sourceLocation,
                print: exprIfToString(getPrint(e_cnd), getPrint(e_thn), getPrint(e_els))
                ->consumeContextWrap
                ->asExpr(ctx),
              },
              it: If(e_cnd, e_thn, e_els),
            }
          },
          stat: ctx => {
            let e_cnd = e_cnd->asExpr(false)
            let (e_thn, e_thn_print) = {
              let (prefix, e_thn, suffix) = e_thn->asStat(ctx)
              (e_thn, wrap(prefix, getPrint(e_thn), suffix)->Print.dummy)
            }
            let (e_els, e_els_print) = {
              let (prefix, e_els, suffix) = e_els->asStat(ctx)
              (e_els, wrap(prefix, getPrint(e_els), suffix)->Print.dummy)
            }
            (
              "",
              {
                ann: {
                  sourceLocation,
                  print: ifStat(getPrint(e_cnd), e_thn_print, Some(e_els_print)),
                },
                it: If(e_cnd, e_thn, e_els),
              },
              "",
            )
          },
        }
      }
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
            ann: defvarToString(getNamePrint(x), getPrint(e)),
            it: Var(x, e),
          },
          "",
        )
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        (
          "",
          {
            ann: deffunToString(
              getNamePrint(f),
              xs->List.map(x => getNamePrint(x)),
              getBlockPrint(b),
            ),
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
            ann: defgenToString(
              getNamePrint(f),
              xs->List.map(x => getNamePrint(x)),
              getBlockPrint(b),
            ),
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
    let print = defvarToString(getNamePrint(x), getPrint(e))
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
        let t = printTerm(t, Step)
        let b = printBlock(b, context)
        let print = Group(list{getTermPrint(t), Print.string("\n"), getBlockPrint(b)})
        {
          ann: {print, sourceLocation: ann},
          it: BCons(t, b),
        }
      }
    }
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>, ctx): term<printAnn> => {
    switch it {
    | Exp(it) => {
        let (prefix, it, suffix) = printExp(it)->asStat(ctx)
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: wrap(prefix, getPrint(it), suffix),
          },
        }
      }
    | Def(it) => {
        let (prefix, it, suffix) = printDef(it)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: wrap(prefix, getDefinitionPrint(it), suffix),
          },
        }
      }
    }
  }
  let printOutputlet = o => {
    let rec p = (v: val): string => {
      switch v {
      | Ref(i) => `#${Int.toString(i)}#`
      | Con(c) => constantToString(c)
      | Struct(i, content) => {
          let i = switch i {
          | None => ""
          | Some(i) => `#${Int.toString(i)}#=`
          }
          let content = switch content {
          | Lst(_) => raisePrintError("Lists are not supported in Pseudo.")
          | Vec(es) => `vec[${concat(", ", es->List.map(p)->List.toArray)}]`
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
    concat(sep, os->List.map(printOutputlet)->List.toArray)
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
          let t = printTerm(t, Step)
          let prefix = ""
          let suffix = ""
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
                print: wrap(prefix, getTermPrint(t), suffix),
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(
                    Print.dummy(wrap(prefix, getTermPrint(t), suffix)),
                    "\n",
                    getProgramPrint(p),
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
    Print.toString(printProgramFull(insertPrintTopLevel, p).ann.print)
  }

  let printStandAloneTerm = ({it}: term<sourceLocation>): string => {
    Print.toString(
      switch it {
      | Def(it) => {
          let (_, it, _) = printDef(it)
          it.ann.print
        }
      | Exp(it) => {
          let (_, it, _) = printExp(it)->asStat(Step)
          it.ann.print
        }
      },
    )
  }
}

module SCPrinter: Printer = {
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
    | Nil => raisePrintError("Lists are not supported in Scala.")
    | Num(n) => Float.toString(n)
    | Lgc(l) =>
      if l {
        "true"
      } else {
        "false"
      }
    | Str(s) => JSON.stringify(String(s))
    | Sym(s) => s
    }
  }

  let listToString = es => {
    if es->List.some(e => containsNL(e.it)) {
      Group(list{
        Print.string("("),
        indentBlock(Print.dummy(Print.concat(",\n", es)), 2),
        Print.string(")"),
      })
    } else {
      Group(list{Print.string("("), Print.dummy(Print.concat(", ", es)), Print.string(")")})
    }
  }

  let defvarLike = (op, x, e) => {
    Group(list{Print.string(op), x, Print.string(" = "), indent(e, 2)})
  }

  let exprAppToString = (e, es) => {
    group2(
      e,
      if es == list{} {
        Print.string("")
      } else {
        Print.dummy(listToString(es))
      },
    )
  }

  let containsVarMutation = ref(false)
  let containsVecMutation = ref(false)

  let consumeContext: consumer = e => {
    {
      expr: _ => surround("", e, ""),
      stat: ctx =>
        switch ctx {
        | Step => ("", e, "")
        | Return => ("", e, "")
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
        | Return => ("", e, "")
        | ctx => consumeContext(e).stat(ctx)
        },
      expr: consumeContext(e).expr,
    }
  }

  let consumeContextEscape: consumer = e => {
    {
      stat: ctx =>
        switch ctx {
        | Return => ("", e, "")
        | ctx => consumeContext(e).stat(ctx)
        },
      expr: consumeContextWrap(e).expr,
    }
  }

  let exprAppPrmToString = (p: Primitive.t, es: list<bool => expression<printAnn>>) => {
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
        | Eq => "eq"
        | Gt => ">"
        | Le => "<="
        | Ge => ">="
        | Ne => "!="
        | Equal => "=="
        }
        let e1 = e1(true)
        let e2 = e2(true)
        {
          ann: (Print.s`${getPrint(e1)} ${Print.string(os)} ${getPrint(e2)}`)->consumeContext,
          it: (Cmp(o), list{e1, e2}),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        let vecKeyword = if containsVecMutation.contents || containsVarMutation.contents {
          "Buffer"
        } else {
          ""
        }
        {
          ann: (
            Print.s`${Print.string(vecKeyword)}(${getPrint(e1)}, ${getPrint(e2)})`
          )->consumeContext,
          it: (PairNew, list{e1, e2}),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`${getPrint(e1)}(0)`)->consumeContext,
          it: (PairRefLeft, list{e1}),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`${getPrint(e1)}(1)`)->consumeContext,
          it: (PairRefRight, list{e1}),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}(0) = ${getPrint(e2)}`)->consumeContextVoid,
          it: (PairSetLeft, list{e1, e2}),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}(1) = ${getPrint(e2)}`)->consumeContextVoid,
          it: (PairSetRight, list{e1, e2}),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        let vecKeyword = if containsVecMutation.contents || containsVarMutation.contents {
          "Buffer"
        } else {
          ""
        }
        {
          ann: (
            Print.s`${Print.string(vecKeyword)}(${Print.dummy(
              Print.concat(`, `, es->List.map(e => getPrint(e))),
            )})`
          )->consumeContext,
          it: (VecNew, es),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          ann: (Print.s`${getPrint(e1)}(${getPrint(e2)})`)->consumeContext,
          it: (VecRef, list{e1, e2}),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          ann: (Print.s`${getPrint(e1)}(${getPrint(e2)}) = ${getPrint(e3)}`)->consumeContextVoid,
          it: (VecSet, list{e1, e2, e3}),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`${getPrint(e1)}.length`)->consumeContext,
          it: (VecLen, list{e1}),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`throw ${getPrint(e1)}`)->consumeContextEscape,
          it: (Err, list{e1}),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          ann: (Print.s`! ${getPrint(e1)}`)->consumeContextWrap,
          it: (Not, list{e1}),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`println(${getPrint(e1)})`)->consumeContextVoid,
          it: (Print, list{e1}),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          ann: (Print.s`next(${getPrint(e1)})`)->consumeContextVoid,
          it: (Next, list{e1}),
        }
      }
    | (Cons, _) => raisePrintError("List is not supported by Scala")
    | _ =>
      raisePrintError(
        `Scala doesn't let you use ${Primitive.toString(p)} on ${Int.toString(
            List.length(es),
          )} parameter(s).`,
      )
    }
  }

  let funLike = (op, x, xs, e) => {
    Print.s`${Print.string(op)} ${Print.dummy(
      exprAppToString(x, xs->List.map(x => Print.dummy(Print.s`${x} : Int`))),
    )} =${indentBlock(e, 2)}`
  }

  let defvarToString = (x, e) => {
    let keyword = if containsVarMutation.contents {
      "var"
    } else {
      "val"
    }
    defvarLike(`${keyword} `, x, e)
  }

  let deffunToString = (f, xs, b) => {
    funLike("def", f, xs, b)
  }

  let exprSetToString = (x, e) => {
    defvarLike("", x, e)
  }

  let exprLamToString = (xs, b) => {
    let xs = Print.dummy(
      Print.concat(", ", xs->List.map(x => Print.dummy(group2(x, Print.string(" : Int"))))),
    )
    Print.s`(${xs}) =>${indentBlock(b, 2)}`
  }
  let exprGenToString = (_xs, _b) => {
    raisePrintError("generators are not supported yet in Scala translation.")
  }

  let exprBgnToString = (es, e) => {
    listToString(list{...es, e})
  }

  let ifStat = (cnd, thn, els) => {
    Print.s`if (${cnd}) {${indentBlock(thn, 2)}\n}${switch els {
    | None => Print.s``
    | Some(els) => Print.s` else {${indentBlock(els, 2)}\n}`
    }->Print.dummy}`
  }

  let exprCndToString = (ebs: list<(_, _)>, ob) => {
    let ebs = ebs->List.map(((e, b)) => ifStat(e, b, None)->Print.dummy)
    let ebs = switch ob {
    | None => ebs
    | Some(b) => list{...ebs, (Print.s`{${indentBlock(b, 2)}\n}`)->Print.dummy}
    }
    Print.concat(" else ", ebs)
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    exprCndToString(list{(e_cnd, e_thn)}, Some(e_els))
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
    let lift = ({it, ann: print}) => sourceLocation => {
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
          ann: exprSetToString(getNamePrint(x), getPrint(e))->consumeContextVoid,
          it: Set(x, e),
        }
      }->lift
    | Lam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprLamToString(
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          )->consumeContextWrap,
          it: Lam(xs, b),
        }
      }->lift
    | GLam(xs, b) =>
      {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        {
          ann: exprGenToString(
            xs->List.map(x => getNamePrint(x)),
            getBlockPrint(b),
          )->consumeContextWrap,
          it: GLam(xs, b),
        }
      }->lift
    | Yield(_) => raisePrintError("Generators are not supported by Scala.")
    | AppPrm(p, es) =>
      {
        let es = es->List.map(e => b => e->printExp->asExpr(b))
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
    | Let(_xes, _b) => raisePrintError("let-expressions are not supported by Scala")
    | Letrec(_xes, _b) => raisePrintError("letrec-expressions are not supported by Scala")
    | Cnd(ebs, ob) =>
      sourceLocation => {
        expr: _ =>
          raisePrintError(
            "Multi-armed conditionals in Scala is not supported by the translator yet.",
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
                  ebs->List.map(((e, b)) => (getPrint(e), getBlockPrint(b))),
                  ob->Option.map(b => getBlockPrint(b)),
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
        let e_cnd = e_cnd->printExp->asExpr(false)
        let e_thn = e_thn->printExp->asExpr(false)
        let e_els = e_els->printExp->asExpr(false)
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
            ann: defvarToString(getNamePrint(x), getPrint(e)),
            it: Var(x, e),
          },
          "",
        )
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Return)
        (
          "",
          {
            ann: deffunToString(
              getNamePrint(f),
              xs->List.map(x => getNamePrint(x)),
              getBlockPrint(b),
            ),
            it: Fun(f, xs, b),
          },
          "",
        )
      }
    | GFun(_f, _xs, _b) => raisePrintError("Generators are not supported by Scala")
    }
    let {ann: print, it} = d
    (prefix, {ann: {print, sourceLocation}, it}, suffix)
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
        let t = printTerm(t, Step)
        let b = printBlock(b, context)
        let print = Group(list{getTermPrint(t), Print.string("\n"), getBlockPrint(b)})
        {
          ann: {print, sourceLocation: ann},
          it: BCons(t, b),
        }
      }
    }
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>, ctx): term<printAnn> => {
    switch it {
    | Exp(it) => {
        let (prefix, it, suffix) = printExp(it)->asStat(ctx)
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: wrap(prefix, getPrint(it), suffix),
          },
        }
      }
    | Def(it) => {
        let (prefix, it, suffix) = printDef(it)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: wrap(prefix, getDefinitionPrint(it), suffix),
          },
        }
      }
    }
  }

  let printOutputlet = o => {
    let rec p = (v: val): string => {
      switch v {
      | Ref(_) => raisePrintError("Can't print circular data structure in Scala")
      | Con(c) => constantToString(c)
      | Struct(i, content) => {
          let i = switch i {
          | None => ""
          | Some(_) => raisePrintError("Can't print circular data structure in Scala")
          }
          let content = switch content {
          | Lst(_es) => raisePrintError("Lists are not supported in Scala.")
          | Vec(es) => `Buffer(${concat(", ", es->List.map(p)->List.toArray)})`
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
    concat(sep, os->List.map(printOutputlet)->List.toArray)
  }

  let printProgramFull = (insertPrintTopLevel, p) => {
    let p = if insertPrintTopLevel {
      insertTopLevelPrint(p)
    } else {
      p
    }
    let s = SMoLPrinter.printProgram(insertPrintTopLevel, p)
    containsVarMutation := Js.String.includes("(set!", s)
    containsVecMutation :=
      Js.String.includes("vec-set!", s) ||
      Js.String.includes("set-left!", s) ||
      Js.String.includes("set-right!", s)

    let rec print = ({it, ann: sourceLocation}: program<sourceLocation>): program<printAnn> => {
      switch it {
      | PNil => {it: PNil, ann: {print: Group(list{}), sourceLocation}}
      | PCons(t, p) => {
          let t = printTerm(t, Step)
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
                print: Print.s`${getTermPrint(t)}`,
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(getTermPrint(t), "\n", getProgramPrint(p)),
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
    Print.toString(printProgramFull(insertPrintTopLevel, p).ann.print)
  }

  let printStandAloneTerm = ({it}: term<sourceLocation>): string => {
    Print.toString(
      switch it {
      | Def(it) => {
          let (_, it, _) = printDef(it)
          it.ann.print
        }
      | Exp(it) => {
          let (_, it, _) = printExp(it)->asStat(Step)
          it.ann.print
        }
      },
    )
  }
}

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
      switch P.printStandAloneTerm(programAsTerm(p)) {
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
module TypedSMoLTranslator = MakeTranslator(TypedSMoLPrinter)
module JSTranslator = MakeTranslator(JSPrinter)
module PYTranslator = MakeTranslator(PYPrinter)
module PCTranslator = MakeTranslator(PCPrinter)
module SCTranslator = MakeTranslator(SCPrinter)
