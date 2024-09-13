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
  type t<'id> = print<'id>
  let toSourceMap = (t, stringOfKey) => {
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
        Map.set(hMap, stringOfKey(ann), {begin, end})
      })
    }
    f(t)
    hMap
  }
  let rec toString = t => {
    switch t.it {
    | Plain(s) => s
    | Group(ts) =>
      concat(
        "",
        ts
        ->List.map(toString)
        ->List.toArray,
      )
    }
  }
  let rec map = f => t => {
    {
      ...t,
      it: switch t.it {
      | Plain(s) => Plain(f(s))
      | Group(ts) => Group(ts->List.map(map(f)))
      },
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

  let wrap = (prefix: string, it, suffix: string) => {
    let prefix = string(prefix)
    let suffix = string(suffix)
    Group(list{prefix, it, suffix})
  }

  let dummy = it => {it, ann: None}

  let rec includes = (t, s) => {
    switch t.it {
    | Plain(it) => String.includes(it, s)
    | Group(ts) => ts->List.some(t => includes(t, s))
    }
  }

  let s = (strings: array<string>, parameters: array<print<'id>>): printNode<'id> => {
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

let containsNL = it => {
  Print.includes(it, "\n")
}
let group = ss => {it: Group(ss), ann: None}

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
    | NumEq
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
    | Cmp(NumEq) => "="
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
        makeAppPrm(ann, Cmp(NumEq), es)
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
    switch src->SExpr.fromString(~ignoreLangLine=true)->List.map(parseTerm) {
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

type kindedSourceLocation = {
  nodeKind: nodeKind,
  sourceLocation: sourceLocation,
}
type printAnn = {sourceLocation: sourceLocation, print: print<kindedSourceLocation>}

module type Printer = {
  let printName: string => string
  let printOutputlet: outputlet => string
  let printOutput: (~sep: string=?, list<outputlet>) => string
  let printStandAloneTerm: term<sourceLocation> => string
  let printProgram: (bool, program<sourceLocation>) => string
  let printProgramFull: (bool, program<sourceLocation>) => program<printAnn>
}

let indent = (t, i) => {
  let pad = Js.String.repeat(i, " ")
  Print.map(s => Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s))(t)
}
let indentBlock = (s, i) => indent(group(list{Print.string("\n"), s}), i)
let hcat = (p1, s, p2) => {
  Group(list{p1, Print.string(s), indent(p2, String.length(Print.toString(p1)) + String.length(s))})
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

  let plainList = es => {
    Print.s`(${Print.concat(" ", es)->Print.dummy})`
  }

  let bindsLikeList = es => {
    Print.s`(${indentBlock(Print.concat("\n", es)->Print.dummy, 1)})`
  }

  let appLikeList = (e1, es) => {
    //IF es contains no NL
    //  (e1 e2 e3)
    //IF contains some NL in e2 or e3 or ...
    //  (e1 e2
    //      ...
    //      e3
    //      ...)
    if List.some(es, containsNL) {
      Print.s`(${hcat(e1, " ", Print.concat("\n", es)->Print.dummy)->Print.dummy})`
    } else {
      Print.s`(${e1} ${Print.concat(" ", es)->Print.dummy})`
    }
  }

  let beginLikeList = (e1, es) => {
    //  (e1
    //    e2
    //    ...
    //    e3
    //    ...)
    Print.s`(${e1}${indentBlock(Print.concat("\n", es)->Print.dummy, 2)})`
  }

  let letLikeList = (e1, e2, e3) => {
    //  (e1 e2
    //      ...
    //    e3
    //    ...)
    Print.s`(${hcat(e1, " ", e2)->Print.dummy}${indentBlock(e3, 2)})`
  }

  let defvarLikeList = (e1, e2, e3) => {
    //IF contains no NL
    //  (e1 e2 e3)
    //IF contains some NL in e2 or e3
    //  (e1 e2
    //      ...
    //    e3
    //    ...)
    if containsNL(e2) || containsNL(e3) {
      letLikeList(e1, e2, e3)
    } else {
      Print.s`(${e1} ${e2} ${e3})`
    }
  }
  let defvarToString = (x, e) => {
    defvarLikeList(Print.string("defvar"), x, e)
  }

  let exprAppToString = (e, es) => {
    appLikeList(e, es)
  }

  let deffunToString = (f, xs, b) => {
    letLikeList(Print.string("deffun"), appLikeList(f, xs)->Print.dummy, b)
  }

  let defgenToString = (f, xs, b) => {
    letLikeList(Print.string("defgen"), appLikeList(f, xs)->Print.dummy, b)
  }

  let exprSetToString = (x: print<kindedSourceLocation>, e) => {
    defvarLikeList(Print.string("set!"), x, e)
  }

  let exprLamToString = (xs, b) => {
    defvarLikeList(Print.string("lambda"), Print.dummy(plainList(xs)), b)
  }
  let exprGLamToString = (xs, b) => {
    defvarLikeList(Print.string("generator"), Print.dummy(plainList(xs)), b)
  }

  let exprYieldToString = e => appLikeList(Print.string("yield"), list{e})

  let exprBgnToString = (es, e) => {
    beginLikeList(Print.string("begin"), list{...es, e})
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
    beginLikeList(Print.string("cond"), ebs)
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    appLikeList(Print.string("if"), list{e_cnd, e_thn, e_els})
  }

  let exprLetToString = (xes, b) => {
    letLikeList(Print.string("let"), xes, b)
  }
  let exprLetrecToString = (xes, b) => {
    letLikeList(Print.string("letrec"), xes, b)
  }

  let symbolToString = ({it, ann: sourceLocation}) => {
    {
      it,
      ann: {
        sourceLocation,
        print: {it: Plain(it), ann: Some({nodeKind: Name, sourceLocation})},
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
          ann: exprGLamToString(xs->List.map(x => x.ann.print), b.ann.print),
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
          ann: exprAppToString(Print.string(Primitive.toString(p)), es->List.map(e => e.ann.print)),
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
          ann: exprLetToString(
            xes->List.map(xe => xe.ann.print)->bindsLikeList->Print.dummy,
            b.ann.print,
          ),
          it: Let(xes, b),
        }
      }
    | Letrec(xes, b) => {
        let xes = xes->List.map(xeToString)
        let b = b->printBlock
        {
          ann: exprLetrecToString(
            xes->List.map(xe => xe.ann.print)->bindsLikeList->Print.dummy,
            b.ann.print,
          ),
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
    {
      ann: {print: {it: print, ann: Some({nodeKind: Expression, sourceLocation})}, sourceLocation},
      it,
    }
  }
  and printDef = ({ann: sourceLocation, it: d}: definition<sourceLocation>): definition<
    printAnn,
  > => {
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
    {
      ann: {
        print: {
          it: print,
          ann: Some({
            nodeKind: Definition,
            sourceLocation,
          }),
        },
        sourceLocation,
      },
      it,
    }
  }
  and xeToString = ({it: xe, ann: sourceLocation}: bind<sourceLocation>): bind<printAnn> => {
    let (x, e) = xe
    let (x, e) = (symbolToString(x), printExp(e))
    let print = Print.s`[${hcat(x.ann.print, " ", e.ann.print)->Print.dummy}]`
    {
      it: (x, e),
      ann: {
        print: {
          it: print,
          ann: Some({
            nodeKind: Bind,
            sourceLocation,
          }),
        },
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
    let annPrint = p => {
      it: p,
      ann: Some({
        nodeKind: Block,
        sourceLocation,
      }),
    }
    switch b {
    | BRet(e) => {
        let e = printExp(e)
        {
          ann: {
            print: (Print.s`${e.ann.print}`)->annPrint,
            sourceLocation: e.ann.sourceLocation,
          },
          it: BRet(e),
        }
      }
    | BCons(t, b) => {
        let t = printTerm(t)
        let b = printBlock(b)
        let print = Group(list{t.ann.print, Print.string("\n"), b.ann.print})->annPrint
        {
          ann: {print, sourceLocation},
          it: BCons(t, b),
        }
      }
    }
  }
  and printTerm = ({ann: sourceLocation, it: t}: term<sourceLocation>): term<printAnn> => {
    let annPrint = p => {
      it: p,
      ann: Some({
        nodeKind: Term,
        sourceLocation,
      }),
    }
    switch t {
    | Exp(it) => {
        let it = printExp(it)
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: Group(list{it.ann.print})->annPrint,
          },
        }
      }
    | Def(it) => {
        let it = printDef(it)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: Group(list{it.ann.print})->annPrint,
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
      let annPrint = it => {
        it,
        ann: Some({
          nodeKind: Program,
          sourceLocation,
        }),
      }
      switch it {
      | PNil => {it: PNil, ann: {print: Group(list{})->annPrint, sourceLocation}}
      | PCons(t, p) => {
          let t = printTerm(t)
          switch p {
          | {it: PNil} => {
              it: PCons(
                t,
                {
                  it: PNil,
                  ann: {
                    print: Plain("")->annPrint,
                    sourceLocation: {
                      begin: sourceLocation.end,
                      end: sourceLocation.end,
                    },
                  },
                },
              ),
              ann: {
                print: Group(list{t.ann.print})->annPrint,
                sourceLocation,
              },
            }
          | _ => {
              let p = print(p)
              {
                it: PCons(t, p),
                ann: {
                  print: Print.concat2(t.ann.print, "\n", p.ann.print)->annPrint,
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

type exprContext = bool
type statContext =
  | Step
  | Return
type context =
  | Expr(exprContext)
  | Stat(statContext)

// module PYPrinter: Printer = {
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
    if es->List.some(containsNL) {
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
    Group(list{Print.string(op), x, Print.string(" = "), e})
  }

  let exprAppToString = (e, es) => {
    Group(list{e, Print.dummy(listToString(es))})
  }

  let consumeContext = (ctx, ann, e) => {
    let e = ann(e)
    switch ctx {
    | Expr(true) => e
    | Expr(false) => e
    | Stat(Step) => e
    | Stat(Return) => (Print.s`return ${e}`)->Print.dummy
    }
  }

  let paren = e => {
    Print.s`(${e->Print.dummy})`
  }

  let consumeContextWrapEvenReturn = (ctx, ann, e) => {
    switch ctx {
    | Expr(true) => paren(e)->ann
    | Expr(false) => e->ann
    | Stat(Step) => e->ann
    | Stat(Return) => (Print.s`return ${paren(e)->ann}`)->Print.dummy
    }
  }

  let consumeContextWrap = (ctx, ann, e) => {
    switch ctx {
    | Expr(true) => paren(e)->ann
    | Expr(false) => e->ann
    | Stat(Step) => e->ann
    | Stat(Return) => (Print.s`return ${e->ann}`)->Print.dummy
    }
  }

  let consumeContextVoid = (ctx, ann, e) => {
    let e = e->ann
    switch ctx {
    | Expr(true) => e
    | Expr(false) => e
    | Stat(Step) => e
    | Stat(Return) => (Print.s`${e}\nreturn`)->Print.dummy
    }
  }

  let consumeContextEscapeWrap = (ctx, ann, e) => {
    let e = e->ann
    switch ctx {
    | Expr(true) => e
    | Expr(false) => e
    | Stat(Step) => e
    | Stat(Return) => e
    }
  }

  let consumeContextStat = (ctx, ann, e) => {
    let e = e->ann
    switch ctx {
    | Expr(_) => raisePrintError(`${Print.toString(e)} can't be used as a expression in Python`)
    | Stat(Step) => e
    | Stat(Return) => (Print.s`${e}\nreturn`)->Print.dummy
    }
  }

  let stringOfArith = o => {
    switch o {
    | Add => "+"
    | Sub => "-"
    | Mul => "*"
    | Div => "/"
    }
  }

  let stringOfCmp = o => {
    switch o {
    | Lt => "<"
    | NumEq => "=="
    | Eq => "is"
    | Gt => ">"
    | Le => "<="
    | Ge => ">="
    | Ne => "!="
    | Equal => "=="
    }
  }

  let exprAppPrmToString = (ann, ctx, p: Primitive.t, es: list<bool => expression<printAnn>>) => {
    switch (p, es) {
    | (Arith(o), es) => {
        let es = es->List.map(e => e(true))
        {
          it: (Arith(o), es),
          ann: consumeContextWrap(
            ctx,
            ann,
            Print.concat(` ${stringOfArith(o)} `, es->List.map(e => e.ann.print)),
          ),
        }
      }
    | (Cmp(o), list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(true)
        {
          it: (Cmp(o), list{e1, e2}),
          ann: consumeContextWrap(
            ctx,
            ann,
            Print.s`${e1.ann.print} ${Print.string(stringOfCmp(o))} ${e2.ann.print}`,
          ),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairNew, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`[ ${e1.ann.print}, ${e2.ann.print} ]`),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          it: (PairRefLeft, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}[0]`),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          it: (PairRefRight, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}[1]`),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairSetLeft, list{e1, e2}),
          ann: consumeContextStat(ctx, ann, Print.s`${e1.ann.print}[0] = ${e2.ann.print}`),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairSetRight, list{e1, e2}),
          ann: consumeContextStat(ctx, ann, Print.s`${e1.ann.print}[1] = ${e2.ann.print}`),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          it: (VecNew, es),
          ann: consumeContext(
            ctx,
            ann,
            Print.s`[ ${Print.dummy(Print.concat(`, `, es->List.map(e => e.ann.print)))} ]`,
          ),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          it: (VecRef, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}[${e2.ann.print}]`),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          it: (VecSet, list{e1, e2, e3}),
          ann: consumeContextStat(
            ctx,
            ann,
            Print.s`${e1.ann.print}[${e2.ann.print}] = ${e3.ann.print}`,
          ),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          it: (VecLen, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`len(${e1.ann.print})`),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Err, list{e1}),
          ann: consumeContextEscapeWrap(ctx, ann, Print.s`raise ${e1.ann.print}`),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Not, list{e1}),
          ann: consumeContextWrap(ctx, ann, Print.s`not ${e1.ann.print}`),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          it: (Print, list{e1}),
          ann: consumeContextVoid(ctx, ann, Print.s`print(${e1.ann.print})`),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          it: (Next, list{e1}),
          ann: consumeContextVoid(ctx, ann, Print.s`next(${e1.ann.print})`),
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
  let exprGLamToString = exprLamToString
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

  let symbolToString = ({it, ann: sourceLocation}) => {
    {
      it,
      ann: {
        sourceLocation,
        print: {
          it: Plain(printName(it)),
          ann: Some({
            nodeKind: Name,
            sourceLocation,
          }),
        },
      },
    }
  }

  let rec printExp = ({it, ann: sourceLocation}, ctx, env): expression<printAnn> => {
    let ann = it => {
      it,
      ann: Some({
        nodeKind: Expression,
        sourceLocation,
      }),
    }
    let addSourceLocation = print => {
      sourceLocation,
      print,
    }
    switch it {
    | Con(c) => {
        it: Con(c),
        ann: consumeContext(ctx, ann, Plain(constantToString(c)))->addSourceLocation,
      }
    | Ref(x) => {
        it: Ref(x),
        ann: consumeContext(ctx, ann, Plain(x->printName))->addSourceLocation,
      }
    | Set(x, e) => {
        refMut(env, x.it)
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp(Expr(false), env)
        {
          it: Set(x, e),
          ann: consumeContextStat(
            ctx,
            ann,
            exprSetToString(x.ann.print, e.ann.print),
          )->addSourceLocation,
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printLamBody(xs, env)
        {
          it: Lam(xs, b),
          ann: consumeContextWrap(
            ctx,
            ann,
            exprLamToString(
              Print.concat(",", xs->List.map(x => x.ann.print))->Print.dummy,
              b.ann.print,
            ),
          )->addSourceLocation,
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printLamBody(xs, env)
        {
          it: GLam(xs, b),
          ann: consumeContextWrap(
            ctx,
            ann,
            exprGLamToString(
              Print.concat(",", xs->List.map(x => x.ann.print))->Print.dummy,
              b.ann.print,
            ),
          )->addSourceLocation,
        }
      }
    | Yield(e) => {
        let e = e->printExp(Expr(false), env)
        {
          it: Yield(e),
          ann: consumeContextWrapEvenReturn(
            ctx,
            ann,
            exprYieldToString(e.ann.print),
          )->addSourceLocation,
        }
      }
    | AppPrm(p, es) => {
        let es = es->List.map(e => b => e->printExp(Expr(b), env))
        let {it: (p, es), ann: print} = exprAppPrmToString(ann, ctx, p, es)
        {
          it: AppPrm(p, es),
          ann: print->addSourceLocation,
        }
      }
    | App(e, es) => {
        let e = e->printExp(Expr(true), env)
        let es = es->List.map(e => e->printExp(Expr(false), env))
        {
          it: App(e, es),
          ann: consumeContext(
            ctx,
            ann,
            exprAppToString(e.ann.print, es->List.map(e => e.ann.print)),
          )->addSourceLocation,
        }
      }
    | Let(_xes, _b) => raisePrintError("let-expressions are not supported by Python")
    | Letrec(_xes, _b) => raisePrintError("letrec-expressions are not supported by Python")
    | Cnd(ebs, ob) =>
      switch ctx {
      | Expr(_) =>
        raisePrintError(
          "Multi-armed conditionals in Python is not supported by the translator yet.",
        )
      | Stat(ctx) => {
          let ebs = ebs->List.map(eb => eb->ebToString(ctx, env))
          let ob = ob->obToString(ctx, env)
          {
            ann: exprCndToString(
              ebs->List.map(((e, b)) => (e.ann.print, b.ann.print)),
              ob->Option.map(b => b.ann.print),
            )
            ->ann
            ->addSourceLocation,
            it: Cnd(ebs, ob),
          }
        }
      }
    | If(e_cnd, e_thn, e_els) =>
      switch ctx {
      | Expr(ctx) => {
          let e_cnd = printExp(e_cnd, Expr(true), env)
          let e_thn = printExp(e_thn, Expr(true), env)
          let e_els = printExp(e_els, Expr(true), env)
          {
            ann: consumeContextWrap(
              Expr(ctx),
              ann,
              exprIfToString(e_cnd.ann.print, e_thn.ann.print, e_els.ann.print),
            )->addSourceLocation,
            it: If(e_cnd, e_thn, e_els),
          }
        }
      | Stat(ctx) => {
          let e_cnd = printExp(e_cnd, Expr(false), env)
          let e_thn = printExp(e_thn, Stat(ctx), env)
          let e_els = printExp(e_els, Stat(ctx), env)
          {
            it: If(e_cnd, e_thn, e_els),
            ann: ifStat(e_cnd.ann.print, e_thn.ann.print, Some(e_els.ann.print))
            ->ann
            ->addSourceLocation,
          }
        }
      }
    | Bgn(_es, _e) => raisePrintError("`begin` expressions are not supported by Python")
    }
  }
  and printDef = ({ann: sourceLocation, it: d}, env): definition<printAnn> => {
    let d = switch d {
    | Var(x, e) => {
        let x = x->symbolToString
        let e = e->printExp(Expr(false), env)
        {
          it: Var(x, e),
          ann: defvarToString(x.ann.print, e.ann.print),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printDefBody(xs, env)
        {
          ann: deffunToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printDefBody(xs, env)
        {
          ann: defgenToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: GFun(f, xs, b),
        }
      }
    }
    let {ann: print, it} = d
    {
      ann: {
        sourceLocation,
        print: {
          it: print,
          ann: Some({
            nodeKind: Definition,
            sourceLocation,
          }),
        },
      },
      it,
    }
  }
  and ebToString = (eb, ctx: statContext, env) => {
    let (e, b) = eb
    (e->printExp(Expr(false), env), b->printBlock(Stat(ctx), env))
  }
  and obToString = (ob, ctx: statContext, env) => {
    ob->Option.map(b => b->printBlock(Stat(ctx), env))
  }
  and printBlockHelper = ({ann: sourceLocation, it: b}, ctx, env) => {
    let annPrint = print => {
      it: print,
      ann: Some({
        nodeKind: Block,
        sourceLocation,
      }),
    }
    switch b {
    | BRet(e) => {
        let e = e->printExp(Stat(ctx), env)
        let print = annPrint(Group(list{e.ann.print}))
        {
          ann: {print, sourceLocation},
          it: BRet(e),
        }
      }
    | BCons(t, b) => {
        let t = printTerm(t, env, Step)
        let b = b->printBlockHelper(ctx, env)
        let print = Group(list{t.ann.print, Print.string("\n"), b.ann.print})->annPrint
        {
          ann: {print, sourceLocation},
          it: BCons(t, b),
        }
      }
    }
  }
  and printBlock = (b, ctx, env) => {
    let {it, ann: sourceLocation} = b
    let annOfPrint = print => {
      sourceLocation,
      print: {
        it: print,
        ann: Some({
          {
            nodeKind: Block,
            sourceLocation,
          }
        }),
      },
    }
    switch (it, ctx) {
    | (BRet(e), Expr(ctx)) => {
        let e = e->printExp(Expr(ctx), env)
        {
          it: BRet(e),
          ann: annOfPrint(Group(list{e.ann.print})),
        }
      }
    | (_, Expr(_)) => raisePrintError("Python blocks can't be used as expressions in general")
    | (it, Stat(ctx)) => {
        let xs = xsOfBlock(b)
        if xs != list{} {
          raisePrintError("Python blocks can't declare local variables")
        }
        printBlockHelper({it, ann: sourceLocation}, ctx, env)
      }
    }
  }
  and printLamBody = (b, args, env): block<printAnn> => {
    switch b.it {
    | BRet(e) => {
        let args: list<string> = args->List.map(x => x.it)
        let (_refs, env) = extend(args, env)
        // todo: there should be some check after we support set! expression (#30)
        let {it, ann} = e->printExp(Expr(false), env)
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
    let b = printBlockHelper(b, Return, env)
    // add declarations
    let print = Print.concat(
      "\n",
      list{
        ...refs
        ->HashMap.String.toArray
        ->Js.Array2.map(((x, r)) => Print.string(`${r->RefDecl.toString} ${x}`))
        ->List.fromArray,
        b.ann.print,
      },
    )->Print.dummy
    {
      ann: {print, sourceLocation: b.ann.sourceLocation},
      it: b.it,
    }
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>, env, ctx): term<printAnn> => {
    switch it {
    | Exp(it) => {
        let it = printExp(it, Stat(ctx), env)
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: group(list{it.ann.print}),
          },
        }
      }
    | Def(it) => {
        let it = printDef(it, env)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: group(list{it.ann.print}),
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
    let rec print = (~isFirst=false, {it, ann: sourceLocation}: program<sourceLocation>): program<
      printAnn,
    > => {
      let annPrint = print => {
        sourceLocation,
        print: {
          it: print,
          ann: Some({
            nodeKind: Program,
            sourceLocation,
          }),
        },
      }
      switch it {
      | PNil => {it: PNil, ann: Plain("")->annPrint}
      | PCons(t, p) => {
          let t = printTerm(t, env, Step)
          let p = print(p)
          {
            it: PCons(t, p),
            ann: annPrint(
              Group(list{
                Print.string(
                  if isFirst {
                    ""
                  } else {
                    "\n"
                  },
                ),
                t.ann.print,
                p.ann.print,
              }),
            ),
          }
        }
      }
    }
    print(~isFirst=true, p)
  }

  let printProgram = (insertPrintTopLevel, p) => {
    Print.toString(printProgramFull(insertPrintTopLevel, p).ann.print)
  }

  let printStandAloneTerm = ({it}: term<sourceLocation>): string => {
    let globalEnv = G(HashSet.String.fromArray([]))
    Print.toString(
      switch it {
      | Def(it) => {
          let it = printDef(it, globalEnv)
          it.ann.print
        }
      | Exp(it) => {
          let it = printExp(it, Stat(Step), globalEnv)
          it.ann.print
        }
      },
    )
  }
}

module JSPrinter = {
  open! Belt

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
    | Uni => "undefined"
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
    if es->List.some(containsNL) {
      Group(list{
        Print.string("("),
        indentBlock(Print.dummy(Print.concat(",\n", es)), 4),
        Print.string(")"),
      })
    } else {
      Group(list{Print.string("("), Print.dummy(Print.concat(", ", es)), Print.string(")")})
    }
  }

  let exprAppToString = (e, es) => {
    Group(list{e, Print.dummy(listToString(es))})
  }

  let consumeContext = (ctx, ann, e) => {
    let e = ann(e)
    switch ctx {
    | Expr(true) => e
    | Expr(false) => e
    | Stat(Step) => (Print.s`${e};`)->Print.dummy
    | Stat(Return) => (Print.s`return ${e};`)->Print.dummy
    }
  }

  let paren = e => {
    Print.s`(${e->Print.dummy})`
  }

  let consumeContextWrapEvenReturn = (ctx, ann, e) => {
    switch ctx {
    | Expr(true) => paren(e)->ann
    | Expr(false) => e->ann
    | Stat(Step) => (Print.s`${e->ann};`)->Print.dummy
    | Stat(Return) => (Print.s`return ${paren(e)->ann};`)->Print.dummy
    }
  }

  let consumeContextWrap = (ctx, ann, e) => {
    switch ctx {
    | Expr(true) => paren(e)->ann
    | Expr(false) => e->ann
    | Stat(Step) => (Print.s`${e->ann};`)->Print.dummy
    | Stat(Return) => (Print.s`return ${e->ann};`)->Print.dummy
    }
  }

  let consumeContextVoid = (ctx, ann, e) => {
    let e = e->ann
    switch ctx {
    | Expr(true) => e
    | Expr(false) => e
    | Stat(Step) => (Print.s`${e};`)->Print.dummy
    | Stat(Return) => (Print.s`${e};\nreturn;`)->Print.dummy
    }
  }

  let consumeContextEscapeWrap = (ctx, ann, e) => {
    let e = e->ann
    switch ctx {
    | Expr(true) => e
    | Expr(false) => e
    | Stat(_) => (Print.s`${e};`)->Print.dummy
    }
  }

  let consumeContextStat = (ctx, ann, e) => {
    let e = e->ann
    switch ctx {
    | Expr(_) => raisePrintError(`${Print.toString(e)} can't be used as a expression in JavaScript`)
    | Stat(Step) => (Print.s`${e};`)->Print.dummy
    | Stat(Return) => (Print.s`${e};\nreturn;`)->Print.dummy
    }
  }

  let stringOfArith = o => {
    switch o {
    | Add => "+"
    | Sub => "-"
    | Mul => "*"
    | Div => "/"
    }
  }

  let stringOfCmp = o => {
    switch o {
    | Lt => "<"
    | NumEq => "=="
    | Eq => "==="
    | Gt => ">"
    | Le => "<="
    | Ge => ">="
    | Ne => "!="
    | Equal => raisePrintError("JavaScript doesn't not have structural equality.")
    }
  }

  let exprAppPrmToString = (ann, ctx, p: Primitive.t, es: list<bool => expression<printAnn>>) => {
    switch (p, es) {
    | (Arith(o), es) => {
        let es = es->List.map(e => e(true))
        {
          it: (Arith(o), es),
          ann: consumeContextWrap(
            ctx,
            ann,
            Print.concat(` ${stringOfArith(o)} `, es->List.map(e => e.ann.print)),
          ),
        }
      }
    | (Cmp(o), list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(true)
        {
          it: (Cmp(o), list{e1, e2}),
          ann: consumeContextWrap(
            ctx,
            ann,
            Print.s`${e1.ann.print} ${Print.string(stringOfCmp(o))} ${e2.ann.print}`,
          ),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairNew, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`[ ${e1.ann.print}, ${e2.ann.print} ]`),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          it: (PairRefLeft, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}[0]`),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          it: (PairRefRight, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}[1]`),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairSetLeft, list{e1, e2}),
          ann: consumeContextStat(ctx, ann, Print.s`${e1.ann.print}[0] = ${e2.ann.print}`),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairSetRight, list{e1, e2}),
          ann: consumeContextStat(ctx, ann, Print.s`${e1.ann.print}[1] = ${e2.ann.print}`),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          it: (VecNew, es),
          ann: consumeContext(
            ctx,
            ann,
            Print.s`[ ${Print.dummy(Print.concat(`, `, es->List.map(e => e.ann.print)))} ]`,
          ),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          it: (VecRef, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}[${e2.ann.print}]`),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          it: (VecSet, list{e1, e2, e3}),
          ann: consumeContextStat(
            ctx,
            ann,
            Print.s`${e1.ann.print}[${e2.ann.print}] = ${e3.ann.print}`,
          ),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(true)
        {
          it: (VecLen, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}.length`),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Err, list{e1}),
          ann: consumeContextEscapeWrap(ctx, ann, Print.s`throw ${e1.ann.print}`),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Not, list{e1}),
          ann: consumeContextWrap(ctx, ann, Print.s`! ${e1.ann.print}`),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          it: (Print, list{e1}),
          ann: consumeContextVoid(ctx, ann, Print.s`console.log(${e1.ann.print})`),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Next, list{e1}),
          ann: consumeContextVoid(ctx, ann, Print.s`${e1.ann.print}.next()`),
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
    Print.s`let ${x} = ${e};`
  }

  let deffunToString = (f, xs, b) => {
    funLike("function", f, xs, b)
  }

  let defgenToString = (f, xs, b) => {
    funLike("function*", f, xs, b)
  }

  let exprSetToString = (x, e) => {
    Print.s`${x} = ${e}`
  }

  let exprLamToString = (xs, b) => {
    Print.s`(${xs}) => {${indentBlock(b, 2)}\n}`
  }
  let exprGLamToString = exprLamToString
  let exprYieldToString = e => Print.s`yield ${e}`

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

  let symbolToString = ({it, ann: sourceLocation}) => {
    {
      it,
      ann: {
        sourceLocation,
        print: {
          it: Plain(printName(it)),
          ann: Some({
            nodeKind: Name,
            sourceLocation,
          }),
        },
      },
    }
  }

  let rec printExp = ({it, ann: sourceLocation}, ctx): expression<printAnn> => {
    let ann = it => {
      it,
      ann: Some({
        nodeKind: Expression,
        sourceLocation,
      }),
    }
    let addSourceLocation = print => {
      sourceLocation,
      print,
    }
    switch it {
    | Con(c) => {
        it: Con(c),
        ann: consumeContext(ctx, ann, Plain(constantToString(c)))->addSourceLocation,
      }
    | Ref(x) => {
        it: Ref(x),
        ann: consumeContext(ctx, ann, Plain(x->printName))->addSourceLocation,
      }
    | Set(x, e) => {
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp(Expr(false))
        {
          it: Set(x, e),
          ann: consumeContextStat(
            ctx,
            ann,
            exprSetToString(x.ann.print, e.ann.print),
          )->addSourceLocation,
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Stat(Return))
        {
          it: Lam(xs, b),
          ann: consumeContextWrap(
            ctx,
            ann,
            exprLamToString(
              Print.concat(",", xs->List.map(x => x.ann.print))->Print.dummy,
              b.ann.print,
            ),
          )->addSourceLocation,
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Stat(Return))
        {
          it: GLam(xs, b),
          ann: consumeContextWrap(
            ctx,
            ann,
            exprGLamToString(
              Print.concat(",", xs->List.map(x => x.ann.print))->Print.dummy,
              b.ann.print,
            ),
          )->addSourceLocation,
        }
      }
    | Yield(e) => {
        let e = e->printExp(Expr(false))
        {
          it: Yield(e),
          ann: consumeContextWrapEvenReturn(
            ctx,
            ann,
            exprYieldToString(e.ann.print),
          )->addSourceLocation,
        }
      }
    | AppPrm(p, es) => {
        let es = es->List.map(e => b => e->printExp(Expr(b)))
        let {it: (p, es), ann: print} = exprAppPrmToString(ann, ctx, p, es)
        {
          it: AppPrm(p, es),
          ann: print->addSourceLocation,
        }
      }
    | App(e, es) => {
        let e = e->printExp(Expr(true))
        let es = es->List.map(e => e->printExp(Expr(false)))
        {
          it: App(e, es),
          ann: consumeContext(
            ctx,
            ann,
            exprAppToString(e.ann.print, es->List.map(e => e.ann.print)),
          )->addSourceLocation,
        }
      }
    | Let(_xes, _b) => raisePrintError("let-expressions are not supported by JavaScript")
    | Letrec(_xes, _b) => raisePrintError("letrec-expressions are not supported by JavaScript")
    | Cnd(ebs, ob) =>
      switch ctx {
      | Expr(_) =>
        raisePrintError(
          "Multi-armed conditionals in JavaScript is not supported by the translator yet.",
        )
      | Stat(ctx) => {
          let ebs = ebs->List.map(eb => eb->ebToString(ctx))
          let ob = ob->obToString(ctx)
          {
            ann: exprCndToString(
              ebs->List.map(((e, b)) => (e.ann.print, b.ann.print)),
              ob->Option.map(b => b.ann.print),
            )
            ->ann
            ->addSourceLocation,
            it: Cnd(ebs, ob),
          }
        }
      }
    | If(e_cnd, e_thn, e_els) =>
      switch ctx {
      | Expr(ctx) => {
          let e_cnd = printExp(e_cnd, Expr(false))
          let e_thn = printExp(e_thn, Expr(false))
          let e_els = printExp(e_els, Expr(false))
          {
            ann: consumeContextWrap(
              Expr(ctx),
              ann,
              exprIfToString(e_cnd.ann.print, e_thn.ann.print, e_els.ann.print),
            )->addSourceLocation,
            it: If(e_cnd, e_thn, e_els),
          }
        }
      | Stat(ctx) => {
          let e_cnd = printExp(e_cnd, Expr(false))
          let e_thn = printExp(e_thn, Stat(ctx))
          let e_els = printExp(e_els, Stat(ctx))
          {
            it: If(e_cnd, e_thn, e_els),
            ann: ifStat(e_cnd.ann.print, e_thn.ann.print, Some(e_els.ann.print))
            ->ann
            ->addSourceLocation,
          }
        }
      }
    | Bgn(_es, _e) => raisePrintError("`begin` expressions are not supported by JavaScript")
    }
  }
  and printDef = ({ann: sourceLocation, it: d}): definition<printAnn> => {
    let d = switch d {
    | Var(x, e) => {
        let x = x->symbolToString
        let e = e->printExp(Expr(false))
        {
          it: Var(x, e),
          ann: defvarToString(x.ann.print, e.ann.print),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Stat(Return))
        {
          ann: deffunToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Stat(Return))
        {
          ann: defgenToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: GFun(f, xs, b),
        }
      }
    }
    let {ann: print, it} = d
    {
      ann: {
        sourceLocation,
        print: {
          it: print,
          ann: Some({
            nodeKind: Definition,
            sourceLocation,
          }),
        },
      },
      it,
    }
  }
  and ebToString = (eb, ctx: statContext) => {
    let (e, b) = eb
    (e->printExp(Expr(false)), printBlock(b, Stat(ctx)))
  }
  and obToString = (ob, ctx: statContext) => {
    ob->Option.map(b => b->printBlock(Stat(ctx)))
  }
  and printBlockHelper = ({ann: sourceLocation, it: b}, ctx) => {
    let annPrint = print => {
      it: print,
      ann: Some({
        nodeKind: Block,
        sourceLocation,
      }),
    }
    switch b {
    | BRet(e) => {
        let e = e->printExp(Stat(ctx))
        let print = annPrint(Group(list{e.ann.print}))
        {
          ann: {print, sourceLocation},
          it: BRet(e),
        }
      }
    | BCons(t, b) => {
        let t = printTerm(t, Step)
        let b = b->printBlockHelper(ctx)
        let print = Group(list{t.ann.print, Print.string("\n"), b.ann.print})->annPrint
        {
          ann: {print, sourceLocation},
          it: BCons(t, b),
        }
      }
    }
  }
  and printBlock = (b, ctx) => {
    let {it, ann: sourceLocation} = b
    let annOfPrint = print => {
      sourceLocation,
      print: {
        it: print,
        ann: Some({
          {
            nodeKind: Block,
            sourceLocation,
          }
        }),
      },
    }
    switch (it, ctx) {
    | (BRet(e), Expr(ctx)) => {
        let e = e->printExp(Expr(ctx))
        {
          it: BRet(e),
          ann: annOfPrint(Group(list{e.ann.print})),
        }
      }
    | (_, Expr(_)) => raisePrintError("JavaScript blocks can't be used as expressions in general")
    | (it, Stat(ctx)) => printBlockHelper({it, ann: sourceLocation}, ctx)
    }
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>, ctx): term<printAnn> => {
    switch it {
    | Exp(it) => {
        let it = printExp(it, Stat(ctx))
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: group(list{it.ann.print}),
          },
        }
      }
    | Def(it) => {
        let it = printDef(it)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: group(list{it.ann.print}),
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
    let rec print = (~isFirst=false, {it, ann: sourceLocation}: program<sourceLocation>): program<
      printAnn,
    > => {
      let annPrint = print => {
        sourceLocation,
        print: {
          it: print,
          ann: Some({
            nodeKind: Program,
            sourceLocation,
          }),
        },
      }
      switch it {
      | PNil => {it: PNil, ann: Plain("")->annPrint}
      | PCons(t, p) => {
          let t = printTerm(t, Step)
          let p = print(p)
          {
            it: PCons(t, p),
            ann: annPrint(
              Group(list{
                Print.string(
                  if isFirst {
                    ""
                  } else {
                    "\n"
                  },
                ),
                t.ann.print,
                p.ann.print,
              }),
            ),
          }
        }
      }
    }
    print(~isFirst=true, p)
  }

  let printProgram = (insertPrintTopLevel, p) => {
    Print.toString(printProgramFull(insertPrintTopLevel, p).ann.print)
  }

  let printStandAloneTerm = ({it}: term<sourceLocation>): string => {
    Print.toString(
      switch it {
      | Def(it) => {
          let it = printDef(it)
          it.ann.print
        }
      | Exp(it) => {
          let it = printExp(it, Stat(Step))
          it.ann.print
        }
      },
    )
  }
}

module PCPrinter = {
  open! Belt

  let printName = x => x

  let constantToString = c => {
    switch c {
    | Uni => "Nothing"
    | Nil => "list[]"
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
    if es->List.some(containsNL) {
      Group(list{
        Print.string("("),
        indentBlock(Print.dummy(Print.concat(",\n", es)), 4),
        Print.string(")"),
      })
    } else {
      Group(list{Print.string("("), Print.dummy(Print.concat(", ", es)), Print.string(")")})
    }
  }

  let exprAppToString = (e, es) => {
    Group(list{e, Print.dummy(listToString(es))})
  }

  let consumeContext = (ctx, ann, e) => {
    let e = ann(e)
    switch ctx {
    | Expr(true) => e
    | Expr(false) => e
    | Stat(Step) => (Print.s`${e}`)->Print.dummy
    | Stat(Return) => (Print.s`return ${e}`)->Print.dummy
    }
  }

  let paren = e => {
    Print.s`(${e->Print.dummy})`
  }

  let consumeContextWrapEvenReturn = (ctx, ann, e) => {
    switch ctx {
    | Expr(true) => paren(e)->ann
    | Expr(false) => e->ann
    | Stat(Step) => (Print.s`${e->ann}`)->Print.dummy
    | Stat(Return) => (Print.s`return ${paren(e)->ann}`)->Print.dummy
    }
  }

  let consumeContextWrap = (ctx, ann, e) => {
    switch ctx {
    | Expr(true) => paren(e)->ann
    | Expr(false) => e->ann
    | Stat(Step) => (Print.s`${e->ann}`)->Print.dummy
    | Stat(Return) => (Print.s`return ${e->ann}`)->Print.dummy
    }
  }

  let consumeContextVoid = (ctx, ann, e) => {
    let e = e->ann
    switch ctx {
    | Expr(true) => e
    | Expr(false) => e
    | Stat(Step) => (Print.s`${e}`)->Print.dummy
    | Stat(Return) => (Print.s`${e}\nreturn`)->Print.dummy
    }
  }

  let consumeContextEscapeWrap = (ctx, ann, e) => {
    let e = e->ann
    switch ctx {
    | Expr(true) => e
    | Expr(false) => e
    | Stat(_) => (Print.s`${e}`)->Print.dummy
    }
  }

  let consumeContextStat = (ctx, ann, e) => {
    let e = e->ann
    switch ctx {
    | Expr(_) => raisePrintError(`${Print.toString(e)} can't be used as a expression in Pseudocode`)
    | Stat(Step) => (Print.s`${e}`)->Print.dummy
    | Stat(Return) => (Print.s`${e}\nreturn`)->Print.dummy
    }
  }

  let stringOfArith = o => {
    switch o {
    | Add => "+"
    | Sub => "-"
    | Mul => "*"
    | Div => "/"
    }
  }

  let stringOfCmp = o => {
    switch o {
    | Lt => "<"
    | NumEq => "=="
    | Eq => "==="
    | Gt => ">"
    | Le => "<="
    | Ge => ">="
    | Ne => "!="
    | Equal => raisePrintError("Pseudocode doesn't not have structural equality.")
    }
  }

  let exprAppPrmToString = (ann, ctx, p: Primitive.t, es: list<bool => expression<printAnn>>) => {
    switch (p, es) {
    | (Arith(o), es) => {
        let es = es->List.map(e => e(true))
        {
          it: (Arith(o), es),
          ann: consumeContextWrap(
            ctx,
            ann,
            Print.concat(` ${stringOfArith(o)} `, es->List.map(e => e.ann.print)),
          ),
        }
      }
    | (Cmp(o), list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(true)
        {
          it: (Cmp(o), list{e1, e2}),
          ann: consumeContextWrap(
            ctx,
            ann,
            Print.s`${e1.ann.print} ${Print.string(stringOfCmp(o))} ${e2.ann.print}`,
          ),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairNew, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`vec[ ${e1.ann.print}, ${e2.ann.print} ]`),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          it: (PairRefLeft, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}[0]`),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          it: (PairRefRight, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}[1]`),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairSetLeft, list{e1, e2}),
          ann: consumeContextStat(ctx, ann, Print.s`${e1.ann.print}[0] = ${e2.ann.print}`),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairSetRight, list{e1, e2}),
          ann: consumeContextStat(ctx, ann, Print.s`${e1.ann.print}[1] = ${e2.ann.print}`),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          it: (VecNew, es),
          ann: consumeContext(
            ctx,
            ann,
            Print.s`vec[ ${Print.dummy(Print.concat(`, `, es->List.map(e => e.ann.print)))} ]`,
          ),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          it: (VecRef, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}[${e2.ann.print}]`),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          it: (VecSet, list{e1, e2, e3}),
          ann: consumeContextStat(
            ctx,
            ann,
            Print.s`${e1.ann.print}[${e2.ann.print}] = ${e3.ann.print}`,
          ),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(false)
        {
          it: (VecLen, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`length(${e1.ann.print})`),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Err, list{e1}),
          ann: consumeContextEscapeWrap(ctx, ann, Print.s`raise ${e1.ann.print}`),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Not, list{e1}),
          ann: consumeContextWrap(ctx, ann, Print.s`! ${e1.ann.print}`),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          it: (Print, list{e1}),
          ann: consumeContextVoid(ctx, ann, Print.s`print(${e1.ann.print})`),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(false)
        {
          it: (Next, list{e1}),
          ann: consumeContextVoid(ctx, ann, Print.s`next(${e1.ann.print})`),
        }
      }
    | (Cons, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (Cons, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`list[${e1.ann.print}, ...${e2.ann.print}]`),
        }
      }
    | _ =>
      raisePrintError(
        `Pseudocode doesn't let you use ${Primitive.toString(p)} on ${Int.toString(
            List.length(es),
          )} parameter(s).`,
      )
    }
  }

  let funLike = (op, x, xs, e) => {
    Print.s`${Print.string(op)} ${Print.dummy(exprAppToString(x, xs))}:${indentBlock(e, 2)}\nend`
  }

  let defvarToString = (x, e) => {
    Print.s`let ${x} = ${e}`
  }

  let deffunToString = (f, xs, b) => {
    funLike("fun", f, xs, b)
  }

  let defgenToString = (f, xs, b) => {
    funLike("gen fun", f, xs, b)
  }

  let exprSetToString = (x, e) => {
    Print.s`${x} = ${e}`
  }

  let exprLamToString = (xs, b) => {
    Print.s`lam (${xs}):${indentBlock(b, 2)}\nend`
  }
  let exprGLamToString = exprLamToString
  let exprYieldToString = e => Print.s`yield ${e}`

  let ifStat = (cnd, thn, els) => {
    Print.s`if ${cnd}:${indentBlock(thn, 2)}${switch els {
    | None => Print.s``
    | Some(els) => Print.s`\nelse:${indentBlock(els, 2)}`
    }->Print.dummy}\nend`
  }

  let exprCndToString = (ebs: list<(_, _)>, ob) => {
    if ebs == list{} {
      raisePrintError("`else`-only conditional is not supported by Pseudo.")
    }
    let ebs = ebs->List.map(((e, b)) => (Print.s`if ${e}:${indentBlock(b, 2)}`)->Print.dummy)
    let ebs = switch ob {
    | None => ebs
    | Some(b) => list{...ebs, (Print.s`e:${indentBlock(b, 2)}`)->Print.dummy}
    }
    Print.s`${Print.concat("\nels", ebs)->Print.dummy}\nend`
  }

  let exprIfToString = (e_cnd, e_thn, e_els) => {
    Print.s`${e_thn} if ${e_cnd} else ${e_els}`
  }

  let symbolToString = ({it, ann: sourceLocation}) => {
    {
      it,
      ann: {
        sourceLocation,
        print: {
          it: Plain(printName(it)),
          ann: Some({
            nodeKind: Name,
            sourceLocation,
          }),
        },
      },
    }
  }

  let rec printExp = ({it, ann: sourceLocation}, ctx): expression<printAnn> => {
    let ann = it => {
      it,
      ann: Some({
        nodeKind: Expression,
        sourceLocation,
      }),
    }
    let addSourceLocation = print => {
      sourceLocation,
      print,
    }
    switch it {
    | Con(c) => {
        it: Con(c),
        ann: consumeContext(ctx, ann, Plain(constantToString(c)))->addSourceLocation,
      }
    | Ref(x) => {
        it: Ref(x),
        ann: consumeContext(ctx, ann, Plain(x->printName))->addSourceLocation,
      }
    | Set(x, e) => {
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp(Expr(false))
        {
          it: Set(x, e),
          ann: consumeContextStat(
            ctx,
            ann,
            exprSetToString(x.ann.print, e.ann.print),
          )->addSourceLocation,
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Stat(Return))
        {
          it: Lam(xs, b),
          ann: consumeContextWrap(
            ctx,
            ann,
            exprLamToString(
              Print.concat(",", xs->List.map(x => x.ann.print))->Print.dummy,
              b.ann.print,
            ),
          )->addSourceLocation,
        }
      }
    | GLam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Stat(Return))
        {
          it: GLam(xs, b),
          ann: consumeContextWrap(
            ctx,
            ann,
            exprGLamToString(
              Print.concat(",", xs->List.map(x => x.ann.print))->Print.dummy,
              b.ann.print,
            ),
          )->addSourceLocation,
        }
      }
    | Yield(e) => {
        let e = e->printExp(Expr(false))
        {
          it: Yield(e),
          ann: consumeContextWrapEvenReturn(
            ctx,
            ann,
            exprYieldToString(e.ann.print),
          )->addSourceLocation,
        }
      }
    | AppPrm(p, es) => {
        let es = es->List.map(e => b => e->printExp(Expr(b)))
        let {it: (p, es), ann: print} = exprAppPrmToString(ann, ctx, p, es)
        {
          it: AppPrm(p, es),
          ann: print->addSourceLocation,
        }
      }
    | App(e, es) => {
        let e = e->printExp(Expr(true))
        let es = es->List.map(e => e->printExp(Expr(false)))
        {
          it: App(e, es),
          ann: consumeContext(
            ctx,
            ann,
            exprAppToString(e.ann.print, es->List.map(e => e.ann.print)),
          )->addSourceLocation,
        }
      }
    | Let(_xes, _b) => raisePrintError("let-expressions are not supported by Pseudocode")
    | Letrec(_xes, _b) => raisePrintError("letrec-expressions are not supported by Pseudocode")
    | Cnd(ebs, ob) =>
      switch ctx {
      | Expr(_) =>
        raisePrintError(
          "Multi-armed conditionals in Pseudocode is not supported by the translator yet.",
        )
      | Stat(ctx) => {
          let ebs = ebs->List.map(eb => eb->ebToString(ctx))
          let ob = ob->obToString(ctx)
          {
            ann: exprCndToString(
              ebs->List.map(((e, b)) => (e.ann.print, b.ann.print)),
              ob->Option.map(b => b.ann.print),
            )
            ->ann
            ->addSourceLocation,
            it: Cnd(ebs, ob),
          }
        }
      }
    | If(e_cnd, e_thn, e_els) =>
      switch ctx {
      | Expr(ctx) => {
          let e_cnd = printExp(e_cnd, Expr(false))
          let e_thn = printExp(e_thn, Expr(false))
          let e_els = printExp(e_els, Expr(false))
          {
            ann: consumeContextWrap(
              Expr(ctx),
              ann,
              exprIfToString(e_cnd.ann.print, e_thn.ann.print, e_els.ann.print),
            )->addSourceLocation,
            it: If(e_cnd, e_thn, e_els),
          }
        }
      | Stat(ctx) => {
          let e_cnd = printExp(e_cnd, Expr(false))
          let e_thn = printExp(e_thn, Stat(ctx))
          let e_els = printExp(e_els, Stat(ctx))
          {
            it: If(e_cnd, e_thn, e_els),
            ann: ifStat(e_cnd.ann.print, e_thn.ann.print, Some(e_els.ann.print))
            ->ann
            ->addSourceLocation,
          }
        }
      }
    | Bgn(_es, _e) => raisePrintError("`begin` expressions are not supported by Pseudocode")
    }
  }
  and printDef = ({ann: sourceLocation, it: d}): definition<printAnn> => {
    let d = switch d {
    | Var(x, e) => {
        let x = x->symbolToString
        let e = e->printExp(Expr(false))
        {
          it: Var(x, e),
          ann: defvarToString(x.ann.print, e.ann.print),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Stat(Return))
        {
          ann: deffunToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: Fun(f, xs, b),
        }
      }
    | GFun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(Stat(Return))
        {
          ann: defgenToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: GFun(f, xs, b),
        }
      }
    }
    let {ann: print, it} = d
    {
      ann: {
        sourceLocation,
        print: {
          it: print,
          ann: Some({
            nodeKind: Definition,
            sourceLocation,
          }),
        },
      },
      it,
    }
  }
  and ebToString = (eb, ctx: statContext) => {
    let (e, b) = eb
    (e->printExp(Expr(false)), printBlock(b, Stat(ctx)))
  }
  and obToString = (ob, ctx: statContext) => {
    ob->Option.map(b => b->printBlock(Stat(ctx)))
  }
  and printBlockHelper = ({ann: sourceLocation, it: b}, ctx) => {
    let annPrint = print => {
      it: print,
      ann: Some({
        nodeKind: Block,
        sourceLocation,
      }),
    }
    switch b {
    | BRet(e) => {
        let e = e->printExp(Stat(ctx))
        let print = annPrint(Group(list{e.ann.print}))
        {
          ann: {print, sourceLocation},
          it: BRet(e),
        }
      }
    | BCons(t, b) => {
        let t = printTerm(t, Step)
        let b = b->printBlockHelper(ctx)
        let print = Group(list{t.ann.print, Print.string("\n"), b.ann.print})->annPrint
        {
          ann: {print, sourceLocation},
          it: BCons(t, b),
        }
      }
    }
  }
  and printBlock = (b, ctx) => {
    let {it, ann: sourceLocation} = b
    let annOfPrint = print => {
      sourceLocation,
      print: {
        it: print,
        ann: Some({
          {
            nodeKind: Block,
            sourceLocation,
          }
        }),
      },
    }
    switch (it, ctx) {
    | (BRet(e), Expr(ctx)) => {
        let e = e->printExp(Expr(ctx))
        {
          it: BRet(e),
          ann: annOfPrint(Group(list{e.ann.print})),
        }
      }
    | (_, Expr(_)) => raisePrintError("Pseudocode blocks can't be used as expressions in general")
    | (it, Stat(ctx)) => printBlockHelper({it, ann: sourceLocation}, ctx)
    }
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>, ctx): term<printAnn> => {
    switch it {
    | Exp(it) => {
        let it = printExp(it, Stat(ctx))
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: group(list{it.ann.print}),
          },
        }
      }
    | Def(it) => {
        let it = printDef(it)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: group(list{it.ann.print}),
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
          | Lst(_) => raisePrintError("Lists are not supported in Pseudocode.")
          | Vec(es) => `vec[ ${concat(", ", es->List.map(p)->List.toArray)} ]`
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
    let rec print = (~isFirst=false, {it, ann: sourceLocation}: program<sourceLocation>): program<
      printAnn,
    > => {
      let annPrint = print => {
        sourceLocation,
        print: {
          it: print,
          ann: Some({
            nodeKind: Program,
            sourceLocation,
          }),
        },
      }
      switch it {
      | PNil => {it: PNil, ann: Plain("")->annPrint}
      | PCons(t, p) => {
          let t = printTerm(t, Step)
          let p = print(p)
          {
            it: PCons(t, p),
            ann: annPrint(
              Group(list{
                Print.string(
                  if isFirst {
                    ""
                  } else {
                    "\n"
                  },
                ),
                t.ann.print,
                p.ann.print,
              }),
            ),
          }
        }
      }
    }
    print(~isFirst=true, p)
  }

  let printProgram = (insertPrintTopLevel, p) => {
    Print.toString(printProgramFull(insertPrintTopLevel, p).ann.print)
  }

  let printStandAloneTerm = ({it}: term<sourceLocation>): string => {
    Print.toString(
      switch it {
      | Def(it) => {
          let it = printDef(it)
          it.ann.print
        }
      | Exp(it) => {
          let it = printExp(it, Stat(Step))
          it.ann.print
        }
      },
    )
  }
}

module SCPrinter = {
  open! Belt

  let useVarRatherThanVal = ref(false)
  let useBufferRatherThanTuple = ref(false)

  let makeVec = es => {
    if useBufferRatherThanTuple.contents {
      Print.s`Buffer(${Print.concat(", ", es)->Print.dummy})`
    } else {
      Print.s`(${Print.concat(", ", es)->Print.dummy})`
    }
  }

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
    | Uni => "undefined"
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
    if es == list{} {
      Plain("")
    } else if es->List.some(containsNL) {
      Group(list{
        Print.string("("),
        indentBlock(Print.dummy(Print.concat(",\n", es)), 4),
        Print.string(")"),
      })
    } else {
      Group(list{Print.string("("), Print.dummy(Print.concat(", ", es)), Print.string(")")})
    }
  }

  let exprAppToString = (e, es) => {
    Group(list{e, Print.dummy(listToString(es))})
  }

  let consumeContext = (_ctx, ann, e) => {
    let e = ann(e)
    e
  }

  let paren = e => {
    Print.s`(${e->Print.dummy})`
  }

  let consumeContextWrap = (ctx, ann, e) => {
    switch ctx {
    | true => paren(e)->ann
    | false => e->ann
    }
  }

  let stringOfArith = o => {
    switch o {
    | Add => "+"
    | Sub => "-"
    | Mul => "*"
    | Div => "/"
    }
  }

  let stringOfCmp = o => {
    switch o {
    | Lt => "<"
    | NumEq => "=="
    | Eq => "==="
    | Gt => ">"
    | Le => "<="
    | Ge => ">="
    | Ne => "!="
    | Equal => raisePrintError("JavaScript doesn't not have structural equality.")
    }
  }

  let exprAppPrmToString = (ann, ctx, p: Primitive.t, es: list<bool => expression<printAnn>>) => {
    switch (p, es) {
    | (Arith(o), es) => {
        let es = es->List.map(e => e(true))
        {
          it: (Arith(o), es),
          ann: consumeContextWrap(
            ctx,
            ann,
            Print.concat(` ${stringOfArith(o)} `, es->List.map(e => e.ann.print)),
          ),
        }
      }
    | (Cmp(o), list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(true)
        {
          it: (Cmp(o), list{e1, e2}),
          ann: consumeContextWrap(
            ctx,
            ann,
            Print.s`${e1.ann.print} ${Print.string(stringOfCmp(o))} ${e2.ann.print}`,
          ),
        }
      }
    | (PairNew, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairNew, list{e1, e2}),
          ann: consumeContext(ctx, ann, makeVec(list{e1.ann.print, e2.ann.print})),
        }
      }
    | (PairRefLeft, list{e1}) => {
        let e1 = e1(true)
        {
          it: (PairRefLeft, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}(0)`),
        }
      }
    | (PairRefRight, list{e1}) => {
        let e1 = e1(true)
        {
          it: (PairRefRight, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}(1)`),
        }
      }
    | (PairSetLeft, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairSetLeft, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}(0) = ${e2.ann.print}`),
        }
      }
    | (PairSetRight, list{e1, e2}) => {
        let e1 = e1(false)
        let e2 = e2(false)
        {
          it: (PairSetRight, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}(1) = ${e2.ann.print}`),
        }
      }
    | (VecNew, es) => {
        let es = es->List.map(e => e(false))
        {
          it: (VecNew, es),
          ann: consumeContext(
            ctx,
            ann,
            makeVec(es->List.map(e => e.ann.print)),
          ),
        }
      }
    | (VecRef, list{e1, e2}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        {
          it: (VecRef, list{e1, e2}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}(${e2.ann.print})`),
        }
      }
    | (VecSet, list{e1, e2, e3}) => {
        let e1 = e1(true)
        let e2 = e2(false)
        let e3 = e3(false)
        {
          it: (VecSet, list{e1, e2, e3}),
          ann: consumeContext(
            ctx,
            ann,
            Print.s`${e1.ann.print}(${e2.ann.print}) = ${e3.ann.print}`,
          ),
        }
      }
    | (VecLen, list{e1}) => {
        let e1 = e1(true)
        {
          it: (VecLen, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}.length`),
        }
      }
    | (Err, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Err, list{e1}),
          ann: consumeContextWrap(ctx, ann, Print.s`throw ${e1.ann.print}`),
        }
      }
    | (Not, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Not, list{e1}),
          ann: consumeContextWrap(ctx, ann, Print.s`! ${e1.ann.print}`),
        }
      }
    | (Print, list{e1}) => {
        let e1 = e1(false)
        {
          it: (Print, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`println(${e1.ann.print})`),
        }
      }
    | (Next, list{e1}) => {
        let e1 = e1(true)
        {
          it: (Next, list{e1}),
          ann: consumeContext(ctx, ann, Print.s`${e1.ann.print}.next()`),
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
    Print.s`${Print.string(op)} ${Print.dummy(
      exprAppToString(x, xs->List.map(x => Print.dummy(Print.s`${x} : Int`))),
    )} =${indentBlock(e, 2)}`
  }

  let defvarToString = (x, e) => {
    if useVarRatherThanVal.contents {
      Print.s`var ${x} = ${e}`
    } else {
      Print.s`val ${x} = ${e}`
    }
  }

  let deffunToString = (f, xs, b) => {
    funLike("def", f, xs, b)
  }
  let exprSetToString = (x, e) => {
    Print.s`${x} = ${e}`
  }

  let exprLamToString = (xs, b) => {
    Print.s`(${xs}) =>${indentBlock(b, 2)}`
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
    Print.s`if (${e_cnd}) {${indentBlock(e_thn, 2)}\n} else {${indentBlock(e_els, 2)}\n}`
  }

  let symbolToString = ({it, ann: sourceLocation}) => {
    {
      it,
      ann: {
        sourceLocation,
        print: {
          it: Plain(printName(it)),
          ann: Some({
            nodeKind: Name,
            sourceLocation,
          }),
        },
      },
    }
  }

  let rec printExp = ({it, ann: sourceLocation}, ctx: bool): expression<printAnn> => {
    let ann = it => {
      it,
      ann: Some({
        nodeKind: Expression,
        sourceLocation,
      }),
    }
    let addSourceLocation = print => {
      sourceLocation,
      print,
    }
    switch it {
    | Con(c) => {
        it: Con(c),
        ann: consumeContext(ctx, ann, Plain(constantToString(c)))->addSourceLocation,
      }
    | Ref(x) => {
        it: Ref(x),
        ann: consumeContext(ctx, ann, Plain(x->printName))->addSourceLocation,
      }
    | Set(x, e) => {
        let x = symbolToString(x)
        let e: expression<printAnn> = e->printExp(false)
        {
          it: Set(x, e),
          ann: consumeContext(
            ctx,
            ann,
            exprSetToString(x.ann.print, e.ann.print),
          )->addSourceLocation,
        }
      }
    | Lam(xs, b) => {
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(false)
        {
          it: Lam(xs, b),
          ann: consumeContextWrap(
            ctx,
            ann,
            exprLamToString(
              Print.concat(
                ", ",
                xs->List.map(x => Print.dummy(Print.s`${x.ann.print} : Int`)),
              )->Print.dummy,
              b.ann.print,
            ),
          )->addSourceLocation,
        }
      }
    | GLam(_xs, _b) => raisePrintError("Generators are not supported by Scala.")
    | Yield(_e) => raisePrintError("Generators are not supported by Scala.")
    | AppPrm(p, es) => {
        let es = es->List.map(e => b => e->printExp(b))
        let {it: (p, es), ann: print} = exprAppPrmToString(ann, ctx, p, es)
        {
          it: AppPrm(p, es),
          ann: print->addSourceLocation,
        }
      }
    | App(e, es) => {
        let e = e->printExp(true)
        let es = es->List.map(e => e->printExp(false))
        {
          it: App(e, es),
          ann: consumeContext(
            ctx,
            ann,
            exprAppToString(e.ann.print, es->List.map(e => e.ann.print)),
          )->addSourceLocation,
        }
      }
    | Let(_xes, _b) => raisePrintError("let-expressions are not supported by JavaScript")
    | Letrec(_xes, _b) => raisePrintError("letrec-expressions are not supported by JavaScript")
    | Cnd(ebs, ob) => {
        let ebs = ebs->List.map(eb => eb->ebToString)
        let ob = ob->obToString
        {
          ann: exprCndToString(
            ebs->List.map(((e, b)) => (e.ann.print, b.ann.print)),
            ob->Option.map(b => b.ann.print),
          )
          ->ann
          ->addSourceLocation,
          it: Cnd(ebs, ob),
        }
      }
    | If(e_cnd, e_thn, e_els) => {
        let e_cnd = printExp(e_cnd, false)
        let e_thn = printExp(e_thn, false)
        let e_els = printExp(e_els, false)
        {
          ann: consumeContextWrap(
            ctx,
            ann,
            exprIfToString(e_cnd.ann.print, e_thn.ann.print, e_els.ann.print),
          )->addSourceLocation,
          it: If(e_cnd, e_thn, e_els),
        }
      }
    | Bgn(_es, _e) => raisePrintError("`begin` expressions are not supported by JavaScript")
    }
  }
  and printDef = ({ann: sourceLocation, it: d}): definition<printAnn> => {
    let d = switch d {
    | Var(x, e) => {
        let x = x->symbolToString
        let e = e->printExp(false)
        {
          it: Var(x, e),
          ann: defvarToString(x.ann.print, e.ann.print),
        }
      }
    | Fun(f, xs, b) => {
        let f = f->symbolToString
        let xs = xs->List.map(symbolToString)
        let b = b->printBlock(false)
        {
          ann: deffunToString(f.ann.print, xs->List.map(x => x.ann.print), b.ann.print),
          it: Fun(f, xs, b),
        }
      }
    | GFun(_f, _xs, _b) => raisePrintError("Generators are not supported by Scala.")
    }
    let {ann: print, it} = d
    {
      ann: {
        sourceLocation,
        print: {
          it: print,
          ann: Some({
            nodeKind: Definition,
            sourceLocation,
          }),
        },
      },
      it,
    }
  }
  and ebToString = eb => {
    let (e, b) = eb
    (e->printExp(false), printBlock(b, false))
  }
  and obToString = ob => {
    ob->Option.map(b => b->printBlock(false))
  }
  and printBlock = (b, _ctx) => {
    let rec p = ({it, ann: sourceLocation}) => {
      let ann = ({it, ann}) => {
        it,
        ann: {
          sourceLocation,
          print: {
            it: ann,
            ann: Some({
              nodeKind: Block,
              sourceLocation,
            }),
          },
        },
      }
      switch it {
      | BRet(e) => {
          let e = printExp(e, false)
          {
            it: BRet(e),
            ann: Group(list{e.ann.print}),
          }
        }
      | BCons(t, b) => {
          let t = printTerm(t)
          let b = p(b)
          {
            it: BCons(t, b),
            ann: Print.s`${t.ann.print}\n${b.ann.print}`,
          }
        }
      }->ann
    }
    p(b)
  }
  and printTerm = ({ann: sourceLocation, it}: term<sourceLocation>): term<printAnn> => {
    switch it {
    | Exp(it) => {
        let it = printExp(it, false)
        {
          it: Exp(it),
          ann: {
            sourceLocation,
            print: group(list{it.ann.print}),
          },
        }
      }
    | Def(it) => {
        let it = printDef(it)
        {
          it: Def(it),
          ann: {
            sourceLocation,
            print: group(list{it.ann.print}),
          },
        }
      }
    }
  }

  let printOutputlet = o => {
    let rec p = (v: val): string => {
      switch v {
      | Ref(_) => `(...)`
      | Con(c) => constantToString(c)
      | Struct(i, content) => {
          let i = switch i {
          | None => ""
          | Some(_) => ""
          }
          let content = switch content {
          | Lst(_) => raisePrintError("Lists are not supported in JavaScript.")
          | Vec(es) => makeVec(es->List.map(p)->List.map(Print.string))->Print.dummy->Print.toString
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
    useBufferRatherThanTuple := true
    concat(sep, os->List.map(printOutputlet)->List.toArray)
  }

  let printProgramFull = (insertPrintTopLevel, p) => {
    let p = if insertPrintTopLevel {
      insertTopLevelPrint(p)
    } else {
      p
    }
    let s = SMoLPrinter.printProgram(insertPrintTopLevel, p)
    let mutVar = Js.String.includes("(set! ", s)
    let mutVec =
      Js.String.includes("(vec-set! ", s) ||
      Js.String.includes("(set-left! ", s) ||
      Js.String.includes("(set-right! ", s)
    useVarRatherThanVal := mutVar
    useBufferRatherThanTuple := mutVar || mutVec

    let rec print = (~isFirst=false, {it, ann: sourceLocation}: program<sourceLocation>): program<
      printAnn,
    > => {
      let annPrint = print => {
        sourceLocation,
        print: {
          it: print,
          ann: Some({
            nodeKind: Program,
            sourceLocation,
          }),
        },
      }
      switch it {
      | PNil => {it: PNil, ann: Plain("")->annPrint}
      | PCons(t, p) => {
          let t = printTerm(t)
          let p = print(p)
          {
            it: PCons(t, p),
            ann: annPrint(
              Group(list{
                Print.string(
                  if isFirst {
                    ""
                  } else {
                    "\n"
                  },
                ),
                t.ann.print,
                p.ann.print,
              }),
            ),
          }
        }
      }
    }
    print(~isFirst=true, p)
  }

  let printProgram = (insertPrintTopLevel, p) => {
    Print.toString(printProgramFull(insertPrintTopLevel, p).ann.print)
  }

  let printStandAloneTerm = ({it}: term<sourceLocation>): string => {
    Print.toString(
      switch it {
      | Def(it) => {
          let it = printDef(it)
          it.ann.print
        }
      | Exp(it) => {
          let it = printExp(it, false)
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
module PYTranslator = MakeTranslator(PYPrinter)
module JSTranslator = MakeTranslator(JSPrinter)
module PCTranslator = MakeTranslator(PCPrinter)
module SCTranslator = MakeTranslator(SCPrinter)
