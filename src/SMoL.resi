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
type constant = Uni | Num(float) | Lgc(bool) | Str(string)
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
  | If(
      annotated<expression>,
      annotated<expression>,
      annotated<expression>,
    )
  | Cnd(list<(annotated<expression>, block)>, option<block>)
and block = (list<term>, annotated<expression>)
and definition =
  | Var(annotated<symbol>, annotated<expression>)
  | Fun(annotated<symbol>, list<annotated<symbol>>, block)
and term = Def(annotated<definition>) | Exp(annotated<expression>)
and program = list<term>

type exn += PrintError(string)
module type Printer = {
  let printProgram: program => string
  let printBlock: block => string
  let printTerm: term => string
}
module SMoLPrinter: Printer
module JSPrinter: Printer
module PYPrinter: Printer

module SExprKind: {
  type t = Atom | List
  let toString: t => string
}
module Arity: {
  type t =
  | ExactlyOne
  | ExactlyTwo
  | ExactlyThree
  | OneThenMany
  | ManyThenOne
  | OneThenManyThenOne
  let toString: t => string
}
module TermKind: {
  type t = Definition | Expression
  let toString: t => string
}
module ParseError: {
  type t =
    | SExprParseError(string)
    | SExprKindError(SExprKind.t, string, annotated<SExpression.t>)
    | SExprArityError(Arity.t, string, list<annotated<SExpression.t>>)
    | LiteralSymbolError(string)
    | LiteralListError(annotated<SExpression.t>)
    | TermKindError(TermKind.t, string, term)
  let toString : t => string
}
type exn += SMoLParseError(ParseError.t)
module Parser: {
  let parseTerms: string => list<term>
  let parseProgram: string => program
}
