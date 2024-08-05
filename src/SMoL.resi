open SExpression

module Primitive: {
  type t =
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
    | Err
    | Not
    | Print
    | Next
  let toString: t => string
}
type constant = Uni | Num(float) | Lgc(bool) | Str(string)
type symbol = string
type rec expression =
  | Con(constant)
  | Ref(annotated<symbol>)
  | Set(annotated<symbol>, annotated<expression>)
  | Lam(list<annotated<symbol>>, block)
  | Let(list<(annotated<symbol>, annotated<expression>)>, block)
  | Letrec(list<(annotated<symbol>, annotated<expression>)>, block)
  | AppPrm(Primitive.t, list<annotated<expression>>)
  | App(annotated<expression>, list<annotated<expression>>)
  | Bgn(list<annotated<expression>>, annotated<expression>)
  | If(annotated<expression>, annotated<expression>, annotated<expression>)
  | Cnd(list<(annotated<expression>, block)>, option<block>)
  | GLam(list<annotated<symbol>>, block)
  | Yield(annotated<expression>)
and block = (list<term>, annotated<expression>)
and definition =
  | Var(annotated<symbol>, annotated<expression>)
  | Fun(annotated<symbol>, list<annotated<symbol>>, block)
  | GFun(annotated<symbol>, list<annotated<symbol>>, block)
and term = Def(annotated<definition>) | Exp(annotated<expression>)
and program = list<term>

type exn += SMoLPrintError(string)
module type Printer = {
  let printProgram: (bool, program) => string
  let printBlock: block => string
  let printTerm: term => string
}
module SMoLPrinter: Printer
module JSPrinter: Printer
module PYPrinter: Printer
module ScalaPrinter: Printer
module CommonPrinter: Printer

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
  let toString: t => string
}
type exn += SMoLParseError(ParseError.t)
module Parser: {
  let parseTerms: string => list<term>
  let parseProgram: string => program
}

module TranslateError: {
  type t =
    | ParseError(ParseError.t)
    | PrintError(string)
  let toString: t => string
}
type exn += SMoLTranslateError(TranslateError.t)
module type Translator = {
  let translateTerms: string => string
  let translateProgram: (bool, string) => string
}
module PYTranslator: Translator
module JSTranslator: Translator
module ScalaTranslator: Translator
module CommonTranslator: Translator
