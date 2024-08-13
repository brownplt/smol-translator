open SExpression

type annotated<'it, 'ann> = {it: 'it, ann: 'ann}

module Print: {
  type rec t =
  | Plain(string)
  | Group(list<annotated<t, option<srcrange>>>)
  let toString: t => string
  let map: (string => string, t) => t
}

type constant = Uni | Nil | Num(float) | Lgc(bool) | Str(string)

type rec val =
  | Con(constant)
  | Lst(list<val>)
  | Vec(list<val>)
type outputlet =
  | OVal(val)
  | OErr
type output = list<outputlet>

module Primitive: {
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
  let toString: t => string
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

// Parse

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
    | SExprKindError(SExprKind.t, string, SExpression.annotated<SExpression.t>)
    | SExprArityError(Arity.t, string, list<SExpression.annotated<SExpression.t>>)
    | LiteralSymbolError(string)
    | LiteralListError(SExpression.annotated<SExpression.t>)
    | TermKindError(TermKind.t, string, term<srcrange>)
  let toString: t => string
}
type exn += SMoLParseError(ParseError.t)
module Parser: {
  let parseOutput: string => output
  let parseProgram: string => program<srcrange>
}

// Print

type exn += SMoLPrintError(string)
type printAnn = {srcrange: srcrange, print: Print.t}
module type Printer = {
  let printOutput: output => string
  let printProgram: (bool, program<srcrange>) => string
  let printProgramFull: (bool, program<srcrange>) => program<printAnn>
}

module SMoLPrinter: Printer
module JSPrinter: Printer
module PYPrinter: Printer
// module ScalaPrinter: Printer
// module CommonPrinter: Printer

module TranslateError: {
  type t =
    | ParseError(ParseError.t)
    | PrintError(string)
  let toString: t => string
}
type exn += SMoLTranslateError(TranslateError.t)
module type Translator = {
  let translateOutput: string => string
  let translateProgram: (bool, string) => string
  let translateProgramFull: (bool, string) => program<printAnn>
}
module JSTranslator: Translator
module PYTranslator: Translator
// module ScalaTranslator: Translator
// module CommonTranslator: Translator
