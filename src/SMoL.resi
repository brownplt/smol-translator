open SExpression
open Belt

type rec printNode<'id> =
  | Plain(string)
  | Group(list<print<'id>>)
and print<'id> = annotated<printNode<'id>, option<'id>>

module Print: {
  type t<'id> = printNode<'id>
  let toString: t<'id> => string
  let toSourceMap: (
    print<'key>,
    Map.id<'key, 'entity>,
  ) => Map.t<'key, SExpression.sourceLocation, 'entity>
  let map: (string => string, t<'id>) => t<'id>
  let concat2: (annotated<t<'id>, option<'id>>, string, annotated<t<'id>, option<'id>>) => t<'id>
  let concat: (string, list<annotated<t<'id>, option<'id>>>) => t<'id>
  let pad: (string, annotated<t<'id>, option<'id>>, string) => t<'id>
  let dummyAnn: t<'id> => print<'id>
}

type constant =
  | Uni
  | Nil
  | Num(float)
  | Lgc(bool)
  | Str(string)
  | Sym(string)

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

and blockNode<'ann> =
  | BRet(expressionNode<'ann>)
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

let xsOfBlock: block<'ann> => list<annotated<symbol, 'ann>>
let xsOfProgram: program<'ann> => list<annotated<symbol, 'ann>>

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
    | SExprKindError(SExprKind.t, string, sexpr)
    | SExprArityError(Arity.t, string, list<sexpr>)
    | LiteralSymbolError(string)
    | LiteralListError(sexpr)
    | TermKindError(TermKind.t, string, term<sourceLocation>)
  let toString: t => string
}
type exn += SMoLParseError(ParseError.t)
module Parser: {
  let parseOutput: string => output
  let parseProgram: string => program<sourceLocation>
}

// Print

type exn += SMoLPrintError(string)
type printAnn = {sourceLocation: sourceLocation, print: Print.t<sourceLocation>}
module type Printer = {
  let printName: string => string
  let printOutput: output => string
  let printStandAloneTerm: term<sourceLocation> => string
  let printProgram: (bool, program<sourceLocation>) => string
  let printProgramFull: (bool, program<sourceLocation>) => program<printAnn>
}

let getPrint: annotated<'a, printAnn> => print<sourceLocation>

module SMoLPrinter: Printer
module JSPrinter: Printer
module PYPrinter: Printer
module PCPrinter: Printer
module SCPrinter: Printer

module TranslateError: {
  type t =
    | ParseError(ParseError.t)
    | PrintError(string)
    | KindError(string)
  let toString: t => string
}
type exn += SMoLTranslateError(TranslateError.t)
module type Translator = {
  let translateName: string => string
  let translateOutput: string => string
  let translateStandAloneTerm: string => string
  let translateProgram: (bool, string) => string
  let translateProgramFull: (bool, string) => program<printAnn>
}
module JSTranslator: Translator
module PYTranslator: Translator
module PCTranslator: Translator
module SCTranslator: Translator
