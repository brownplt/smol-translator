open SExpression
open Belt

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
type constant = Uni | Num(float) | Lgc(bool) | Str(string)
type symbol = string
type rec expression =
  | Con(constant)
  | Ref(annotated<symbol>)
  | Set(annotated<symbol>, annotated<expression>)
  | Lam(list<annotated<symbol>>, block)
  | Let(list<(annotated<symbol>, annotated<expression>)>, block)
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

type result =
  | Vec(int) // the int is the address
  | Fun(int) // the int is the address
  | PrmFun(primitive)

module type Stringifier = {
  let string_of_result: result => string
  let string_of_expr: annotated<expression> => string
  let string_of_def: annotated<definition> => string
  let string_of_term: term => string
  let string_of_block: block => string
  let string_of_program: program => string
}

module Stringify: Stringifier
module StringifyAsJS: Stringifier
module StringifyAsPY: Stringifier

module type Translator = {
  let translate_program: string => string
  let translate_block: string => string
  let translate_expressions: string => string
}

module SMoLToJS : Translator
module SMoLToPY : Translator

type kind_expectation = Atom | List
type arity_expectation =
  | ExactlyOne
  | ExactlyTwo
  | ExactlyThree
  | OneThenMany
  | ManyThenOne
  | OneThenManyThenOne
type term_kind = Definition | Expression
type parse_error =
  | SExprKindError(kind_expectation, string, annotated<SExpression.t>)
  | SExprArityError(arity_expectation, string, list<annotated<SExpression.t>>)
  | LiteralSymbolError(string)
  | LiteralListError(annotated<SExpression.t>)
  | TermKindError(term_kind, string, term)
type exn += ParseError(parse_error)
let stringOfExprs: List.t<annotated<SExpression.t>> => string
let stringOfParseError: parse_error => string
type exn += ExpectingExpression
let as_expr: (string, term) => annotated<expression>
let constant_of_atom: ('a, SExpression.Atom.t) => expression
let value_of_sexpr: annotated<SExpression.t> => annotated<expression>
let expr_of_atom: (SExpression.srcrange, SExpression.Atom.t) => expression
let term_of_sexpr: annotated<SExpression.t> => term
let app_prm: (SExpression.srcrange, primitive, List.t<annotated<SExpression.t>>) => term
let terms_of_sexprs: List.t<annotated<SExpression.t>> => List.t<term>
let terms_of_string: string => List.t<term>
