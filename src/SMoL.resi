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
module Stringify: {
  let unannotate: annotated<'a> => 'a
  let indent: (Js.String.t, int) => Js.String.t
  let string_of_constant: constant => string
  let string_of_prm: primitive => string
  let string_of_list: list<string> => string
  let string_of_def_var: (annotated<string>, string) => string
  let string_of_def_for: (annotated<string>, string, string, Js.String.t) => string
  let string_of_def_fun: (string, list<string>, Js.String.t) => string
  let string_of_expr_set: (string, string) => string
  let string_of_expr_lam: (list<string>, Js.String.t) => string
  let string_of_expr_app: (string, list<string>) => string
  let string_of_expr_bgn: (Belt.List.t<string>, string) => string
  let string_of_expr_whl: (string, Js.String.t) => string
  let string_of_expr_cnd: (list<(string, string)>, option<Js.String.t>) => string
  let string_of_expr_if: (string, string, string) => string
  let string_of_expr_let: (
    Belt.List.t<(annotated<string>, Js.String.t)>,
    Js.String.t,
  ) => string
  let string_of_expr: annotated<expression> => symbol
  let string_of_def: annotated<definition> => symbol
  let string_of_xe: ((annotated<symbol>, annotated<expression>)) => (
    annotated<symbol>,
    Js.String.t,
  )
  let string_of_eb: ((annotated<expression>, block)) => (symbol, Js.String.t)
  let string_of_ob: option<block> => option<Js.String.t>
  let string_of_block: block => Js.String.t
  let string_of_term: term => symbol
}
let toString: term => symbol
type s_expr = SExpression.t
let stringOfSExpr: annotated<SExpression.t> => string
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
  | SExprKindError(kind_expectation, string, annotated<s_expr>)
  | SExprArityError(arity_expectation, string, list<annotated<s_expr>>)
  | LiteralSymbolError(string)
  | LiteralListError(annotated<s_expr>)
  | TermKindError(term_kind, string, term)
type exn += ParseError(parse_error)
let stringOfExprs: Belt.List.t<annotated<SExpression.t>> => string
let stringOfParseError: parse_error => string
type exn += ExpectingExpression
let as_expr: (string, term) => annotated<expression>
let constant_of_atom: ('a, SExpression.Atom.t) => expression
let value_of_sexpr: annotated<s_expr> => annotated<expression>
let expr_of_atom: (SExpression.srcrange, SExpression.Atom.t) => expression
let term_of_sexpr: annotated<s_expr> => term
let app_prm: (SExpression.srcrange, primitive, Belt.List.t<annotated<s_expr>>) => term
let terms_of_sexprs: Belt.List.t<annotated<s_expr>> => Belt.List.t<term>
let fromString: string => Belt.List.t<term>
