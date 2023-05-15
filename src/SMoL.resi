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
  | OError
type constant = Uni | Num(float) | Lgc(bool) | Str(string)
type symbol = string
type rec expression =
  | Con(constant)
  | Ref(SExpression.annotated<symbol>)
  | Set(SExpression.annotated<symbol>, SExpression.annotated<expression>)
  | Lam(list<SExpression.annotated<symbol>>, block)
  | Let(list<(SExpression.annotated<symbol>, SExpression.annotated<expression>)>, block)
  | AppPrm(primitive, list<SExpression.annotated<expression>>)
  | App(SExpression.annotated<expression>, list<SExpression.annotated<expression>>)
  | Bgn(list<SExpression.annotated<expression>>, SExpression.annotated<expression>)
  | If(
      SExpression.annotated<expression>,
      SExpression.annotated<expression>,
      SExpression.annotated<expression>,
    )
  | Cnd(list<(SExpression.annotated<expression>, block)>, option<block>)
and block = (list<term>, SExpression.annotated<expression>)
and definition =
  | Var(SExpression.annotated<symbol>, SExpression.annotated<expression>)
  | Fun(SExpression.annotated<symbol>, list<SExpression.annotated<symbol>>, block)
and term = Def(SExpression.annotated<definition>) | Exp(SExpression.annotated<expression>)
and program = list<term>
module Stringify: {
  let unannotate: SExpression.annotated<'a> => 'a
  let indent: (Js.String.t, int) => Js.String.t
  let string_of_constant: constant => string
  let string_of_prm: primitive => string
  let string_of_list: list<string> => string
  let string_of_def_var: (SExpression.annotated<string>, string) => string
  let string_of_def_for: (SExpression.annotated<string>, string, string, Js.String.t) => string
  let string_of_def_fun: (string, list<string>, Js.String.t) => string
  let string_of_expr_set: (string, string) => string
  let string_of_expr_lam: (list<string>, Js.String.t) => string
  let string_of_expr_app: (string, list<string>) => string
  let string_of_expr_bgn: (Belt.List.t<string>, string) => string
  let string_of_expr_whl: (string, Js.String.t) => string
  let string_of_expr_cnd: (list<(string, string)>, option<Js.String.t>) => string
  let string_of_expr_if: (string, string, string) => string
  let string_of_expr_let: (
  Belt.List.t<(SExpression.annotated<string>, Js.String.t)>,
  Js.String.t,
) => string
  let string_of_expr: SExpression.annotated<expression> => symbol
  let string_of_def: SExpression.annotated<definition> => symbol
  let string_of_xe: ((SExpression.annotated<symbol>, SExpression.annotated<expression>)) => (
  SExpression.annotated<symbol>,
  Js.String.t,
)
  let string_of_eb: ((SExpression.annotated<expression>, block)) => (symbol, Js.String.t)
  let string_of_ob: option<block> => option<Js.String.t>
  let string_of_block: block => Js.String.t
  let string_of_term: term => symbol
}
let toString: term => symbol
type s_expr = SExpression.t
let stringOfSExpr: SExpression.annotated<SExpression.t> => string
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
  | SExprKindError(kind_expectation, string, SExpression.annotated<s_expr>)
  | SExprArityError(arity_expectation, string, list<SExpression.annotated<s_expr>>)
  | LiteralSymbolError(string)
  | LiteralListError(SExpression.annotated<s_expr>)
  | TermKindError(term_kind, string, term)
type exn +=  ParseError(parse_error)
let stringOfExprs: Belt.List.t<SExpression.annotated<SExpression.t>> => string
let stringOfParseError: parse_error => string
let as_id: (string, SExpression.annotated<s_expr>) => SExpression.annotated<string>
let as_list: (string, SExpression.annotated<s_expr>) => list<SExpression.annotated<SExpression.t>>
let as_one_then_many: (string, list<SExpression.annotated<s_expr>>) => (
  SExpression.annotated<s_expr>,
  list<SExpression.annotated<s_expr>>,
)
let as_many_then_one: (string, list<SExpression.annotated<s_expr>>) => (
  list<SExpression.annotated<s_expr>>,
  SExpression.annotated<s_expr>,
)
let as_one: (string, list<SExpression.annotated<s_expr>>) => SExpression.annotated<s_expr>
let as_two: (string, list<SExpression.annotated<s_expr>>) => (
  SExpression.annotated<s_expr>,
  SExpression.annotated<s_expr>,
)
let as_three: (string, list<SExpression.annotated<s_expr>>) => (
  SExpression.annotated<s_expr>,
  SExpression.annotated<s_expr>,
  SExpression.annotated<s_expr>,
)
let as_one_then_many_then_one: (string, list<SExpression.annotated<s_expr>>) => (
  SExpression.annotated<s_expr>,
  list<SExpression.annotated<s_expr>>,
  SExpression.annotated<s_expr>,
)
type exn +=  ExpectingExpression
let as_expr: (string, term) => SExpression.annotated<expression>
let constant_of_atom: ('a, SExpression.Atom.t) => expression
let value_of_sexpr: SExpression.annotated<s_expr> => SExpression.annotated<expression>
let expr_of_atom: (SExpression.srcrange, SExpression.Atom.t) => expression
let term_of_sexpr: SExpression.annotated<s_expr> => term
let app_prm: (SExpression.srcrange, primitive, Belt.List.t<SExpression.annotated<s_expr>>) => term
let terms_of_sexprs: Belt.List.t<SExpression.annotated<s_expr>> => Belt.List.t<term>
let fromString: string => Belt.List.t<term>
