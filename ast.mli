type operator = Add | Sub | Mul | Div

type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Var of string
  | Assi of string * expr
  | Seq of expr * expr