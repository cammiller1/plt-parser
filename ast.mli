type operator = Add | Sub | Mul | Div | Seq

type expr =
    Binop of expr * operator * expr
  | Assnop of string * expr
  | Var of string
  | Lit of int