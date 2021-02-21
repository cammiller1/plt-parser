type operator = Add | Sub | Mul | Div | Exp | Mod | Lt | Gt | Lte | Gte | Eq | Ne | And | Or | Pp | Mm

type expr =
    Binop of expr * operator * expr
  | Uniop of expr * operator
  | Liti of int
  | Litf of float
  | Bool of bool
  | Var of string
  | Assi of string * expr
  | Seq of expr * expr