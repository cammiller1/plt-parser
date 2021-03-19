type operator = Add | Sub | Mul | Div | Seq | Lt | Gt | Exp | Mod | Lte | Gte
			| Eq | Ne | And | Or

type unary_operator = Pp | Mm | Neg

type typ = Int | Float | Boolean | String

type bind = typ * string

type expr =
    Liti of int
  | Litf of string
  | Litb of bool
  | Var of string
  | Binop of expr * op * expr
  | Uniop of uop * expr
  | Assign of string * expr
  | Call of string * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
 }

 type program = bind list * func_decl list