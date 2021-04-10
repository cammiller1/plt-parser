type operator = Add | Sub | Mul | Div | Seq | Lt | Gt | Exp | Mod | Lte | Gte
			| Eq | Ne | And | Or

type unary_operator = Pp | Mm

type typ = Int | Float | Boolean | Void | String | Array

type expr =
    Liti of int
  | Litf of float
  | Litb of bool
  | Lits of string
  | Id of string
  | Binop of expr * operator * expr
  | Uniop of unary_operator * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr


type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt * stmt
  | Elif of expr * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type bind = typ * string * expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
}

type program = bind list * func_decl list * stmt list