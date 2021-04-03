type operator = Add | Sub | Mul | Div | Seq | Lt | Gt | Exp | Mod | Lte | Gte
			| Eq | Ne | And | Or

type unary_operator = Pp | Mm

type typ = Int | Float | Boolean | Void | String | Array

type bind = typ * string

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
  | VDeclareAssign of typ * string * expr
  | VDeclare of typ * string
  | FuncDef of func_decl
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

and func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
}

type program = stmt list