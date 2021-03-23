type operator = Add | Sub | Mul | Div | Seq | Lt | Gt | Exp | Mod | Lte | Gte
			| Eq | Ne | And | Or

type unary_operator = Pp | Mm | Neg

type typ = Int | Float | Boolean | String

type bind = typ * string


type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt


type expr =
    Liti of int
  | Litf of float
  | Litb of bool
  | Id of string
  | Binop of expr * operator * expr
  | Uniop of unary_operator * expr
  | Assign of string * expr
  | Call of string * expr list

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
}


(* our program is just a list of statements *)
type program = stmt list