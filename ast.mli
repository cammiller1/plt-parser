type operator = Add | Sub | Mul | Div | Seq | Lt | Gt | Exp | Mod | Lte | Gte
			| Eq | Ne | And | Or

type unary_operator = Pp | Mm | Neg

type typ = Int | Float | Boolean | String | None

type bind = typ * string

type expr =
    Liti of int
  | Litf of float
  | Litb of bool
  | Id of string
  | Binop of expr * operator * expr
  | Uniop of unary_operator * expr
  | Assign of typ * string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | FDefine of typ * string * bind list * stmt list

(*
type fdef = {
  typ : typ;
  fname : string;
  formals : bind list;
  body : stmt list;
}
*)

type program = stmt list