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

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)