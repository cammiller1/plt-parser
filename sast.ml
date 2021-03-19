open Ast

type sexpr = typ * sx
type sx =
    SLiti of int
  | SLitf of string
  | SLitb of bool
  | SVar of string
  | SBinop of sexpr * op * sexpr
  | SUniop of uop * sexpr
  | SAssign of string * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
}

type sprogram = bind list * sfunc_decl list