(* Semantically-checked Abstract Syntax Tree*)
open Ast

type sexpr = typ * sx
and sx =
    SLiti of int
  | SLitf of float
  | SLitb of bool
  | SLits of string
  | SId of string
  | SBinop of sexpr * operator * sexpr
  | SUniop of unary_operator * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr


type sstmt =
    SBlock of sstmt list
  | SDeclareAssign of typ * string * sexpr
  | SDeclare of typ * string
  | SFuncDef of sfunc_decl
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

and sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
}

type sprogram = sstmt list