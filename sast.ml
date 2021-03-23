(* Semantically-checked Abstract Syntax Tree*)
open Ast

type styp = SInt | SFloat | SBoolean | SString | Snone

type sexpr = styp * sx
type sx =
    SLiti of int
  | SLitf of string
  | SLitb of bool
  | SVar of string
  | SBinop of sexpr * op * sexpr
  | SUniop of uop * sexpr
  | SAssign of styp * string * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SFDefine of styp * string * bind list * sstmt list

type sprogram = sstmt list