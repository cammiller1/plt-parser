(* Semantically-checked Abstract Syntax Tree*)
open Ast

(* open Printf *)

type sexpr = typ * sx
and sx =
    SLiti of int
  | SLitf of string
  | SLitb of bool
  | SLits of string
  | SAssign of string * sexpr
  | SId of string
  | SArray of typ * int  (* this will be an interesting case *)
  | SBinop of sexpr * operator * sexpr
  | SUniop of unary_operator * sexpr
  | SCall of string * sexpr list
  | SNoexpr


type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sbind = typ * string * sexpr

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : sbind list;
    slocals : sbind list;
    sbody : sstmt list;
}

type sprogram = sbind list * sfunc_decl list * sstmt list


(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiti(l) -> string_of_int l
  | SLitf(l) -> l
  | SLitb(true) -> "True"
  | SLitb(false) -> "False"
  | SLits(l) -> l
  | SArray(t, size) -> "an array"  (* TODO: make me better *)
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUniop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
          ) ^ ")"            

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_vdecl (t, id, e) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"


let string_of_sprogram (vars, funcs, statements) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt statements)
