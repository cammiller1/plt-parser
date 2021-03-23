(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
(* context = the thing we need to pass to certain LLVM functions 
internally: some C++ class 
REMINDER: L.pointer_type i8_t IS A CHAR POINTER. WILL NEED FOR STRINGS
*)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Jpie" in


  (* Get types from the context *)
  (* llvm only supports primitive types *)
  let i32_t      = L.i32_type       context  (* 32-bit int type *)
  and i8_t       = L.i8_type        context  (* caracters *)
  and i1_t       = L.i1_type        context  (* boolean type *)
  and float_t    = L.double_type    context  (* double/float type *)
  and void_t     = L.void_type      context  (* void type *)
  and string_t   = L.pointer_type   i8_t     (* pointer type to char *)
  in


  (* Return the LLVM type for a Jpie type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Boolean  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> string_t  (* added for our project *)
  in

  (* Create a map of global variables after creating each *)
  (* This is basically the symbol table for variables *)
  (* Goes through and for each thing, applies global_var function *)
  let global_vars : L.llvalue StringMap.t =
    (* binding/bind *)
    let global_var m (t, n) =
      (* for each glboal we are giving them an initial value of 0 *)
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      (* *)
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declaring external functions *)
  (* create a link to the C library's "printf" *)
  let printf_t : L.lltype = 
  	  (* the [| and |] indicates an Ocaml array*)
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  	(* below LLVM's connection to a built-in function *)
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  (* We are basically defining function protoypes *)
  (* Define each function (arguments and return type) so we can 
   call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in






