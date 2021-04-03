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
let translate (statements) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "complyed" in


  (* Get types from the context *)
  (* llvm only supports primitive types *)
  let i32_t      = L.i32_type       context  (* 32-bit int type *)
  and i8_t       = L.i8_type        context  (* caracters *)
  and i1_t       = L.i1_type        context  (* boolean type *)
  and float_t    = L.double_type    context  (* double/float type *)
  and void_t     = L.void_type      context   (* void type *)
  and string_t   = L.pointer_type   (L.i8_type context)      (* pointer type to char *)
  in

  (* Return the LLVM type for a complyed type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Boolean  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> string_t (* added for our project *)
  in


  (* Declaring external functions *)
  (* create a link to the C library's "printf" *)
  let printf_t : L.lltype = 
      (* the [| and |] indicates an Ocaml array*)
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    (* below LLVM's connection to a built-in function *)
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  (* create fake main function *)
  let main_t : L.lltype = 
      (* the [| and |] indicates an Ocaml array*)
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  (*  (* below LLVM's connection to a built-in function *)
  let main_func : L.llvalue = 
      L.declare_function "main" printf_t the_module in *)


  (* Define each function (arguments and return type) so we can 
      call it even before we've created its body *)
   (* define a main function to wrap out program in *)
   let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
     let function_decl m fdecl =
       let name = fdecl.sfname
       and formal_types = 
   Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
       in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
       StringMap.add name (L.define_function name ftype the_module, fdecl) m in
     List.fold_left function_decl StringMap.empty [] in
 
 
    let function_decls = StringMap.add "main" (L.define_function "main" main_t the_module, ({styp = Int; sfname = "main"; sformals = []; sbody = [] })) function_decls
 
 
  in

    
  (* entry point *)
  let build_statements statements =

      (* this needs to get moved when we have user functions *)
      (* creating a fake main funcion to wrap the entire script in *)
      (* Needs to occur outside of the build_statement function *)
     let (the_function, _) = StringMap.find "main" function_decls in
     let builder = L.builder_at_end context (L.entry_block the_function) in
 
     let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
     and float_format_str = L.build_global_stringptr "%f\n" "fmt" builder
     and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
  in


  (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
  StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
  let local_var = L.build_alloca (ltype_of_typ t) n builder
  in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

     (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in


        (* Construct code for an expression; return its value *)
    (* An expression in LLVM always turns into code in a single basic block (not true for stmts) *)
    (* build instructions in the given builder that evaluate the expr; return the expr's value *)
    let rec expr builder ((_, e) : sexpr) = match e with
        SLiti i  -> L.const_int i32_t i
      | SLitb b  -> L.const_int i1_t (if b then 1 else 0)
      | SLitf l -> L.const_float float_t l
      | SLits s -> L.build_global_stringptr s "str" builder
      | SNoexpr     -> L.const_int i32_t 0
      (* | SId s       -> L.build_load (lookup s) s builder *)

      (* a bunch of stuff between here *) 
      
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
          L.build_call printf_func [| int_format_str ; (expr builder e) |]
            "printf" builder

      | SCall ("prints", [e]) ->
          L.build_call printf_func [| string_format_str ; (expr builder e) |]
            "printf" builder
      
      | SCall ("printf", [e]) -> 
          L.build_call printf_func [| float_format_str ; (expr builder e) |]
          "printf" builder
    in


    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
     let rec stmt builder = function
        SBlock sl -> List.fold_left stmt builder sl
        | SExpr e -> ignore(expr builder e); builder
        | SVDeclare (typ, str) -> 
            let L.build_alloca (ltype_of_typ typ) str builder
            in add_local local_vars (typ, str)

    in

     (* Build the code for each statement *)
    let builder = stmt builder (SBlock statements) in

    (* Add a return for the simulated main function *)
    L.build_ret (L.const_int i32_t 0) builder
  in

    build_statements statements;
    the_module  (* return the LLVM module result *)