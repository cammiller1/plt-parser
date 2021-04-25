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
*)

let translate (globals, functions, statements) =
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
  (* and array_t    = L.array_type     (L.i32_type context) *)
  in

  (* Return the LLVM type for a complyed type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Boolean  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> string_t
    (* | A.Array -> array_t *)
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
   Array.of_list (List.map (fun (t,_, e) -> ltype_of_typ t) fdecl.sformals)
       in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
       StringMap.add name (L.define_function name ftype the_module, fdecl) m in
     List.fold_left function_decl StringMap.empty functions in



  (* Generate a name for the main function *)    

     let main_name = "main" in


     let test_main_name main_name =
        try 
          let (_,the_function) = StringMap.find main_name function_decls
          in the_function.sfname
        with Not_found -> main_name

    in 

    let rec generate_main_name name = (* match test_main_name name with *)
      if test_main_name name == name then name
      else generate_main_name (name ^ "0")


  in let main_name = generate_main_name main_name in

  let function_decls = StringMap.add main_name (L.define_function main_name main_t the_module, ({styp = Int; sfname = main_name; sformals = []; slocals = []; sbody = [] })) function_decls

    

  in

  (* creating a fake main funcion to wrap the entire script in *)
  (* Needs to occur outside of the build_statement function *)
  (* Need to find the main function with the most zeros at the end *)
  let (the_function, _) = StringMap.find main_name function_decls in
  let builder = L.builder_at_end context (L.entry_block the_function) in



(********* THIS EXPR BUILDER IS SOLELY FOR INITIALIZATION!!!! ******)
(* Construct code for an expression in the INITIALIZATION; return its value *)
let rec expr ((_, e) : sexpr) = match e with
    SLiti i  -> L.const_int i32_t i
  | SLitb b  -> L.const_int i1_t (if b then 1 else 0)
  | SLitf l -> L.const_float_of_string float_t l
  | SLits s -> L.build_global_stringptr s "str" builder
  | SNoexpr     -> L.const_int i32_t 0
  | SAssign (s, e) -> expr e
  (* | SArray(t, size) -> L.pointer_type (L.array_type t size) *)

  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n, se) = 
      let init = match se with
        (A.Void, _) ->  (
            match t with
              A.Float -> L.const_float (ltype_of_typ t) 0.0
            | A.Int -> L.const_int (ltype_of_typ t) 0
            | A.Boolean -> L.const_int (ltype_of_typ t) 0
            | A.String -> L.const_pointer_null (ltype_of_typ t)
            (* arrays should never match here *)
          )
        | (A.Array, _) -> L.const_pointer_null (ltype_of_typ A.String)
        (* give some bogus value here for now to get past arrays *)
        
        (*| (A.Array, _) -> (match snd se with
                    SArray(t, size) -> L.build_array_malloc (ltype_of_typ t) (L.const_int i32_t size) "arr" builder )
        *)
        | _ -> expr se
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Create a map of global variables after creating each *)
  let global_var_types : A.typ StringMap.t =
    let global_var m (t, n, se) = 
      let atype = t
      in StringMap.add n t m in
    List.fold_left global_var StringMap.empty globals in




  (******** BUILD FUNCTIONS ************)

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let f_builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" f_builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" f_builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" f_builder in


    (********* THIS EXPR BUILDER IS SOLELY FOR INITIALIZATION!!!! ******)
    (* Construct code for an expression in the INITIALIZATION; return its value *)
    let rec expr ((_, e) : sexpr) = match e with
        SLiti i  -> L.const_int i32_t i
      | SLitb b  -> L.const_int i1_t (if b then 1 else 0)
      | SLitf l -> L.const_float_of_string float_t l
      | SLits s -> L.build_global_stringptr s "str" f_builder
      | SNoexpr     -> L.const_int i32_t 0
      (* | SId s       -> L.build_load (lookup s) s f_builder *)
      | SAssign (s, e) -> expr e

  in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n, se) p = 
        L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n f_builder 
          in ignore (L.build_store p local f_builder); 
          StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n, se) =
        L.set_value_name n (expr se);
        let local_var = L.build_alloca (ltype_of_typ t) n f_builder
          in ignore (L.build_store (expr se) local_var f_builder);
        StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    (* Create a map of global variables after creating each *)
  let local_var_types : A.typ StringMap.t =
    let local_var m (t, n, se) = 
      let atype = t
      in StringMap.add n t m in
    List.fold_left local_var StringMap.empty fdecl.slocals in
  


    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let find_type n = try StringMap.find n local_var_types
                   with Not_found -> raise (Failure "local variable type not found")
    in


    (* Construct code for an expression; return its value *)
    (* An expression in LLVM always turns into code in a single basic block (not true for stmts) *)
    (* build instructions in the given builder that evaluate the expr; return the expr's value *)
    let rec expr f_builder ((_, e) : sexpr) = match e with
        SLiti i  -> L.const_int i32_t i
      | SLitb b  -> L.const_int i1_t (if b then 1 else 0)
      | SLitf l -> L.const_float_of_string float_t l
      | SLits s -> L.build_global_stringptr s "str" f_builder
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s f_builder
      | SAssign (s, e) -> let e' = expr f_builder e in
                          ignore(L.build_store e' (lookup s) f_builder); e'
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
        let e1' = expr f_builder e1
        and e2' = expr f_builder e2 in
          (match op with 
              A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Mul    -> L.build_fmul
            | A.Div     -> L.build_fdiv 
            | A.Eq   -> L.build_fcmp L.Fcmp.Oeq
            | A.Ne     -> L.build_fcmp L.Fcmp.One
            | A.Lt    -> L.build_fcmp L.Fcmp.Olt
            | A.Lte     -> L.build_fcmp L.Fcmp.Ole
            | A.Gt -> L.build_fcmp L.Fcmp.Ogt
            | A.Gte     -> L.build_fcmp L.Fcmp.Oge
            | A.And | A.Or ->
                raise (Failure "internal error: semant should have rejected and/or on float")
            ) e1' e2' "tmp" f_builder
              | SBinop (e1, op, e2) ->
            let e1' = expr f_builder e1
            and e2' = expr f_builder e2 in
            (match op with
              A.Add     -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Mul    -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Eq   -> L.build_icmp L.Icmp.Eq
            | A.Ne     -> L.build_icmp L.Icmp.Ne
            | A.Lt    -> L.build_icmp L.Icmp.Slt
            | A.Lte     -> L.build_icmp L.Icmp.Sle
            | A.Gt -> L.build_icmp L.Icmp.Sgt
            | A.Gte     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" f_builder
      | SUniop(op, ((t, _) as e)) ->
          let e' = expr f_builder e in
        (match op with
            A.Not -> L.build_not) e' "tmp" f_builder
      | SCall ("print", [e]) ->
          let e' = expr f_builder e in
          ( match e with
              (_, SLiti i)  -> L.build_call printf_func [| int_format_str ; e' |] "printf" f_builder
            | (_, SLitb b)  -> L.build_call printf_func [| int_format_str ; e' |] "printf" f_builder
            | (_, SLitf l) -> L.build_call printf_func [| float_format_str ; e' |] "printf" f_builder
            | (_, SLits s) -> L.build_call printf_func [| string_format_str ; e' |] "printf" f_builder
            | (_, SId s) ->
                ( match find_type s with
                      A.Int     -> L.build_call printf_func [| int_format_str ; e' |] "printf" f_builder
                    | A.Boolean      -> L.build_call printf_func [| int_format_str ; e' |] "printf" f_builder
                    | A.Float   -> L.build_call printf_func [| float_format_str ; e' |] "printf" f_builder
                    | A.String  -> L.build_call printf_func [| string_format_str ; e' |] "printf" f_builder
                    | _ -> raise (Failure "invalid argument called on the print function")
                )
            | (_, SBinop ((A.Float,_ ) as e1, op, e2)) ->  L.build_call printf_func [| float_format_str ; e' |] "printf" builder
            | (_, SBinop (e1, op, e2)) ->  L.build_call printf_func [| int_format_str ; e' |] "printf" f_builder
            | (_, SCall(f, args)) -> L.build_call printf_func [| int_format_str ; e' |] "printf" f_builder
            | (_,_) ->  raise (Failure "invalid argument called on the print function")
          )
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
           let llargs = List.rev (List.map (expr f_builder) (List.rev args)) in
           let result = (match fdecl.styp with 
              A.Void -> ""
             | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list llargs) result f_builder
    in


    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal f_builder instr =
      match L.block_terminator (L.insertion_block f_builder) with
          Some _ -> ()
        | None -> ignore (instr f_builder) in


    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
     let rec stmt f_builder = function
        SBlock sl -> List.fold_left stmt f_builder sl
        | SExpr e -> ignore(expr f_builder e); f_builder
        | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void f_builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr f_builder e) f_builder );
                     f_builder
        | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr f_builder predicate in
         let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

         let then_bb = L.append_block context "then" the_function in
         add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
           build_br_merge;

         let else_bb = L.append_block context "else" the_function in
         add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
           build_br_merge;

           ignore(L.build_cond_br bool_val then_bb else_bb f_builder);
           L.builder_at_end context merge_bb
        | SWhile (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore(L.build_br pred_bb f_builder);

          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (stmt (L.builder_at_end context body_bb) body)
            (L.build_br pred_bb);

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder predicate in

          let merge_bb = L.append_block context "merge" the_function in
          ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb
        (* Implement for loops as while loops *)
        | SFor (e1, e2, e3, body) -> stmt f_builder
            ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )

    in

     (* Build the code for each statement *)
    let f_builder = stmt f_builder (SBlock fdecl.sbody) in


    (* Add a return if the last block falls off the end *)
    add_terminal f_builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in


  (****** build statements *********)

  let build_statements statements =
 
     let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
     and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
     and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
  in


    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n global_vars
                   with Not_found -> raise (Failure "global variable not found")
    in

    let find_type n = try StringMap.find n global_var_types
                   with Not_found -> raise (Failure "global variable type not found")
    in


    (* Construct code for an expression; return its value *)
    (* An expression in LLVM always turns into code in a single basic block (not true for stmts) *)
    (* build instructions in the given builder that evaluate the expr; return the expr's value *)
    let rec expr builder ((_, e) : sexpr) = match e with
        SLiti i  -> L.const_int i32_t i
      | SLitb b  -> L.const_int i1_t (if b then 1 else 0)
      | SLitf l -> L.const_float_of_string float_t l
      | SLits s -> L.build_global_stringptr s "str" builder
      | SArray(t, size) -> L.build_array_malloc (ltype_of_typ t) (L.const_int i32_t size) "arr" builder
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
          (match op with 
              A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Mul    -> L.build_fmul
            | A.Div     -> L.build_fdiv 
            | A.Eq   -> L.build_fcmp L.Fcmp.Oeq
            | A.Ne     -> L.build_fcmp L.Fcmp.One
            | A.Lt    -> L.build_fcmp L.Fcmp.Olt
            | A.Lte     -> L.build_fcmp L.Fcmp.Ole
            | A.Gt -> L.build_fcmp L.Fcmp.Ogt
            | A.Gte     -> L.build_fcmp L.Fcmp.Oge
            | A.And | A.Or ->
                raise (Failure "internal error: semant should have rejected and/or on float")
            ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
            let e1' = expr builder e1
            and e2' = expr builder e2 in
            (match op with
              A.Add     -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Mul    -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Eq   -> L.build_icmp L.Icmp.Eq
            | A.Ne     -> L.build_icmp L.Icmp.Ne
            | A.Lt    -> L.build_icmp L.Icmp.Slt
            | A.Lte     -> L.build_icmp L.Icmp.Sle
            | A.Gt -> L.build_icmp L.Icmp.Sgt
            | A.Gte     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder
      | SUniop(op, ((t, _) as e)) ->
          let e' = expr builder e in
        (match op with
            A.Not -> L.build_not) e' "tmp" builder
      | SCall ("print", [e]) ->
          let e' = expr builder e in
          ( match e with
              (_, SLiti i)  -> L.build_call printf_func [| int_format_str ; e' |] "printf" builder
            | (_, SLitb b)  -> L.build_call printf_func [| int_format_str ; e' |] "printf" builder
            | (_, SLitf l) -> L.build_call printf_func [| float_format_str ; e' |] "printf" builder
            | (_, SLits s) -> L.build_call printf_func [| string_format_str ; e' |] "printf" builder
            | (_, SId s) ->
                ( match find_type s with
                      A.Int     -> L.build_call printf_func [| int_format_str ; e' |] "printf" builder
                    | A.Boolean      -> L.build_call printf_func [| int_format_str ; e' |] "printf" builder
                    | A.Float   -> L.build_call printf_func [| float_format_str ; e' |] "printf" builder
                    | A.String  -> L.build_call printf_func [| string_format_str ; e' |] "printf" builder
                    | _ -> raise (Failure "invalid argument called on the print function")
                )
            | (_, SBinop ((A.Float,_ ) as e1, op, e2)) ->  L.build_call printf_func [| float_format_str ; e' |] "printf" builder
            | (_, SBinop (e1, op, e2)) ->  L.build_call printf_func [| int_format_str ; e' |] "printf" builder
            | (_, SCall(f, args)) -> L.build_call printf_func [| int_format_str ; e' |] "printf" builder
            | (_,_) ->  raise (Failure "invalid argument called on the print function")
          )
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
           let llargs = List.rev (List.map (expr builder) (List.rev args)) in
           let result = (match fdecl.styp with 
              A.Void -> ""
             | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list llargs) result builder
    in


    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder) in


    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
     let rec stmt builder = function
        SBlock sl -> List.fold_left stmt builder sl
        | SExpr e -> ignore(expr builder e); builder
        (* | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder *)
        | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
         let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

         let then_bb = L.append_block context "then" the_function in
         add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
           build_br_merge;

         let else_bb = L.append_block context "else" the_function in
         add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
           build_br_merge;

           ignore(L.build_cond_br bool_val then_bb else_bb builder);
           L.builder_at_end context merge_bb
        | SWhile (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore(L.build_br pred_bb builder);

          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (stmt (L.builder_at_end context body_bb) body)
            (L.build_br pred_bb);

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder predicate in

          let merge_bb = L.append_block context "merge" the_function in
          ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb
        (* Implement for loops as while loops *)
        | SFor (e1, e2, e3, body) -> stmt builder
            ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )

    in

     (* Build the code for each statement *)
    let builder = stmt builder (SBlock statements) in

    (* Add a return for the simulated main function *)
    L.build_ret (L.const_int i32_t 0) builder
  in

    (List.iter build_function_body functions, build_statements statements);
    the_module  (* return the LLVM module result *)