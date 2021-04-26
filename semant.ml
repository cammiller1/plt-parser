(* Semantic checking for the ComPyled compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check (globals, functions, statements) =


  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
      (Void, b, e) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      | ((_,n1,_) :: (_,n2,_) :: _) when n1 = n2 ->
    raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a,_) (_,b,_) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;


    (* drop the expression "e" from being stored in the symbol table *)
    let symbols = List.fold_left (fun m (ty, name, e) -> StringMap.add name ty m)
                  StringMap.empty globals
    in

    let global_arrays = List.find_all (fun (ty, name, e) -> ty = Array) globals

  in
    
    let array_symbols = List.fold_left (fun m (ty, name, e) -> match e with
         LitArray(t, size) -> StringMap.add name t m)
                  StringMap.empty global_arrays
    in

    (* Return a variable from our temp symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    let rec expr = function
        Liti l -> (Int, SLiti l)
      | Litf l -> (Float, SLitf l)
      | Litb l  -> (Boolean, SLitb l)
      | Lits l  -> (String, SLits l)
      | Noexpr     -> (Void, SNoexpr)
      | LitArray(t, size) -> (Array, SLitArray(t, size))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mul | Div when same && t1 = Int   -> Int
          | Add | Sub | Mul | Div when same && t1 = Float -> Float
          | Eq | Ne            when same               -> Boolean
          | Lt | Lte | Gt | Gte
                     when same && (t1 = Int || t1 = Float) -> Boolean
          | And | Or when same && t1 = Boolean -> Boolean
          | _ -> raise (
        Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Uniop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Not when t = Boolean -> Boolean
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUniop(op, (t, e')))

  in
  
  (***** CHECK expressions of global variables ****)
   (* Return a semantically-checked expression, i.e., with a type *)
  let check_globals global = 

    let return_checked_global (t, s, e) =
      match e with 
        | Noexpr     -> (t, s, expr e)
        | _ ->
          let lt = type_of_identifier s
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr e
          in check_assign lt rt err;
      (t, s, expr e)

    in return_checked_global global

  in




  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = [(ty, "x", Noexpr)];
      locals = [];
      body = [] } map 
    in List.fold_left add_bind StringMap.empty [ ("print", Int) ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  let main_name = "main"
  in

  let find_main name =
    try (StringMap.find name function_decls).fname
    with Not_found -> ""
  in


  (* add the main function. If user defined a function with same name, recurse to 
     create one with a name they didn't use.
     Just keep adding a "0" after main's function name until no matches
   *)

  let rec create_main name = match find_main name with
        "" -> StringMap.add name {typ = Int; fname = name; formals = []; locals = []; body = [] } function_decls
       | _ -> create_main (name ^ "0")
  in 
  let function_decls = create_main main_name
  
  in

  

  (**** Check Functions ****)


  (* Return a function from our built_in symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;


  (* FOR LOCAL VAR INIT and formal check *)
   (* Build local symbol table to check the types of the initializations *)
    (* drop the expression "e" from being stored in the symbol table*)
    let local_symbols = List.fold_left (fun m (ty, name, e) -> StringMap.add name ty m)
                  StringMap.empty (globals @ func.formals @ func.locals)
    in

    let local_arrays = List.find_all (fun (ty, name, e) -> ty = Array) func.locals

    in
    
    let local_array_symbols = List.fold_left (fun m (ty, name, e) -> match e with
         LitArray(t, size) -> StringMap.add name t m)
                  StringMap.empty local_arrays
    in

    (* Return a variable from the local symbol table *)
    let type_of_identifier s =
      try StringMap.find s local_symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a array type from our array symbol table *)
    let type_of_array_identifier s =
      try StringMap.find s local_array_symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let check_print lvaluet rvaluet err =
       lvaluet
    in

     (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Liti l -> (Int, SLiti l)
      | Litf l -> (Float, SLitf l)
      | Litb l  -> (Boolean, SLitb l)
      | Lits l  -> (String, SLits l)
      | LitArray(t, size) -> (Array, SLitArray(t, size))
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let rt = if rt = Array then let SArrayIndexAccess(var, idx) = e' in type_of_array_identifier var else rt
          in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in 
          (check_assign lt rt err, SAssign(var, (rt, e')))
      | ArrayIndexAssign(var, idx, e) ->
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          (Array, SArrayIndexAssign(var, idx, (rt, e')))
      | ArrayIndexAccess(var, idx) -> (Array, SArrayIndexAccess(var, idx))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mul | Div when same && t1 = Int   -> Int
          | Add | Sub | Mul | Div when same && t1 = Float -> Float
          | Eq | Ne            when same               -> Boolean
          | Lt | Lte | Gt | Gte
                     when same && (t1 = Int || t1 = Float) -> Boolean
          | And | Or when same && t1 = Boolean -> Boolean
          | _ -> raise (
        Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Uniop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Not when t = Boolean -> Boolean
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUniop(op, (t, e')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("wrong number of args "))
          else let check_call (ft, _, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found "
            (* skip print function *)
            in if fname = "print" then (check_print ft et err, e') else (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in
  
  
  let check_locals local = 
    let return_checked_locals (t, s, e) =
      match e with 
          | Noexpr     -> (t, s, expr e)
          | _ ->
            let lt = type_of_identifier s
            and (rt, e') = expr e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
              string_of_typ rt ^ " in " ^ string_of_expr e
            in check_assign lt rt err;
        (t, s, expr e)

       in return_checked_locals local

  in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Boolean then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
          SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
          if t = func.typ then SReturn (t, e') 
          else raise ( Failure ("return gives " ^ string_of_typ t ^ " expected " ^
            string_of_typ func.typ ^ " in " ^ string_of_expr e))
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)


    (* Need to return semantically checked expressions for locals *)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = (List.map check_locals func.formals);
      slocals  = (List.map check_locals func.locals);
      sbody = match check_stmt (Block func.body) with
  SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }








in

  (* Return a function from our built_in symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  

  let check_statement statements = 

    (* Return a variable from the global symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a array type from the global array symbol table *)
    let type_of_array_identifier s =
      try StringMap.find s array_symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let check_print lvaluet rvaluet err =
       lvaluet
    in


     (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Liti l -> (Int, SLiti l)
      | Litf l -> (Float, SLitf l)
      | Litb l  -> (Boolean, SLitb l)
      | Lits l  -> (String, SLits l)
      | LitArray(t, size) -> (Array, SLitArray(t, size))
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let rt = if rt = Array then let SArrayIndexAccess(var, idx) = e' in type_of_array_identifier var else rt
          in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in 
          (check_assign lt rt err, SAssign(var, (rt, e')))
      | ArrayIndexAssign(var, idx, e) as ex ->
          let lt = type_of_array_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in
          (check_assign lt rt err, SArrayIndexAssign(var, idx, (rt, e')))
      | ArrayIndexAccess(var, idx) -> (type_of_array_identifier var, SArrayIndexAccess(var, idx))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mul | Div when same && t1 = Int   -> Int
          | Add | Sub | Mul | Div when same && t1 = Float -> Float
          | Eq | Ne            when same               -> Boolean
          | Lt | Lte | Gt | Gte
                     when same && (t1 = Int || t1 = Float) -> Boolean
          | And | Or when same && t1 = Boolean -> Boolean
          | _ -> raise (
        Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Uniop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Not when t = Boolean -> Boolean
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUniop(op, (t, e')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("wrong number of args "))
          else let check_call (ft, _, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found "
            (* skip print function *)
            in if fname = "print" then (check_print ft et err, e') else (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in


    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Boolean then raise (Failure err) else (t', e')
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
          SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
          SReturn (t, e')
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in
    
    match check_stmt (Block statements) with
        SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
  


  in (List.map check_globals globals, List.map check_function functions, check_statement statements)