(* Semantic checking for the Jpie compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (statements) =

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = [(ty, "x")];
      body = [] } map 
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
                               ("printb", Boolean);
                               ("printf", Float); 
                               ("prints", String) ]
  in


  (* Return a function from our built_in symbol table *)
  let find_func s = 
    try StringMap.find s built_in_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  


  let check_statement statements = 

   (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   


    (* Build global symbol table of variables *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty []
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* check if variable has already been declared *)
    let check_already_declared s =
      let declared = StringMap.find s symbols in
      if declared then raise (Failure ("variable " ^ s ^ " already declared" ))
      else s
    in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else lvaluet (* raise (Failure err) *)
    in

     (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Liti l -> (Int, SLiti l)
      | Litf l -> (Float, SLitf l)
      | Litb l  -> (Boolean, SLitb l)
      | Lits l  -> (String, SLits l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment "
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("wrong number of args "))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found "
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | VDeclare (typ, str, e) -> 
          match e with
          None -> SVDecl(typ, str, None)
          | _ -> let e' = expr e in
            SVDeclare(ty, name, e')
          (*
          let e' = expr e in
          (* check valid declaration and add to symbol table *)
          let ty = match typ with
            Void -> raise ( Failure ("illegal void assignment variable " ^ str) )
            | _ -> typ
          and name = check_already_declared str
           in StringMap.add name ty symbols in
          let name2 = name *)

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


  in (check_statement statements)