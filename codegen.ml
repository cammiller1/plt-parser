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
  (* and string_t   = L.pointer_type   i8_t     (* pointer type to char *)
  *)
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

  (* We are basically defining function protoypes for all of the USER-DEFINED functions *)
  (* Define each function (arguments and return type) so we can 
   call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname  (* the name of the function *)
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
	  (* ltype_of_type is the return type from the function declaration *)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      (* Call L.define_function from LLVM on our our function declaration (ie.e m fdecl above) *)
      (* Add the result to the stringmap/our symbol table *)
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    (* stuff all the function definitions into the stringmap *)
    List.fold_left function_decl StringMap.empty functions in


  (* Fill in the body of the given function *)
  (* very-much like C++, very stateful *)
  let build_function_body fdecl =
    (* "the_function" is going to be the one we created we created in let function_decls: *)
    let (the_function, _) = 
    	StringMap.find fdecl.sfname function_decls in  (* get function def from stringmap *)
    let builder =
    	(* entry_block - the initial basic block that every function has *)
    	(* TODO: "context" may be an issue for how we are doing string pointer.... *) 
    	L.builder_at_end context (L.entry_block the_function) in
    (* The instruction builder is basically a pointer that keeps track of where in the 
       current basic block we are going to put the next instructon
     *)

    (* below we are creating string literals that we are going to pass to printf *)
    let int_format_str = 
    	L.build_global_stringptr "%d\n" "fmt" builder  (* print integers *)
    and float_format_str = 
    	L.build_global_stringptr "%g\n" "fmt" builder (* print floats *) 
    in


    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "local_vars" map *)
    (* the locals maps are going to be like sub-symbol tables like we learned in class *)
    let local_vars =
    	(* m = map of names, t = the bind/type of the formal args, p = LLVM function parameter for it *)
      	
      	(* give the function a name, store the value*)
      let add_formal m (t, n) p = 
        
        L.set_value_name n p;  (* give the value a name *)
		(* build at the build. updates "build" as a side effect *)
		
		let local = L.build_alloca (ltype_of_typ t) n builder in  (* allocate memory for the formal param value *)
        
        ignore (L.build_store p local builder);  (* store the value of the formal param in the allocated location on the stack*)
		
		StringMap.add n local m (* add the local var to the local stringmap! *)


	(* Allocate space for any locally declared variables and add the
       * resulting registers to our map. This way we can "get the value of a formal arg later"! *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m 
      in

      (* fold_left2 steps through 2 lists simultaneously *)

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      (* we are assigning the formals from the sast to the appropriate local vals *)
      List.fold_left add_local formals fdecl.slocals
    in


    (* purpose of locals: given a var name, where can we find the value of that var in memory? *)

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    (* EITHER THE FIRST OR SECOND SHOULD ALWAYS WORK. IF NOT THE PROGRAM IS ILL-FORMED AND 
     THE CHECKING SHOULD HAVE CAUGHT THE ISSUE *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in




