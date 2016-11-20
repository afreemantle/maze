(* Codegen for MAZE - based off of MicroC *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) = 
    let context = L.global_context () in 
    let the_module = L.create_module context "maze"

    and i32_t = L.i32_type context      (* int *)
    and i8_t = L.i8_type context        (* printf *)
    and i1_t = L.i1_type context        (* bool *)
    and void_t = L.void_type context in (* void *)
    (* add our other types here *)

    let ltype_of_type = function 
        A.Int -> i32_t
      | A.Bool -> i1_t
      | A.Void -> void_t in
      (* add our other types here *)

    (* This is where global var func would go *)
    
    (* Lets make a function that does some of this boilerplate stuff
     * below so that we dont have to keep writing it out over and over
     * again for each function *)
    let printf_t = 
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = 
        L.declare_function "print" printf_t the_module in

    (* Define functions *) 
    
    let function_decls = 
        let function_decl m fdecl =
            let name = fdecl.A.fname
            and formal_types = Array.of_list
                (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
            in let ftype = 
                L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
            StringMap.add name (L.define_function name ftype the_module,
                                fdecl) m in
        List.fold_left function_decl StringMap.empty functions in

    (* Here we fill in the body of each of the functions *)

    let build_function_body fdecl = 

        let (the_function, _) = 
            StringMap.find fdecl.A.fname function_decls in

        let builder = 
            L.builder_at_end context (L.entry_block the_function) in

        (* this will only print ints right now *)
        let int_format_str =
            L.build_global_stringptr "%d\n" "fmt" builder in

        (* For the cool TADS like feature I am going to 
         * need to do string formatting *)

    (* local variables - Print takes an argument,need this for hello world*)
    let local_vars = 
        let add_formal m (t, n) p = L.set_value_name n p;
          let local = L.build_alloca (ltype_of_typ t) n builder in 
          ignore (L.build_store p local builder);
          StringMap.add n local m in

        let add_local m (t, n) = 
            let local_var = L.build_alloca (ltype_of_typ t) n builder
            in StringMap.add n local_var m in

        let formals = List.fold_left2 add_formal StringMap.empty
            fdecl.A.formals (Array.to_list (L.params the_function)) in
        List.fold_left add_local formals fdecl.A.locals in
    
    (* in MicroC, this lookup funtion finds the value for a variable *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Generate code for an expression *)

    let rec expr builder = function 
      (*  A.Int_Lit i -> L.const_int i32_t i
      | A.Bool_Lit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Assign (s, e) -> let e' = expr builder e in
                ignore (L.build_store e' (lookup s) builder); e'*)
      | A.Call ("print", [e]) ->
              L.build_call printf_func
                 [| int_format_str ; (expr builder e) |]
                 "print" builder
      

