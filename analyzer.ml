
open Ast
open Codegen

module StringMap = Map.Make(String)

let check classes =

  (* -------------------- Verify variable section -------------------- *)

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Check for duplicate class names *)
  report_duplicate (fun n -> "Duplicate class name " ^ n) (List.map (fun n -> n.dname) classes);

  let check_assign lvaluet rvaluet err =
      if lvaluet == rvaluet then lvaluet else raise err
  in

  (* Helper function for check_not_void *)
  let typ_of_datatype = function
      Arraytype(p, i) -> p
    | Datatype(p) -> p
    | Any -> Null               (* <- this will cause problems eventually *)
  in  

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      Field(t, n) -> if typ_of_datatype t == Void then raise (Failure (exceptf n)) else ()
  in

  let check_not_voidf exceptf = function
      Formal(t, n) -> if typ_of_datatype t == Void then raise (Failure (exceptf n)) else ()
  in


  (* Grabs second element of Field (vdecl) *)
  let get_second = function
      Field(t, n) -> n
  in

  let get_second_formal = function
      Formal(t, n) -> n
  in 

  (* Checks that local method variables are neither null nor duplicates *)
  let check_locals_lists someMethod =
      List.iter (check_not_void (fun n -> "illegal void variable " ^ n)) someMethod.locals;
      report_duplicate (fun n -> "duplicate variable " ^ n) (List.map get_second someMethod.locals);
  in

  (* Checks local variables for each method of a class argument *)
  let check_locals_class someClass =
      List.iter check_locals_lists someClass.dbody.methods;
  in

  let check_class_vars someClass =
      List.iter (check_not_void (fun n -> "illegal void variable " ^ n)) someClass.dbody.vdecls;
      report_duplicate (fun n -> "duplicate variable " ^ n) (List.map get_second someClass.dbody.vdecls);
  in

  List.iter check_locals_class classes;
  List.iter check_class_vars classes;


  (* -------------------- Verify function section -------------------- *)
  (*let check_methods_class someClass =*)
  let string_of_fname = function
      FName(s) -> s
    | Constructor -> ""
  in

  let check_for_print funcList =
      if List.mem "print" (List.map (fun f -> string_of_fname f.fname) funcList)
      then raise (Failure ("function print is already defined")) else ();
  in
  
  let m = StringMap.empty in

  let built_in_decls = StringMap.add "print"
     { returnType = Datatype(Void); fname = FName("print"); formals = [Formal(Datatype(String), "")]; locals = []; body = [] } m
  in

  let build_f_decls funcList = 
      List.fold_left (fun m f -> StringMap.add (string_of_fname f.fname) f m) built_in_decls funcList
  in

  let function_decl s someMap = try StringMap.find s someMap
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let print_fname fname f = print_string(fname ^ " " ^ string_of_typ f^"\n") in

  let map_add_vdecl m v =
      match v with
    | Field(t, n) -> StringMap.add n (typ_of_datatype t) m
  in

  let map_add_formal m v =
      match v with
    | Formal(t, n) -> StringMap.add n (typ_of_datatype t) m
  in

  let rec grab_func_locals = function
      [] -> []
    | [x] -> let y = x.locals in y
    | head :: tail -> let r = head.locals in r @ grab_func_locals tail
  in

  let rec grab_func_formals = function
      [] -> []
    | [x] -> let y = x.formals in y
    | head :: tail -> let r = head.formals in r @ grab_func_formals tail
  in

  let check_methods_class someClass =
      let methods = someClass.dbody.methods in
        let function_decls = build_f_decls methods in
        (*StringMap.iter print_fname function_decls; *)
        (*function_decl "tfunc" function_decls; *)
        check_for_print methods;
        report_duplicate (fun n -> "duplicate function " ^ n)
                (List.map (fun f -> string_of_fname f.fname) methods);

        let symbols_classVars = List.fold_left map_add_vdecl StringMap.empty (someClass.dbody.vdecls @ (grab_func_locals methods))
        in

        (* Map of class vars, locals, and formals for fcns in the class *)
        let symbols = List.fold_left map_add_formal symbols_classVars (grab_func_formals methods)
        in

        (*StringMap.iter print_fname symbols;*)

        let type_of_identifier s =
            try StringMap.find s symbols
            with Not_found -> raise (Failure ("undeclared identifier " ^ s))
        in

        let check_func func =

            let rec expr = function
                (*match s with  *)
                Id s -> type_of_identifier s
              | Int_Lit _ -> Int
              | Bool_Lit _ -> Bool
              | Float_Lit _ -> Float
              | Char_Lit _ -> Char
              | String_Lit _ -> String
              | Noexpr -> Void
              | Null -> Null
              | Unop(op, e) as ex -> let t = expr e
                in (match op with
                  Neg when t = Int -> Int
                | Not when t = Bool -> Bool
                | _ -> raise (Failure ("illegal unary operator " ^
                  string_of_uop op ^ string_of_typ t ^ " in " ^
                  string_of_expr ex)))
              | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 
                in (match op with
                  Add | Sub | Mult | Div -> if ((t1 = Int || t1 = Float)
                                        && (t1 = t2))
                        then (t1)
                        else raise (Failure ("operator " ^
                                    string_of_op op ^ " requires " ^
                                    "two ints or two floats")) 
                | Equal | Neq when t1 = t2 -> Bool 
                         (*add error message, test *)
                | Less | Leq | Greater | Geq -> 
                        if ((t1 = Int || t1 = Float) &&
                            (t1 = t2))
                        then (Bool)
                        else raise (Failure ("operator " ^ string_of_op op ^
                                    " requires int or float operands"))

                        (* ^^ Currently doesn't throw this error, FIX *)
                | And | Or when t1 = Bool && t2 = Bool -> Bool
                | _ -> raise (Failure ("illegal binary operator " ^
                      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                      string_of_typ t2 ^ " in " ^ string_of_expr e))
                )
              | Assign(var, e) as ex -> (*let lt = type_of_identifer var
                                        and rt = expr e in*) Void
              | ObjCreate(oname, actuals) -> Void
              | ObjAccess(e1, e2) -> Void
              | Call(fname, actuals) -> Void
              (*| _ -> raise (Failure ("_")) *) 
            in

            let check_bool_expr e = if expr e != Bool
              then raise (Failure ("expected Boolean expression in " ^ string_of_expr e)) else () in

            let rec stmt = function
                Block sl -> let rec check_block = function
                   [Return _ as s] -> stmt s
                 | Return _ :: _ -> raise (Failure "nothing may follow a return")
                 | Block sl :: ss -> check_block (sl @ ss)
                 | s :: ss -> stmt s ; check_block ss
                 | [] -> ()
                in check_block sl 
              | Expr e -> ignore (expr e)
              | Return e -> let t = expr e in if t = (typ_of_datatype func.returnType) then () else
                  raise (Failure ("return gives " ^ string_of_typ t ^
                        " expected " ^ (string_of_datatype func.returnType)
                        ^ " in " ^ string_of_expr e))
              | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
              | While(p, s) -> check_bool_expr p; stmt s
             in

             stmt (Block func.body);
        

            List.iter (check_not_voidf (fun n -> "illegal void formal " ^ n ^ " in " ^ string_of_fname func.fname)) func.formals;
            report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ string_of_fname func.fname) (List.map get_second_formal func.formals) in

        List.iter check_func methods
  in



  List.iter check_methods_class classes;

  (* check for main at some point *)
