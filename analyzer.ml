
open Ast

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

  let print_fname fname f = print_string(fname ^ " . \n") in

  let map_add_vdecl m v =
      match v with
    | Field(t, n) -> StringMap.add n t m
  in

  let rec grab_func_locals = function
      [] -> []
    | [x] -> let y = x.locals in y
    | head :: tail -> let r = head.locals in r @ grab_func_locals tail
  in

  let check_methods_class someClass =
      let methods = someClass.dbody.methods in
        let function_decls = build_f_decls methods in
        (*StringMap.iter print_fname function_decls; *)
        (*function_decl "tfunc" function_decls; *)
        check_for_print methods;
        report_duplicate (fun n -> "duplicate function " ^ n)
                (List.map (fun f -> string_of_fname f.fname) methods);

        let symbols_classVars = List.fold_left map_add_vdecl StringMap.empty someClass.dbody.vdecls
        in

        (*let symbols = List.fold_left map_add_vdecl symbols_classVars *)

        let check_function func = 
            List.iter (check_not_voidf (fun n -> "illegal void formal " ^ n ^ " in " ^ string_of_fname func.fname)) func.formals;
            report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ string_of_fname func.fname) (List.map get_second_formal func.formals) in

        List.iter check_function methods
  in



  List.iter check_methods_class classes;

  (* check for main at some point *)
