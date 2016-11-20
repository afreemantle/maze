
open Ast

module StringMap = Map.Make(String)

let check classes =


  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      Field(t, n) -> print_endline (string_of_datatype t)
      (*Field(Void, n) -> raise (Failure (exceptf n))  *)
    | _ -> ()
  in

  let make_str_list = function
      Field(_, n) -> print_endline(n)
    | _ -> ()
  in

  let check_locals_lists someMethod =
      List.iter (check_not_void (fun n -> "illegal void variable " ^ n)) someMethod.locals
  in

  let print_dname someClass =
      print_endline ("hi");
      (*List.iter (check_not_void (fun n -> "illegal void variable " ^ n)) someClass.dbody.vdecls;*)
      (*List.iter (check_not_void (fun n -> "illegal void variable " ^ n)) someClass.dbody.methods;*)
      List.iter check_locals_lists someClass.dbody.methods;
      List.iter make_str_list someClass.dbody.vdecls
  in
      (* List.iter report_duplicate (fun n -> "duplicate variable " ^ n ) *)

  (*let make_str_list = function
      Field(_, n) -> print_string(n)
    | _ -> ()
  in *)
  

  (*List.iter make_str_list classes;*)
  List.iter print_dname classes
