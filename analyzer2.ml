
open Ast

module StringMap = Map.Make(String)

let check classes =

  (* ------------------------- Verify variable section ------------------------- *)

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Helper function for check_not_void *)
  let typ_of_datatype = function
      Arraytype(p, i) -> p
    | Datatype(p) -> p
    | Any -> Null               (* <- this will cause problems eventually *)
  in  

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (*Field(Void, n) -> raise (Failure (exceptf n))  
    | Field(t, n) -> print_endline (string_of_datatype t)*)
    | Field(t, n) -> if typ_of_datatype t == Void then raise (Failure (exceptf n)) else (print_endline n)
  in

  (* Grabs second element of Field (vdecl) *)
  let get_second = function
      Field(t, n) -> n
  in 

  (* Checks that local method variables are neither null nor duplicates *)
  let check_locals_lists someMethod =
      List.iter (check_not_void (fun n -> "illegal void variable " ^ n)) someMethod.locals;
      report_duplicate (fun n -> "duplicate variable " ^ n) (List.map get_second someMethod.locals);
  in

  (* Checks local variables for each method of a class argument *)
  let print_dname someClass =
      List.iter check_locals_lists someClass.dbody.methods;
  in

  List.iter print_dname classes;


  (* ------------------------- Verify function section ------------------------- *)


