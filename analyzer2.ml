
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
      Field(Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in


  let print_dname someClass =
      List.iter (check_not_void (fun n -> "illegal void variable " ^ n)) someClass.dbody.vdecls
      (* List.iter report_duplicate (fun n -> "duplicate variable " ^ n ) *)
  in

  List.iter print_dname classes
