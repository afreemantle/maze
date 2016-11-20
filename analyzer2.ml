
open Ast

module StringMap = Map.Make(String)

let check classes =

  (*

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  *)

  let print_dname someClass =
      print_string someClass.dname
  in

  List.iter print_dname classes
