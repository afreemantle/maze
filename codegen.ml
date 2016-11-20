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

    let printf_t = 
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = 
        L.declare_function "print" printf_t the_module in

    

