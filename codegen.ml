open Llvm


module L = Llvm
module A = Ast

exception Error of string

let context = global_context ()
let the_module  = create_module context "maze codegen"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 40


let integer_t = integer_type context;;
let bool_t = i1_type context;;
let char_t = i8_type context;;
let float_t = double_type context;; 
let void_t = void_type context;; 
let null_t = const_null context;;  


let rec codegen_expr = function 
    | A.Int n -> ...
    | A.Bool bl -> ...
    | A.Char ch -> ...
    | A.Float fl -> ...
    | A.Void vd -> ...
    | A.Null nl -> ...
