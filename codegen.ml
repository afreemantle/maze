module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) = 
    let context = L.global_context () in
    let the_module = L.create_module context "Maze"
    and t_int = L.i32_type context
    and t_bool = L.i1_type context
    (* and t_char = L.char_type context *)
    (* and t_float = L.float_type context *)
    and t_void = L.void_type context 
    (* and t_null = L.null_type contxt in *)


    let ltype_of_type = function
        A.Int
      | A.Bool
      | A.Char
      | A.Float
      | A.Void
      | A.Null
