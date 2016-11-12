open Ast 

module StringMap = Map.Make(String)
module StringSet = Map.Make(String)

let struct_indexes:(string, int) Hashtbl.t = Hashtbl.create 10
let predescessors:(string, string list) Hashtbl.t = Hashtbl.create 10

module SS = Set.Make(
struct
let compare = Pervasives.compare
type t = datatype
end )

type class_map = {
	vdecl_map : Ast.vdecl StringMap.t;
	func_map  : Ast.func_decl StringMap.t;
	constructor_map  :  Ast.func_decl StringMap.t;
	reserved_map  :  func_decl StringMap.t;
	decl  :  Ast.class_decl;
}

type env = {
	env_class_maps  :  class_map StringMap.t;
	env_name   :   string;
	env_cmap   :  class_map;
	env_locals   :   datatype StringMap.t;
	env_parameters  : Ast.formal StringMap.t;
	env_returnType  :  datatype;
	env_in_while   :  bool;
	env_reserved   :  func_decl list;

}

let get_name cname fdecl = 
	let name = Ast.string_of_fname fdecl.fname in
	if name = "main"
		then "main"
		else cname ^ "." ^ name
(*
let build_class_maps reserved cdecls = 
	let reserved_map = List.fold_left (fun m f -> StringMap.add (Ast.string_of_fname f.sfname) f m) StrinMap.empty reserved in
	let helper m (cdecl:Ast.class_decl) = 
	   let vdeclfun = (fun m -> (function Field(s, d, n) -> if(StringMap.mem (n) m) then raise(Exceptions.DuplicatField) else (StringMap.add n (Field(s, d, n)) m))) in
	let funcname = get_name cdecl.cname in
	let funcfun m fdecl = 
		if (StringMap.mem (funcname fdecl) m) then raise(Excpetions.DuplicateFunction(funcname fdecl)) else if (StringMap.mem (Ast.string_of_fname fdecl.fname) reserved_map) then raise(Exceptions.CannotUseReservedFuncName( Ast.string_of_fname fdecl.fname)) else (StringMap.add (funcname fdecl) fdecl m) 
(*List.fold_left helper StringMap.empty decls*)
*)


