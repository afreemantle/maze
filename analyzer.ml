open Ast 
open Sast


module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

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


let update_env_name env env_name = {
	env_class_maps = env.env_class_maps;
	env_name = env_name;
	env_cmap = env.env_cmap;
	env_locals = env.env_locals;
	env_parameters = env.env_parameters;
	env_returnType = env.env_returnType;
	env_in_while = env.env_in_while;
	env_reserved = env.env_reserved;
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
	   let vdeclfun = (fun m -> (function Field(s, d, n) -> if(StringMap.mem (n) m) then raise(Exceptions.DuplicateField) else (StringMap.add n (Field(s, d, n)) m))) in
	let funcname = get_name cdecl.cname in
	let funcfun m fdecl = 
		if (StringMap.mem (funcname fdecl) m) then raise(Excpetions.DuplicateFunction(funcname fdecl)) else if (StringMap.mem (Ast.string_of_fname fdecl.fname) reserved_map) then raise(Exceptions.CannotUseReservedFuncName( Ast.string_of_fname fdecl.fname)) else (StringMap.add (funcname fdecl) fdecl m) in
List.fold_left helper StringMap.empty decls
*)





let get_arithmetic_binop_type se1 se2 op = function
	(Datatype(Int), Datatype(Float))
|	(Datatype(Float), Datatype(Int))
|	(Datatype(Int), Datatype(Char))
|	(Datatype(Char), Datatype(Int))
|	_ -> raise (Failure ("Arithmetic operators don't support these types"))


let get_equality_binop_type type1 type2 se1 se2 op =
	if (type1 = Datatype(Float) || type2 = Datatype(Float)) then raise ( Failure ("get equality binop"))
	else
	match type1, type2 with
		Datatype(Char), Datatype(Int)
	| 	Datatype(Int), Datatype(Char)
	|	_ -> if type1 = type2 then SBinop(se1, op, se2, Datatype(Bool))
		     else raise ( Failure ("get equality binop 2"))

let get_logical_binop_type se1 se2 op = function
	(Datatype(Bool), Datatype(Bool)) -> SBinop(se1, op, se2, Datatype(Bool))
	| _ -> raise (Failure ("get logical binop type"))

let get_comparison_binop_type type1 type2 se1 se2 op =
	let numerics = SS.of_list [Datatype(Int); Datatype(Char); Datatype(Float)]
	in
		if SS.mem type1 numerics && SS.mem type2 numerics
		then SBinop(se1, op, se2, Datatype(Bool))
		else raise(Failure ("get comparison binop"))

let rec get_ID_type env s =
	try StringMap.find s env.env_locals
	with | Not_found ->
	try let formal = StringMap.find s env.env_parameters in 
	   (function Formal(t, _) -> t | Many t -> t ) formal 
	with | Not_found -> raise (Failure ("get ID type failure"))


and check_assign env e1 e2 = 
	let se1, env = expr_sexpr env e1 in
	let se2, env = expr_sexpr env e2 in 
	let type1 = type_expr se1 in
	let type2 = type_expr se2 in
	match (type1, se2) with
		_ ->
	match type1, type2 with
		Datatype(Char), Datatype(Int)
	|	Datatype(Int), Datatype(Char) -> SAssign(se1, se2, type1)
	| _ ->
	if type1 = type2 then SAssign(se1, se2, type1) else raise (Failure (" check_assign env e1 e2")) 


and check_unop env op e =
	let check_num_unop t = function
		Sub  -> t
	|	_  ->  raise (Failure ( "check_unop env"))
	in
	let check_bool_unop = function
		Not  ->  Datatype(Bool)
	|	_   ->  raise (Failure ("check_bool_unop"))
	in
	
	let se, env = expr_sexpr env e in
	let t = type_expr se in
	match t with 
		Datatype(Int)
	|	Datatype(Float)  -> SUnop(op, se, check_num_unop t op)
	|	Datatype(Bool)   -> SUnop(op, se, check_bool_unop op)
	|	_  -> raise( Failure ("check_unop_match"))


and check_binop env e1 op e2 = 
	let se1, env = expr_sexpr env e1 in
	let se2, env = expr_sexpr env e2 in
	let type1 =  type_expr se1 in
	let type2 = type_expr se2 in
	match op with
	Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
	| And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
	| Less | Leq | Greater | Geq -> get_comparison_binop_type type1 type2 se1 se2 op
	| Add | Sub | Mult | Div -> get_arithmetic_binop_type se1 se2 op (type1, type2)
	| _ -> raise (Failure (" check_binop env"))


and expr_sexpr env = function
	Int_Lit i   ->  SInt_Lit(i), env
|	Bool_Lit b  ->  SBool_Lit(b), env
|	Float_Lit f  ->  SFloat_Lit(f), env
|	Char_Lit c   -> SChar_Lit(c), env
|	Id s   ->   SId(s, get_ID_type env s), env
|	Null   ->   SNull, env
| 	Noexpr  ->   SNoexpr, env
| 	Call(s, el)  ->  check_call_type env false env s el, env
| 	Assign(e1, e2)  ->  check_assign env e1 e2, env
|	Unop(op, e)  ->  check_unop env op e, env
|	Binop(e1, op, e2)   ->  check_binop env e1 op e2, env 


and type_expr = function
	SInt_Lit(_)  ->  Datatype(Int)
|	SBool_Lit(_) ->  Datatype(Bool)
|	SFloat_Lit(_) -> Datatype(Float)
(*| 	String_Lit(_)  ->  ArrayType(Char, 1) *)
|	SChar_Lit(_)  ->   Datatype(Char) 
|	SId(_,d)  -> d
|	SBinop(_,_,_,d)  -> d
|	SAssign(_,_,d)   -> d
|	SNoexpr		-> Datatype(Void)
|	SUnop(_,_,d)     -> d
|	SCall(_,_,d,_)   -> d
| 	SNull		-> Datatype(Null)


and exprl_to_sexprl env el = 
	let env_ref = ref(env) in
	let rec helper = function
		head :: tail ->
		let a_head, env = expr_env !env_ref head in
		env_ref := env;
		a_head::(helper tail)
	| [] -> []
	in (helper el), !env_ref

