type op = Add | Sub | Mult | Div | Equal | Neq |
	  Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not

type typ = Int | Bool | Char | String |Float | Void | Null

type extends = NoParent | Parent of string

type datatype = Datatype of typ | Arraytype of typ * int | Any

type fname = Constructor | FName of string

type formal = Formal of datatype * string | Many of datatype

type bind = typ * string

type expr = Int_Lit of int
	    | Id of string
	    | Binop of expr * op * expr
	    | Assign of string * expr
	    | Bool_Lit of bool
	    | Float_Lit of float
	    | Char_Lit of char
	    | String_Lit of string
	    | ArrayTyp of expr list
	    | ArrayCreate of datatype * expr list
	    | ArrayAccess of expr * expr
	    | Noexpr
	    | Unop of uop *expr
	    | Call of string * expr list
 	    | Null

type stmt = Block of stmt list
	    | If of expr * stmt * stmt
	    | While of expr * stmt
	    | Expr of expr
	    | Return of expr
            
type vdecl = Field of datatype * string

type func_decl = {
	    fname   :  fname;
            returnType : datatype;
	    formals :  bind list;
	    (*locals  :  bind list;*)
	    body    :  stmt list;
}

type dbody = {
        vdecls : vdecl list;
        constructors : func_decl list;
        methods : func_decl list;
}

type class_decl = {
        dname : string;
        dbody : dbody; 
	extends : extends;
}    

type program = class_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Char -> "char"
  | String -> "string"
  | Float -> "float"
  | Null -> "null"


let string_of_datatype = function
  Arraytype(p, i) -> (string_of_typ p)
| Datatype(p) -> (string_of_typ p)
| Any -> "Any"


let rec string_of_bracket = function
  [] -> ""
| head :: tail -> "[" ^ (string_of_expr head) ^ "]" ^ (string_of_bracket tail)

and string_of_array_typ = function
  [] -> ""
| [last] -> string_of_expr last
| head :: tail -> string_of_expr head ^ ", " ^ string_of_array_typ tail



and string_of_expr = function
    Int_Lit(l) -> string_of_int l
  | Bool_Lit(true) -> "true"
  | Bool_Lit(false) -> "false"
  | Float_Lit(m) -> string_of_float m
  | Char_Lit(c) -> String.make 1 c
  | String_Lit(s) -> s
  | Null -> ""
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | ArrayTyp(e1) -> "|" ^ string_of_array_typ e1 ^ "|"
  | ArrayCreate(d, e1) -> "new " ^ string_of_datatype d
  | ArrayAccess(e, e1) -> string_of_expr e


let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s


let string_of_fname = function
     Constructor -> "constructor"
|    FName(s) -> s


let string_of_vdecl = function
Field(t, id) -> (string_of_datatype t) ^ " " ^ id ^ ";\n"

let string_of_formal = function
Formal(d, s) -> (string_of_datatype d) ^ " " ^ s
| _ -> ""


let string_of_func_decl fdecl = (string_of_datatype fdecl.returnType) ^ " " ^ (string_of_fname fdecl.fname) ^ " " ^
    "\n{\n" ^ 
     String.concat "," (List.map string_of_formal fdecl.formals) ^
     String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"  

let string_of_dbody dbody = 
  String.concat "" (List.map string_of_vdecl dbody.vdecls) ^
  String.concat "" (List.map string_of_func_decl dbody.constructors) ^
  String.concat "" (List.map string_of_func_decl dbody.methods)

let string_of_class decl = 
    "class" ^ decl.dname ^ " {\n" ^ (string_of_dbody decl.dbody) ^" }\n"   

let string_of_program(decls) = String.concat "\n" (List.map string_of_class decls) ^ "" 



