type op = Add | Sub | Mult | Div | Equal | Neq |
	  Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not

type typ = Int | Bool | Char | Float | Void | Null

type datatype = Datatype of typ | Any

type bind = typ * string

type expr = Int_Lit of int
	    | Id of string
	    | Binop of expr * op * expr
	    | Assign of string * expr
	    | Bool_Lit of bool
	    | Float_Lit of float
	    | Char_Lit of char
	    | String_Lit of string
	    | No expr
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
	    name   :  string;
            returnTyp : datatype;
	    formals :  bind list;
	    locals  :  bind list;
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
}    

type program = bind list * class_decl list

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

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_func_decl fdecl = (string_of_type fdecl.returnType) ^ " " ^ (fdecl.name) ^ " " ^
    "\n{\n" ^ 
     String.concat "," (List.map string_of_vdecl fdecl.formals) ^
     String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"  

let string_of_dbody dbody = 
  String.concat ""
