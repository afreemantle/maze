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
	    typ     :  typ;
	    fname   :  string;
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
