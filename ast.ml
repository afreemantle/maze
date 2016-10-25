type op = Add | Sub | Mult | Div | Equal | Neq |
	  Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not

type typ = Int | Bool | Void

type bind = typ * string

type expr = Literal of int
	    | Id of string
	    | Binop of expr * op * expr
	    | Assign of string * expr
	    | BoolLit of bool
	    | No expr
	    | Unop of uop *expr
	    | Call of string * expr list

type stmt = Block of stmt list
	    | If of expr * stmt * stmt
	    | While of expr * stmt
	    | Expr of expr
	    | Return of expr

type func_decl = {
	    typ     :  typ;
	    fname   :  string;
	    formals :  bind list;
	    locals  :  bind list;
	    body    :  stmt list;
}

type program = bind list * func_decl list 
