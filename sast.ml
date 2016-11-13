open Ast

type sexpr = 
	SId of string * datatype
|	SBinop of sexpr * op * sexpr * datatype
|	SAssign of sexpr * sexpr * datatype
|	SCall of string * sexpr list * datatype * int
|	SUnop of op * sexpr * datatype
