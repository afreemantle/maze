open Ast

type sexpr =
	SInt_Lit of int
|	SFloat_Lit of float
|	SChar_Lit of char
|	SBool_Lit of bool
|	SString_Lit of string 
|	SId of string * datatype
|	SBinop of sexpr * op * sexpr * datatype
|	SAssign of sexpr * sexpr * datatype
|	SCall of string * sexpr list * datatype * int
|	SUnop of op * sexpr * datatype
|	SNull 
|	SNoexpr
