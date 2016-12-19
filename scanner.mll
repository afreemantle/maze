{ open Parser 

  let unescape s = Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x->x)
}

let alpha = ['a' - 'z' 'A' - 'Z']
let digit = ['0' - '9']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let float = (digit+) ['.'] digit+
let int = digit+
let char = ''' ( ascii | digit ) '''
let string = '"' ( (ascii | escape)* as s) '"'
let id = alpha(alpha | digit | '_')*

rule token = parse
    [ ' ' '\t' '\r' '\n' ] {token lexbuf} (* Whitespace *)
| "(*" {comment lexbuf} (*Comments*)

| '(' {LPAREN}
| ')' {RPAREN}
| '{' {LBRACE}
| '}' {RBRACE}
| ';' {SEMI}
| ',' {COMMA}
| '.' {DOT}

| '+' {PLUS}
| '-' {MINUS}
| '*' {TIMES}
| '/' {DIVIDE}
| '=' {ASSIGN}
| "==" {EQ}
| "!=" {NEQ}
| '<' {LT}
| '>' {GT}
| "<=" {LEQ}
| ">=" {GEQ}
| "&&" {AND}
| "||" {OR}
| "!" {NOT}

| "if" {IF}
| "else" {ELSE}
| "while" {WHILE}
| "return" {RETURN}
| "int" {INT}
| "bool" {BOOL}
| "char" {CHAR}
| "float" {FLOAT}
| "string" {STRING}
| "void" {VOID}
| "true" {TRUE}
| "false" {FALSE}
| "null" {NULL}
| "void" {VOID}
| "extends" {EXTENDS}
| "class" {CLASS}
| "new" {NEW}
| "obj" {OBJ}

(*| ['0' - '9']+ as lxm { LITERAL(int_of_string lxm) } *)
(*| ['0' - '9']+['.']['0' - '9']+ as lxm { FLOAT_LITERAL(float_of_string lxm) }
| ['a' - 'z' 'A' - 'Z']['a' - 'z' 'A' - 'Z' '0' - '9' '_']* as lxm { ID(lxm) }
| eof {EOF}
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) } 
*)

| int as lxm  { INT_LITERAL(int_of_string lxm) }
| float as lxm   { FLOAT_LITERAL(float_of_string lxm) }
| char as lxm  { CHAR_LITERAL(String.get lxm 1) }
| string   { STRING_LITERAL(unescape s) }
| id as lxm  { ID(lxm) } 
| eof   { EOF }

and comment = parse
    "*)" {token lexbuf}
| _ {comment lexbuf}


