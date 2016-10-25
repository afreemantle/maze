{ open Parser }

rule token = parse
    [ ' ' '\t' '\r' '\n' ] {token lexbuf} (* Whitespace *)
| "(*" {comment lexbuf} (*Comments*)

| '(' {LPAREN}
| ')' {RPAREN}
| '{' {LBRACE}
| '}' {RBRACE}
| ';' {SEMI}
| ',' {COMMA}

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
| "void" {VOID}
| "true" {TRUE}
| "false" {FALSE}
| "null" {NULL}

| "class" {CLASS}
| "extends" {EXTENDS}
| "new" {NEW}

| ['0' - '9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a' - 'z' 'A' - 'Z']['a' - 'z' 'A' - 'Z' '0' - '9' '_']* as lxm { ID(lxm) }
| eof {EOF}
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "*)" {token lexbuf}
| _ {comment lexbuf}


