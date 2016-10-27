%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE WHILE 
%token INT BOOL CHAR FLOAT VOID NULL
%token CLASS EXTENDS NEW

%token <int> INT_LITERAL
%token <char> CHAR_LITERAL
%token <float> FLOAT_LITERAL
%token <string> ID
%token <string> STRING_LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%


stmt_list:
   /* nothing */{ [] }
  | stmt_list stmt  { $2 :: $1 }

stmt:
   expr SEMI     { Expr $1 }
 | RETURN SEMI   { Return Noexpr }
 | RETURN expr SEMI  { Return $2 }
 | LBRACE stmt_list RBRACE   { Block(List.rev $2) }
 | IF LPAREN expr RPAREN stmt %prec NOELSE   { If($3, $5, Block([])) }
 | IF LPAREN expr RPAREN stmt ELSE stmt   { If($3, $5, $7) }
 | WHILE LPAREN expr RPAREN stmt  { While($3, $5) }


expr:
   LITERAL     { Literal($1) }
 | TRUE        { BoolLit(true) }
 | FALSE       { BoolLit(false) }
 | ID          { Id($1) }
 | expr PLUS expr  { Binop($1, Add, $3) }
 | expr MINUS expr { Binop($1, Sub, $3) }
 | expr TIMES expr { Binop($1, Mult, $3) }
 | expr DIVIDE expr { Binop($1, Div, $3) }
 | expr EQ expr  { Binop($1, Equal, $3) }
 | expr NEQ expr { Binop($1, Neq, $3) }
 | expr LT expr { Binop($1, Less, $3) }
 | expr GT expr { Binop($1, Greater, $3) }
 | expr LEQ expr { Binop($1, Leq, $3) }
 | expr GEQ expr { Binop($1, Geq, $3) }
 | expr AND expr { Binop($1, And, $3) }
 | expr OR expr { Binop($1, Or, $3) }
 | MINUS expr %prec NEG { Unop(Neg, $2) }
 | NOT expr  { Unop(Not, $2) }
 | ID ASSIGN expr  { Assign($1, $3) }
 | LPAREN expr RPAREN  { $2 }
 | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
