%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE WHILE 
%token INT BOOL CHAR FLOAT VOID NULL
%token CLASS CONSTRUCTOR EXTENDS NEW

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


program: decls EOF { $1 }

/* Classes */

decls:
    class_decl_list   { List.rev $1}

class_decl_list:
    class_decl           { [$1] }
  | class_decl_list class_decl { $2::$1 }

class_decl:
        CLASS ID LBRACE dbody RBRACE { {
            dname = $2;
            extends = NoParent;
            dbody = $4;
        } }
       
       | CLASS ID EXTENDS ID LBRACE dbody RBRACE { {
		dname = $2;
		extends = Parent($4);
		dbody = $6;
       } }
dbody:

        /* nothing here */ { {
            vdecls = [];
            constructors = [];
            methods = [];
        } }

     |   dbody vdecl { {
            vdecls =$2 :: $1.vdecls;
            constructors = $1.constructors;
            methods = $1.methods;
        } }   


     |   dbody constructor { {
            vdecls = $1.vdecls;
            constructors = $2 :: $1.constructors;
            methods = $1.methods;
        } }

     |   dbody fdecl { {
            vdecls = $1.vdecls;
            constructors = $1.constructors;
            methods = $2 :: $1.methods;
        } }

/* Constructors */

constructor:
    CONSTRUCTOR LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE {
        {
            fname = Constructor;
            formals = $3;
            body = List.rev $6;
            returnType = Any
        }
    }



/* Methods */  

fname:
    ID { $1 }
  
fdecl:
     typ fname LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     {
         {
             fname = FName($2);
             returnType = $1;
             formals = $4;
             body = List.rev $7; (*stmt_list; *)
         }
     }


/* Variables */

/* vdecl_list:  nothing  { [] } */
/* | vdecl_list vdecl { $2 :: $1 } */

vdecl: typ ID SEMI { Field($1, $2) }
     
          
/* Formals */

formals_opt:
   /* nothing */ { [] }
 | formal_list  {List.rev $1 }

formal_list:
   typ ID   { [Formal($1, $2)] }
 | formal_list COMMA typ ID { Formal($3, $4) :: $1 }


actuals_opt:
  /* nothing */  { [] }
 | actuals_list { List.rev $1}

actuals_list:
   expr    { [$1] }
 | actuals_list COMMA expr { $3 :: $1 }

 
/* TYPES */ 
 
typ:
   INT   { Datatype(Int) }
 | FLOAT { Datatype(Float) }
 | CHAR  { Datatype(Char) }
 | BOOL  { Datatype(Bool) }
 | VOID  { Datatype(Void) }


/* Expressions */

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
   literals     { $1 }
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
 | NEW ID LPAREN actuals_opt RPAREN { ObjectCreate($2, $4) }

literals: 
  INT_LITERAL     { Int_Lit($1) }
| FLOAT_LITERAL   { Float_Lit($1) }
| CHAR_LITERAL    { Char_Lit($1) }
| STRING_LITERAL  { String_Lit($1) }
| TRUE		  { Bool_Lit(true) }
| FALSE           { Bool_Lit(false) } 
| ID 		  { Id($1) } 
| NULL            { Null } 
