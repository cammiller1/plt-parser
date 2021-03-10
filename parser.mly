%{ open Ast %}  
//  Exp | Mod | Lt | Gt | Lte | Gte | Eq | Ne | And | Or

/* Declarations: tokens, precendence, etc. */

%token ASSI PLUS MINUS TIMES DIVIDE EOF LT GT LTE GTE EQ NE AND OR MOD EXP SEMC PP MM 
%token IF ELSE ELIF WHILE
%token LPAREN RPAREN RBRACE LBRACE 
%token <int> ILITERAL
%token <float> FLITERAL
%token <string> VARIABLE
%token <bool> BOOL

/* lowest to highest precedence */
%left SEQ
%right ASSI
%left LT GT LTE GTE EQ NE AND OR
%left PLUS MINUS
%left TIMES DIVIDE MOD EXP
%left PP MM

%start expr  /* the entry point */
%type <Ast.expr> expr

%%
/* Rules: context-free rules */

expr:
  expr PLUS   expr   { Binop($1, Add, $3) }
| expr MINUS  expr   { Binop($1, Sub, $3) }
| expr TIMES  expr   { Binop($1, Mul, $3) }
| expr DIVIDE expr   { Binop($1, Div, $3) }
| ILITERAL           { Liti($1) }
| FLITERAL           { Litf($1) }
| BOOL               { Bool($1) }
| VARIABLE           { Var($1) }
// | expr SEMC expr     { Seq($1, $3) }
| VARIABLE ASSI expr { Assi($1, $3) }
| expr LT expr       { Binop($1, Lt, $3) }
| expr GT expr       { Binop($1, Gt, $3) }
| expr EXP expr      { Binop($1, Exp, $3) }
| expr MOD expr      { Binop($1, Mod, $3) }
| expr LTE expr      { Binop($1, Lte, $3) }
| expr GTE expr      { Binop($1, Gte, $3) }
| expr EQ expr       { Binop($1, Eq, $3) }
| expr NE expr       { Binop($1, Ne, $3) }
| expr AND expr      { Binop($1, And, $3) }
| expr OR expr       { Binop($1, Or, $3) }
| expr PP            { Uniop($1, Pp) }
| expr MM            { Uniop($1, Mm) }
| LPAREN expr RPAREN { $2 }
| 

stmt://
  IF LPAREN expr RPAREN LBRACE stmt RBRACE ELSE LBRACE stmt RBRACE
|// IF LPAREN expr RPAREN  {error} //
| RBRACE ELSE LBRACE stmt RBRACE// to be implemented
| LBRACE stmt RBRACE   { ... }//to be implemented
| //FOR...to be implemented
| //WHILEto be implemented

 //types



