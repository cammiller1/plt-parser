%{ open Ast %}

/* Declarations: tokens, precendence, etc. */

%token ASSIGN PLUS MINUS TIMES DIVIDE LT GT LTE GTE EQ NE AND OR MOD  
%token EXP SEMC PP MM
%token IF ELSE ELIF WHILE FOR DEF RETURN
%token LPAREN RPAREN RBRACE LBRACE COMMA
%token INT FLOAT BOOL STRING VOID /*ARRAY OF*/
%token <int> ILITERAL
%token <float> FLITERAL
%token <string> ID
%token <bool> BLITERAL
%token EOF

/* lowest to highest precedence */
%nonassoc NOELSE
%nonassoc ELSE
%right UMINUS
%right ASSIGN
%left AND OR
%left EQ NE
%left LT GT LTE GTE 
%left PLUS MINUS
%left TIMES DIVIDE MOD EXP
%left PP MM

%start program  /* the entry point */
%type <Ast.program> program


%%
/* Rules: context-free rules */

program:
  stmt_list EOF { List.rev $1 }

stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 } 

stmt:
    expr SEMC { Expr $1 }
  | RETURN expr_opt SEMC { Return $2 }
  | IF LPAREN expr RPAREN LBRACE stmt RBRACE %prec NOELSE { If($3, $6, Block([]))}
  | IF LPAREN expr RPAREN LBRACE stmt RBRACE ELSE LBRACE stmt RBRACE { If($3, $6, $10)}
  | FOR LPAREN expr SEMC expr SEMC expr RPAREN LBRACE stmt RBRACE { For($3, $5, $7, $10) } 
  | WHILE LPAREN expr RPAREN LBRACE stmt RBRACE { While($3, $6) } 
  | fdecl              { FDefine($1) }  /* function declaration */

expr:
    ILITERAL           { Liti($1) }
  | FLITERAL           { Litf($1) }
  | BLITERAL           { Litb($1) }
  | ID                 { Id($1) } 
  | expr PLUS   expr   { Binop($1, Add, $3) }
  | expr MINUS  expr   { Binop($1, Sub, $3) }
  | expr TIMES  expr   { Binop($1, Mul, $3) }
  | expr DIVIDE expr   { Binop($1, Div, $3) }
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
  | MINUS expr %prec UMINUS { Uniop(Neg, $2) }
  | ID ASSIGN expr { Assign($1, $3) }  /* replaces vdecl */
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }  /* function call */
  | LPAREN expr RPAREN { $2 }

typ: 
    INT              { Int }
  | FLOAT            { Float }
  | BOOL             { Boolean }
  | STRING           { String }
  | VOID             { Void  }
  /*/  | ARRAY OF T = typ { TypArray t }
*/

fdecl:
  DEF typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  { { typ = $2;
  fname = $3;
  formals = List.rev $5;
  body = List.rev $8 } } 

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }


formals_opt:
    /* nothing */ { [] }
  | formal_list { $1 } 

formal_list:
  typ ID { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }



/* add function call logic 
put the function with its argument into a table
- put locals back into fdecl file
user-defined function calls and builtins treated the same
check if function call is valid (built in, previously declare)
built-in functions should be stored in sybmol table
also need table for variable (think like hw 1)

symbol table stuff done in codegen file

possible things to add: arrays, slicing
ex: arr[1:2]
ex: arr[1:4:2]
*/