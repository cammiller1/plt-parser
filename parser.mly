%{
open Ast

let fst (a,_,_) = a;;
let snd (_,b,_) = b;;
let trd (_,_,c) = c;;

%}


/* Declarations: tokens, precendence, etc. */

/* types */
%token INT FLOAT BOOL STRING VOID ARRAY
/* operators */
%token ASSIGN PLUS MINUS TIMES DIVIDE
%token SEMC
%token LBRACKET RBRACKET
/* comparators */
%token LT GT LTE GTE EQ NE AND OR MOD NOT IN

%token IF ELSE WHILE FOR DEF RETURN
%token LPAREN RPAREN RBRACE LBRACE COMMA

%token <int> ILITERAL
%token <string> FLITERAL
%token <string> ID
%token <string> SLITERAL
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
%left TIMES DIVIDE MOD
%right NOT

%start program  /* the entry point */
%type <Ast.program> program


%%
/* Rules: context-free rules */

program:
  highest EOF { $1 }


highest:
   /* nothing */ { ([], [], [])             }
 | highest vdecl { (($2 :: fst $1), snd $1, trd $1) }
 | highest fdecl { (fst $1, ($2 :: snd $1), trd $1) }
 | highest stmt { (fst $1, snd $1, (trd $1 @ [$2]))  }


/* statement-relevant parsing */
stmt:
    expr SEMC { Expr $1 }
  | RETURN expr_opt SEMC { Return $2 }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7)}
  | FOR LPAREN expr SEMC expr SEMC expr RPAREN stmt { For($3, $5, $7, $9) } 
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
/* =================================== */

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }


vdecl:
     typ ID vdecl_assign SEMC { ($1, $2, $3) } /* declaration with or w.o assignment */

vdecl_assign:
                { Noexpr }
  | ASSIGN expr { $2 }
  | expr  { $1 } 


vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }


fdecl:
  DEF typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  { { typ = $2;
          fname = $3;
          formals = List.rev $5;
          locals = List.rev $8;
          body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list { $1 }

formal_list:
  typ ID { [($1,$2, Noexpr)] }
  | formal_list COMMA typ ID { ($3,$4, Noexpr) :: $1 }


/* type-relevant parsing */
typ: 
    INT              { Int }
  | FLOAT            { Float }
  | BOOL             { Boolean }
  | STRING           { String }
  | VOID             { Void }
  | ARRAY            { Array }
/* =================================== */



/* expression-relevant parsing */
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    ILITERAL           { Liti($1) }
  | FLITERAL           { Litf($1) }
  | BLITERAL           { Litb($1) }
  | SLITERAL           { Lits($1) }
  | ID                 { Id($1) }
  | ID LBRACKET expr RBRACKET { ArrayIndexAccess($1, $3) }
  | LBRACKET typ expr RBRACKET { LitArray($2, $3) }  /* array decl for type and size */
  | expr PLUS   expr   { Binop($1, Add, $3) }
  | expr MINUS  expr   { Binop($1, Sub, $3) }
  | expr TIMES  expr   { Binop($1, Mul, $3) }
  | expr DIVIDE expr   { Binop($1, Div, $3) }
  | expr LT expr       { Binop($1, Lt, $3) }
  | expr GT expr       { Binop($1, Gt, $3) }
  | expr MOD expr      { Binop($1, Mod, $3) }
  | expr LTE expr      { Binop($1, Lte, $3) }
  | expr GTE expr      { Binop($1, Gte, $3) }
  | expr EQ expr       { Binop($1, Eq, $3) }
  | expr NE expr       { Binop($1, Ne, $3) }
  | expr AND expr      { Binop($1, And, $3) }
  | expr OR expr       { Binop($1, Or, $3) }
  | ID ASSIGN expr     { Assign($1, $3) }
  | ID LBRACKET expr RBRACKET ASSIGN expr { ArrayIndexAssign($1, $3, $6) }  /* array index assign */
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }  /* function call */
  | LPAREN expr RPAREN { $2 }
  | NOT expr           { Uniop(Not, $2) }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

/* =================================== */