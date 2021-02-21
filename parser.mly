%{ open Ast %}

%token SEQ
%token ASSI PLUS MINUS TIMES DIVIDE EOF
%token <int> LITERAL
%token <string> VARIABLE

%left SEQ
%right ASSI
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
  expr PLUS   expr   { Binop($1, Add, $3) }
| expr MINUS  expr   { Binop($1, Sub, $3) }
| expr TIMES  expr   { Binop($1, Mul, $3) }
| expr DIVIDE expr   { Binop($1, Div, $3) }
| LITERAL            { Lit($1) }
| VARIABLE           { Var($1) }
| expr SEQ expr      { Seq($1, $3) }
| VARIABLE ASSI expr { Assi($1, $3) }

