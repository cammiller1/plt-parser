type token =
  | INT
  | FLOAT
  | BOOL
  | STRING
  | VOID
  | ARRAY
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EXP
  | SEMC
  | PP
  | MM
  | LT
  | GT
  | LTE
  | GTE
  | EQ
  | NE
  | AND
  | OR
  | MOD
  | NOT
  | IN
  | IF
  | ELSE
  | WHILE
  | FOR
  | DEF
  | RETURN
  | LPAREN
  | RPAREN
  | RBRACE
  | LBRACE
  | COMMA
  | ILITERAL of (int)
  | FLITERAL of (float)
  | ID of (string)
  | SLITERAL of (string)
  | BLITERAL of (bool)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
