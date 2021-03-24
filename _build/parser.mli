type token =
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | LT
  | GT
  | LTE
  | GTE
  | EQ
  | NE
  | AND
  | OR
  | MOD
  | EXP
  | SEMC
  | PP
  | MM
  | IF
  | ELSE
  | ELIF
  | WHILE
  | FOR
  | DEF
  | RETURN
  | LPAREN
  | RPAREN
  | RBRACE
  | LBRACE
  | COMMA
  | INT
  | FLOAT
  | BOOL
  | STRING
  | VOID
  | ILITERAL of (int)
  | FLITERAL of (float)
  | ID of (string)
  | SLITERAL of (string)
  | BLITERAL of (bool)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
