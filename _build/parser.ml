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
  | LSBRAC
  | RSBRAC
  | ILITERAL of (int)
  | FLITERAL of (string)
  | ID of (string)
  | SLITERAL of (string)
  | BLITERAL of (bool)
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast

let fst (a,_,_) = a;;
let snd (_,b,_) = b;;
let trd (_,_,c) = c;;

# 58 "parser.ml"
let yytransl_const = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* BOOL *);
  260 (* STRING *);
  261 (* VOID *);
  262 (* ARRAY *);
  263 (* ASSIGN *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIVIDE *);
  268 (* EXP *);
  269 (* SEMC *);
  270 (* PP *);
  271 (* MM *);
  272 (* LT *);
  273 (* GT *);
  274 (* LTE *);
  275 (* GTE *);
  276 (* EQ *);
  277 (* NE *);
  278 (* AND *);
  279 (* OR *);
  280 (* MOD *);
  281 (* NOT *);
  282 (* IN *);
  283 (* IF *);
  284 (* ELSE *);
  285 (* WHILE *);
  286 (* FOR *);
  287 (* DEF *);
  288 (* RETURN *);
  289 (* LPAREN *);
  290 (* RPAREN *);
  291 (* RBRACE *);
  292 (* LBRACE *);
  293 (* COMMA *);
  294 (* LSBRAC *);
  295 (* RSBRAC *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  296 (* ILITERAL *);
  297 (* FLITERAL *);
  298 (* ID *);
  299 (* SLITERAL *);
  300 (* BLITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\008\000\008\000\003\000\010\000\
\010\000\011\000\011\000\004\000\012\000\012\000\013\000\013\000\
\009\000\009\000\009\000\009\000\009\000\009\000\007\000\007\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\014\000\
\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\000\000\002\000\004\000\000\000\
\002\000\000\000\002\000\010\000\000\000\001\000\002\000\004\000\
\001\000\001\000\001\000\001\000\001\000\004\000\000\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\003\000\002\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\060\000\000\000\025\000\026\000\027\000\028\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\033\000\034\000\000\000\036\000\035\000\001\000\003\000\
\004\000\005\000\000\000\000\000\055\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\054\000\008\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\041\000\
\044\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\053\000\000\000\030\000\000\000\015\000\000\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\000\000\000\000\010\000\000\000\018\000\000\000\000\000\000\000\
\024\000\011\000\019\000\000\000\020\000"

let yydgoto = "\002\000\
\003\000\004\000\024\000\025\000\064\000\027\000\035\000\037\000\
\028\000\085\000\112\000\099\000\100\000\067\000\068\000"

let yysindex = "\004\000\
\000\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\000\000\230\255\242\254\003\255\004\255\030\255\230\255\230\255\
\000\000\000\000\000\000\252\254\000\000\000\000\000\000\000\000\
\000\000\000\000\201\000\235\254\000\000\230\255\230\255\230\255\
\240\254\252\000\029\255\178\255\104\000\230\255\230\255\230\255\
\230\255\230\255\230\255\230\255\000\000\230\255\230\255\230\255\
\230\255\230\255\230\255\230\255\230\255\230\255\013\255\057\255\
\197\255\216\255\218\000\010\255\000\000\000\000\000\000\000\000\
\252\000\252\000\031\255\043\255\011\000\011\000\000\000\000\000\
\000\000\167\255\167\255\167\255\167\255\030\001\030\001\013\001\
\013\001\000\000\054\255\230\255\082\255\164\000\164\000\230\255\
\030\255\000\000\230\255\000\000\252\000\000\000\080\255\000\000\
\235\000\036\255\075\255\073\255\252\000\164\000\230\255\000\000\
\083\255\030\255\000\000\082\000\000\000\037\255\164\000\030\255\
\000\000\000\000\000\000\124\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\105\255\000\000\
\000\000\000\000\000\000\104\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\116\255\000\000\000\000\000\000\000\000\096\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\118\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\255\067\255\000\000\098\255\126\255\148\255\000\000\000\000\
\000\000\247\254\050\255\046\000\101\000\068\255\231\255\249\254\
\063\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\099\255\000\000\000\000\000\000\123\255\000\000\001\000\000\000\
\000\000\000\000\000\000\103\255\069\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\144\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\028\000\000\000\252\255\008\000\000\000\038\000\
\244\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 566
let yytable = "\026\000\
\009\000\033\000\038\000\042\000\001\000\050\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\050\000\050\000\
\055\000\029\000\030\000\052\000\056\000\055\000\034\000\036\000\
\042\000\060\000\050\000\042\000\039\000\050\000\005\000\006\000\
\007\000\008\000\009\000\031\000\032\000\057\000\058\000\059\000\
\052\000\061\000\089\000\052\000\023\000\065\000\066\000\069\000\
\070\000\071\000\072\000\073\000\083\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\043\000\084\000\
\090\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\055\000\055\000\051\000\098\000\104\000\113\000\091\000\
\048\000\095\000\096\000\043\000\051\000\051\000\043\000\048\000\
\048\000\048\000\048\000\093\000\092\000\110\000\094\000\097\000\
\051\000\107\000\101\000\051\000\058\000\048\000\059\000\058\000\
\048\000\059\000\114\000\102\000\105\000\106\000\108\000\037\000\
\037\000\037\000\037\000\037\000\037\000\031\000\109\000\037\000\
\037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\032\000\056\000\016\000\057\000\021\000\038\000\038\000\017\000\
\022\000\037\000\038\000\115\000\037\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\038\000\116\000\000\000\000\000\
\000\000\000\000\000\000\039\000\039\000\000\000\000\000\038\000\
\039\000\000\000\038\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\000\000\000\000\000\000\040\000\041\000\
\042\000\043\000\044\000\000\000\000\000\039\000\000\000\000\000\
\039\000\040\000\041\000\042\000\043\000\044\000\054\000\000\000\
\000\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\000\000\000\000\040\000\041\000\042\000\043\000\
\044\000\000\000\000\000\062\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\000\000\000\000\040\000\
\041\000\042\000\043\000\044\000\000\000\000\000\086\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\000\000\000\000\000\000\049\000\000\000\000\000\000\000\000\000\
\000\000\087\000\049\000\049\000\049\000\049\000\010\000\000\000\
\000\000\009\000\009\000\009\000\009\000\009\000\016\000\000\000\
\049\000\000\000\000\000\049\000\000\000\018\000\019\000\020\000\
\021\000\022\000\000\000\000\000\042\000\043\000\044\000\000\000\
\000\000\009\000\000\000\009\000\000\000\009\000\009\000\009\000\
\009\000\009\000\054\000\009\000\009\000\000\000\000\000\000\000\
\009\000\009\000\009\000\009\000\009\000\005\000\006\000\007\000\
\008\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000\000\000\000\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\046\000\010\000\000\000\011\000\
\000\000\012\000\013\000\014\000\015\000\016\000\000\000\046\000\
\017\000\000\000\046\000\000\000\018\000\019\000\020\000\021\000\
\022\000\040\000\041\000\042\000\043\000\044\000\000\000\000\000\
\000\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\047\000\000\000\111\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\000\000\000\000\000\000\000\000\
\010\000\000\000\011\000\000\000\012\000\013\000\047\000\015\000\
\016\000\047\000\063\000\017\000\000\000\000\000\000\000\018\000\
\019\000\020\000\021\000\022\000\010\000\000\000\011\000\000\000\
\012\000\013\000\000\000\015\000\016\000\000\000\117\000\017\000\
\000\000\000\000\000\000\018\000\019\000\020\000\021\000\022\000\
\013\000\000\000\013\000\000\000\013\000\013\000\000\000\013\000\
\013\000\000\000\013\000\013\000\000\000\000\000\000\000\013\000\
\013\000\013\000\013\000\013\000\010\000\000\000\011\000\000\000\
\012\000\013\000\000\000\015\000\016\000\000\000\000\000\017\000\
\000\000\000\000\000\000\018\000\019\000\020\000\021\000\022\000\
\040\000\041\000\042\000\043\000\044\000\045\000\000\000\000\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\040\000\041\000\042\000\043\000\044\000\088\000\000\000\
\000\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\040\000\041\000\042\000\043\000\044\000\103\000\
\000\000\000\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\040\000\041\000\042\000\043\000\044\000\
\000\000\000\000\000\000\046\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\040\000\041\000\042\000\043\000\
\044\000\000\000\000\000\000\000\046\000\047\000\048\000\049\000\
\050\000\051\000\000\000\000\000\054\000\040\000\041\000\042\000\
\043\000\044\000\000\000\000\000\000\000\046\000\047\000\048\000\
\049\000\000\000\000\000\000\000\000\000\054\000"

let yycheck = "\004\000\
\000\000\014\000\007\001\013\001\001\000\013\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\022\001\023\001\
\038\001\010\000\033\001\013\001\042\001\038\001\015\000\016\000\
\034\001\042\001\034\001\037\001\033\001\037\001\001\001\002\001\
\003\001\004\001\005\001\033\001\033\001\030\000\031\000\032\000\
\034\001\013\001\033\001\037\001\000\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\040\001\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\013\001\007\001\
\034\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\038\001\038\001\013\001\089\000\042\001\042\001\037\001\
\013\001\086\000\087\000\034\001\022\001\023\001\037\001\020\001\
\021\001\022\001\023\001\084\000\039\001\106\000\013\001\088\000\
\034\001\102\000\091\000\037\001\034\001\034\001\034\001\037\001\
\037\001\037\001\111\000\028\001\034\001\037\001\103\000\008\001\
\009\001\010\001\011\001\012\001\013\001\013\001\036\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\013\001\034\001\013\001\034\001\034\001\008\001\009\001\013\001\
\034\001\034\001\013\001\112\000\037\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\112\000\255\255\255\255\
\255\255\255\255\255\255\008\001\009\001\255\255\255\255\034\001\
\013\001\255\255\037\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\255\255\255\255\008\001\009\001\
\010\001\011\001\012\001\255\255\255\255\034\001\255\255\255\255\
\037\001\008\001\009\001\010\001\011\001\012\001\024\001\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\255\255\255\255\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\034\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\255\255\255\255\008\001\
\009\001\010\001\011\001\012\001\255\255\255\255\034\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\255\255\255\255\255\255\013\001\255\255\255\255\255\255\255\255\
\255\255\034\001\020\001\021\001\022\001\023\001\025\001\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\033\001\255\255\
\034\001\255\255\255\255\037\001\255\255\040\001\041\001\042\001\
\043\001\044\001\255\255\255\255\010\001\011\001\012\001\255\255\
\255\255\025\001\255\255\027\001\255\255\029\001\030\001\031\001\
\032\001\033\001\024\001\035\001\036\001\255\255\255\255\255\255\
\040\001\041\001\042\001\043\001\044\001\001\001\002\001\003\001\
\004\001\005\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\013\001\255\255\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\025\001\255\255\027\001\
\255\255\029\001\030\001\031\001\032\001\033\001\255\255\034\001\
\036\001\255\255\037\001\255\255\040\001\041\001\042\001\043\001\
\044\001\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\013\001\255\255\034\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\255\255\255\255\255\255\255\255\
\025\001\255\255\027\001\255\255\029\001\030\001\034\001\032\001\
\033\001\037\001\035\001\036\001\255\255\255\255\255\255\040\001\
\041\001\042\001\043\001\044\001\025\001\255\255\027\001\255\255\
\029\001\030\001\255\255\032\001\033\001\255\255\035\001\036\001\
\255\255\255\255\255\255\040\001\041\001\042\001\043\001\044\001\
\025\001\255\255\027\001\255\255\029\001\030\001\255\255\032\001\
\033\001\255\255\035\001\036\001\255\255\255\255\255\255\040\001\
\041\001\042\001\043\001\044\001\025\001\255\255\027\001\255\255\
\029\001\030\001\255\255\032\001\033\001\255\255\255\255\036\001\
\255\255\255\255\255\255\040\001\041\001\042\001\043\001\044\001\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\008\001\009\001\010\001\011\001\012\001\013\001\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\008\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\255\255\255\255\024\001\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\016\001\017\001\018\001\
\019\001\255\255\255\255\255\255\255\255\024\001"

let yynames_const = "\
  INT\000\
  FLOAT\000\
  BOOL\000\
  STRING\000\
  VOID\000\
  ARRAY\000\
  ASSIGN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  EXP\000\
  SEMC\000\
  PP\000\
  MM\000\
  LT\000\
  GT\000\
  LTE\000\
  GTE\000\
  EQ\000\
  NE\000\
  AND\000\
  OR\000\
  MOD\000\
  NOT\000\
  IN\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  DEF\000\
  RETURN\000\
  LPAREN\000\
  RPAREN\000\
  RBRACE\000\
  LBRACE\000\
  COMMA\000\
  LSBRAC\000\
  RSBRAC\000\
  EOF\000\
  "

let yynames_block = "\
  ILITERAL\000\
  FLITERAL\000\
  ID\000\
  SLITERAL\000\
  BLITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    Obj.repr(
# 52 "parser.mly"
              ( _1 )
# 394 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
                 ( ([], [], [])             )
# 400 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 57 "parser.mly"
                 ( ((_2 :: fst _1), snd _1, trd _1) )
# 408 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 58 "parser.mly"
                 ( (fst _1, (_2 :: snd _1), trd _1) )
# 416 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 59 "parser.mly"
                ( (fst _1, snd _1, (trd _1 @ [_2]))  )
# 424 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
              ( Expr _1 )
# 431 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 65 "parser.mly"
                         ( Return _2 )
# 438 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 66 "parser.mly"
                                            ( Block(List.rev _2)    )
# 445 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 453 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 68 "parser.mly"
                                         ( If(_3, _5, _7))
# 462 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
                                                    ( For(_3, _5, _7, _9) )
# 472 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                                  ( While(_3, _5) )
# 480 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                   ( [] )
# 486 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 75 "parser.mly"
                   ( _2 :: _1 )
# 494 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_assign) in
    Obj.repr(
# 79 "parser.mly"
                             ( (_1, _2, _3) )
# 503 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
  ( Noexpr )
# 509 "parser.ml"
               : 'vdecl_assign))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                ( _2 )
# 516 "parser.ml"
               : 'vdecl_assign))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                     ( [] )
# 522 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 88 "parser.mly"
                     ( _2 :: _1 )
# 530 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 93 "parser.mly"
  ( { typ = _2;
          fname = _3;
          formals = List.rev _5;
          locals = List.rev _8;
          body = List.rev _9 } )
# 545 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                  ( [] )
# 551 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 101 "parser.mly"
                ( _1 )
# 558 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
         ( [(_1,_2, Noexpr)] )
# 566 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
                             ( (_3,_4, Noexpr) :: _1 )
# 575 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                     ( Int )
# 581 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
                     ( Float )
# 587 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                     ( Boolean )
# 593 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                     ( String )
# 599 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                     ( Void )
# 605 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 115 "parser.mly"
                                 ( Array(_1,_3) )
# 613 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
                  ( Noexpr )
# 619 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                  ( _1 )
# 626 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 126 "parser.mly"
                       ( Liti(_1) )
# 633 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 127 "parser.mly"
                       ( Litf(_1) )
# 640 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 128 "parser.mly"
                       ( Litb(_1) )
# 647 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser.mly"
                       ( Lits(_1) )
# 654 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parser.mly"
                       ( Id(_1) )
# 661 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                       ( Binop(_1, Add, _3) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                       ( Binop(_1, Sub, _3) )
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                       ( Binop(_1, Mul, _3) )
# 685 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 693 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                       ( Binop(_1, Lt, _3) )
# 701 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                       ( Binop(_1, Gt, _3) )
# 709 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                       ( Binop(_1, Exp, _3) )
# 717 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                       ( Binop(_1, Mod, _3) )
# 725 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                       ( Binop(_1, Lte, _3) )
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                       ( Binop(_1, Gte, _3) )
# 741 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                       ( Binop(_1, Eq, _3) )
# 749 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                       ( Binop(_1, Ne, _3) )
# 757 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                       ( Binop(_1, And, _3) )
# 765 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                       ( Binop(_1, Or, _3) )
# 773 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                       ( Assign(_1, _3) )
# 781 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 146 "parser.mly"
                              ( Call(_1, _3)  )
# 789 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                       ( _2 )
# 796 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                       ( Uniop(Not, _2) )
# 803 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "parser.mly"
                  ( [] )
# 809 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 152 "parser.mly"
               ( List.rev _1 )
# 816 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 155 "parser.mly"
                            ( [_1] )
# 823 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
                         ( _3 :: _1 )
# 831 "parser.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
