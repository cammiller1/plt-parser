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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast

let fst (a,_,_) = a;;
let snd (_,b,_) = b;;
let trd (_,_,c) = c;;

# 56 "parser.ml"
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
    0 (* EOF *);
    0|]

let yytransl_block = [|
  294 (* ILITERAL *);
  295 (* FLITERAL *);
  296 (* ID *);
  297 (* SLITERAL *);
  298 (* BLITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\008\000\008\000\003\000\010\000\
\010\000\011\000\011\000\004\000\012\000\012\000\013\000\013\000\
\009\000\009\000\009\000\009\000\009\000\007\000\007\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\014\000\014\000\015\000\
\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\000\000\002\000\004\000\000\000\
\002\000\000\000\002\000\010\000\000\000\001\000\002\000\004\000\
\001\000\001\000\001\000\001\000\001\000\000\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\058\000\000\000\025\000\026\000\027\000\028\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\032\000\033\000\000\000\035\000\034\000\001\000\003\000\004\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000\053\000\008\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\039\000\040\000\043\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\044\000\000\000\
\000\000\000\000\000\000\000\000\000\000\052\000\000\000\000\000\
\015\000\000\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\000\000\010\000\000\000\018\000\
\000\000\000\000\000\000\024\000\011\000\019\000\000\000\020\000"

let yydgoto = "\002\000\
\003\000\004\000\023\000\024\000\061\000\026\000\033\000\035\000\
\027\000\081\000\107\000\094\000\095\000\064\000\065\000"

let yysindex = "\004\000\
\000\000\000\000\000\000\017\000\000\000\000\000\000\000\000\000\
\000\000\239\254\242\254\243\254\110\255\100\255\100\255\000\000\
\000\000\000\000\003\255\000\000\000\000\000\000\000\000\000\000\
\000\000\134\000\012\255\100\255\100\255\100\255\013\255\185\000\
\050\255\139\255\212\000\100\255\100\255\100\255\100\255\100\255\
\100\255\100\255\000\000\100\255\100\255\100\255\100\255\100\255\
\100\255\100\255\100\255\100\255\019\255\158\255\177\255\151\000\
\031\255\000\000\000\000\000\000\000\000\185\000\185\000\036\255\
\034\255\073\255\073\255\000\000\000\000\000\000\057\255\057\255\
\057\255\057\255\219\000\219\000\202\000\202\000\000\000\100\255\
\063\255\004\001\004\001\100\255\110\255\000\000\100\255\185\000\
\000\000\054\255\000\000\168\000\046\255\053\255\051\255\185\000\
\004\001\100\255\000\000\056\255\110\255\000\000\196\255\000\000\
\049\255\004\001\110\255\000\000\000\000\000\000\228\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\081\255\000\000\000\000\
\000\000\000\000\038\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\085\255\
\000\000\000\000\000\000\000\000\065\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\088\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\246\254\228\254\000\000\
\082\255\087\255\109\255\000\000\000\000\000\000\215\255\047\000\
\069\000\091\000\247\254\233\255\039\000\104\000\000\000\000\000\
\000\000\000\000\000\000\000\000\086\255\000\000\000\000\106\255\
\000\000\001\000\000\000\000\000\000\000\000\000\089\255\237\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\244\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\027\000\000\000\252\255\249\255\000\000\028\000\
\245\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 558
let yytable = "\025\000\
\009\000\031\000\051\000\047\000\001\000\056\000\032\000\034\000\
\056\000\036\000\047\000\047\000\047\000\047\000\057\000\028\000\
\022\000\057\000\029\000\030\000\054\000\055\000\056\000\051\000\
\047\000\080\000\051\000\047\000\062\000\063\000\066\000\067\000\
\068\000\069\000\070\000\037\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\079\000\036\000\036\000\036\000\
\036\000\036\000\036\000\053\000\057\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\058\000\085\000\
\038\000\039\000\040\000\041\000\042\000\086\000\087\000\036\000\
\088\000\093\000\036\000\089\000\092\000\090\000\091\000\096\000\
\052\000\097\000\040\000\041\000\042\000\099\000\100\000\101\000\
\108\000\105\000\103\000\104\000\102\000\030\000\037\000\037\000\
\052\000\031\000\054\000\037\000\016\000\109\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000\037\000\005\000\006\000\
\007\000\008\000\009\000\055\000\038\000\038\000\017\000\021\000\
\037\000\038\000\022\000\037\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\015\000\110\000\111\000\000\000\
\000\000\017\000\018\000\019\000\020\000\021\000\038\000\000\000\
\000\000\038\000\038\000\039\000\040\000\041\000\042\000\000\000\
\000\000\000\000\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\000\000\000\000\038\000\039\000\040\000\
\041\000\042\000\000\000\000\000\059\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\000\000\000\000\
\038\000\039\000\040\000\041\000\042\000\000\000\000\000\082\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\000\000\000\000\038\000\039\000\040\000\041\000\042\000\
\000\000\000\000\083\000\044\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\000\000\106\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\000\000\000\000\000\
\041\000\000\000\000\000\041\000\048\000\048\000\048\000\048\000\
\000\000\009\000\009\000\009\000\009\000\009\000\000\000\000\000\
\000\000\000\000\048\000\000\000\000\000\048\000\000\000\000\000\
\000\000\005\000\006\000\007\000\008\000\009\000\000\000\000\000\
\000\000\000\000\000\000\009\000\000\000\009\000\009\000\009\000\
\009\000\009\000\000\000\009\000\009\000\000\000\009\000\009\000\
\009\000\009\000\009\000\010\000\000\000\011\000\012\000\013\000\
\014\000\015\000\000\000\049\000\016\000\000\000\017\000\018\000\
\019\000\020\000\021\000\042\000\049\000\049\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\000\000\000\000\
\049\000\000\000\000\000\049\000\000\000\000\000\000\000\000\000\
\042\000\045\000\000\000\042\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\045\000\046\000\
\000\000\045\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\000\000\000\000\050\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\046\000\050\000\050\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\050\000\000\000\000\000\050\000\038\000\039\000\040\000\
\041\000\042\000\043\000\000\000\000\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\038\000\039\000\
\040\000\041\000\042\000\084\000\000\000\000\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\038\000\
\039\000\040\000\041\000\042\000\098\000\000\000\000\000\044\000\
\045\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\038\000\039\000\040\000\041\000\042\000\000\000\000\000\000\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\038\000\039\000\040\000\041\000\042\000\000\000\000\000\
\000\000\044\000\045\000\046\000\047\000\048\000\049\000\000\000\
\000\000\052\000\038\000\039\000\040\000\041\000\042\000\000\000\
\000\000\000\000\044\000\045\000\046\000\047\000\010\000\000\000\
\011\000\012\000\052\000\014\000\015\000\000\000\060\000\016\000\
\000\000\017\000\018\000\019\000\020\000\021\000\010\000\000\000\
\011\000\012\000\000\000\014\000\015\000\000\000\112\000\016\000\
\000\000\017\000\018\000\019\000\020\000\021\000\013\000\000\000\
\013\000\013\000\000\000\013\000\013\000\000\000\013\000\013\000\
\000\000\013\000\013\000\013\000\013\000\013\000\010\000\000\000\
\011\000\012\000\000\000\014\000\015\000\000\000\000\000\016\000\
\000\000\017\000\018\000\019\000\020\000\021\000"

let yycheck = "\004\000\
\000\000\013\000\013\001\013\001\001\000\034\001\014\000\015\000\
\037\001\007\001\020\001\021\001\022\001\023\001\034\001\033\001\
\000\000\037\001\033\001\033\001\028\000\029\000\030\000\034\001\
\034\001\007\001\037\001\037\001\036\000\037\000\038\000\039\000\
\040\000\041\000\042\000\033\001\044\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\008\001\009\001\010\001\
\011\001\012\001\013\001\040\001\040\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\013\001\033\001\
\008\001\009\001\010\001\011\001\012\001\034\001\037\001\034\001\
\080\000\085\000\037\001\013\001\084\000\082\000\083\000\087\000\
\024\001\028\001\010\001\011\001\012\001\040\001\034\001\037\001\
\040\001\101\000\098\000\036\001\097\000\013\001\008\001\009\001\
\024\001\013\001\034\001\013\001\013\001\106\000\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\001\001\002\001\
\003\001\004\001\005\001\034\001\008\001\009\001\013\001\034\001\
\034\001\013\001\034\001\037\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\033\001\107\000\107\000\255\255\
\255\255\038\001\039\001\040\001\041\001\042\001\034\001\255\255\
\255\255\037\001\008\001\009\001\010\001\011\001\012\001\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\255\255\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\034\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\255\255\255\255\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\034\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\255\255\255\255\008\001\009\001\010\001\011\001\012\001\
\255\255\255\255\034\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\013\001\255\255\034\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\013\001\255\255\255\255\
\034\001\255\255\255\255\037\001\020\001\021\001\022\001\023\001\
\255\255\001\001\002\001\003\001\004\001\005\001\255\255\255\255\
\255\255\255\255\034\001\255\255\255\255\037\001\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\255\255\255\255\
\255\255\255\255\255\255\027\001\255\255\029\001\030\001\031\001\
\032\001\033\001\255\255\035\001\036\001\255\255\038\001\039\001\
\040\001\041\001\042\001\027\001\255\255\029\001\030\001\031\001\
\032\001\033\001\255\255\013\001\036\001\255\255\038\001\039\001\
\040\001\041\001\042\001\013\001\022\001\023\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\255\255\255\255\
\034\001\255\255\255\255\037\001\255\255\255\255\255\255\255\255\
\034\001\013\001\255\255\037\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\034\001\013\001\
\255\255\037\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\255\255\255\255\013\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\034\001\022\001\023\001\037\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\034\001\255\255\255\255\037\001\008\001\009\001\010\001\
\011\001\012\001\013\001\255\255\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\008\001\009\001\
\010\001\011\001\012\001\013\001\255\255\255\255\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\255\255\
\255\255\024\001\008\001\009\001\010\001\011\001\012\001\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\027\001\255\255\
\029\001\030\001\024\001\032\001\033\001\255\255\035\001\036\001\
\255\255\038\001\039\001\040\001\041\001\042\001\027\001\255\255\
\029\001\030\001\255\255\032\001\033\001\255\255\035\001\036\001\
\255\255\038\001\039\001\040\001\041\001\042\001\027\001\255\255\
\029\001\030\001\255\255\032\001\033\001\255\255\035\001\036\001\
\255\255\038\001\039\001\040\001\041\001\042\001\027\001\255\255\
\029\001\030\001\255\255\032\001\033\001\255\255\255\255\036\001\
\255\255\038\001\039\001\040\001\041\001\042\001"

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
# 51 "parser.mly"
              ( _1 )
# 383 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                 ( ([], [], [])             )
# 389 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 56 "parser.mly"
                 ( ((_2 :: fst _1), snd _1, trd _1) )
# 397 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 57 "parser.mly"
                 ( (fst _1, (_2 :: snd _1), trd _1) )
# 405 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 58 "parser.mly"
                ( (fst _1, snd _1, (trd _1 @ [_2]))  )
# 413 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
              ( Expr _1 )
# 420 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 64 "parser.mly"
                         ( Return _2 )
# 427 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 65 "parser.mly"
                                            ( Block(List.rev _2)    )
# 434 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 66 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 442 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser.mly"
                                         ( If(_3, _5, _7))
# 451 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 68 "parser.mly"
                                                    ( For(_3, _5, _7, _9) )
# 461 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
                                  ( While(_3, _5) )
# 469 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                   ( [] )
# 475 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 74 "parser.mly"
                   ( _2 :: _1 )
# 483 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_assign) in
    Obj.repr(
# 78 "parser.mly"
                             ( (_1, _2, _3) )
# 492 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
  ( Noexpr )
# 498 "parser.ml"
               : 'vdecl_assign))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                ( _2 )
# 505 "parser.ml"
               : 'vdecl_assign))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                     ( [] )
# 511 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 87 "parser.mly"
                     ( _2 :: _1 )
# 519 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 92 "parser.mly"
  ( { typ = _2;
          fname = _3;
          formals = List.rev _5;
          locals = List.rev _8;
          body = List.rev _9 } )
# 534 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
                  ( [] )
# 540 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 100 "parser.mly"
                ( _1 )
# 547 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "parser.mly"
         ( [(_1,_2, Noexpr)] )
# 555 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
                             ( (_3,_4, Noexpr) :: _1 )
# 564 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
                     ( Int )
# 570 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                     ( Float )
# 576 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
                     ( Boolean )
# 582 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                     ( String )
# 588 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                     ( Void )
# 594 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
                  ( Noexpr )
# 600 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                  ( _1 )
# 607 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 125 "parser.mly"
                       ( Liti(_1) )
# 614 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 126 "parser.mly"
                       ( Litf(_1) )
# 621 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 127 "parser.mly"
                       ( Litb(_1) )
# 628 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 128 "parser.mly"
                       ( Lits(_1) )
# 635 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser.mly"
                       ( Id(_1) )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                       ( Binop(_1, Add, _3) )
# 650 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                       ( Binop(_1, Sub, _3) )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                       ( Binop(_1, Mul, _3) )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 674 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                       ( Binop(_1, Lt, _3) )
# 682 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                       ( Binop(_1, Gt, _3) )
# 690 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                       ( Binop(_1, Exp, _3) )
# 698 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                       ( Binop(_1, Mod, _3) )
# 706 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                       ( Binop(_1, Lte, _3) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                       ( Binop(_1, Gte, _3) )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                       ( Binop(_1, Eq, _3) )
# 730 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                       ( Binop(_1, Ne, _3) )
# 738 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                       ( Binop(_1, And, _3) )
# 746 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                       ( Binop(_1, Or, _3) )
# 754 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                       ( Assign(_1, _3) )
# 762 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 145 "parser.mly"
                              ( Call(_1, _3)  )
# 770 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                       ( _2 )
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
                  ( [] )
# 783 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 150 "parser.mly"
               ( List.rev _1 )
# 790 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
                            ( [_1] )
# 797 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "parser.mly"
                         ( _3 :: _1 )
# 805 "parser.ml"
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
