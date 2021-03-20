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
  | ILITERAL of (int)
  | FLITERAL of (float)
  | VARIABLE of (string)
  | BLITERAL of (bool)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 47 "parser.ml"
let yytransl_const = [|
  257 (* ASSIGN *);
  258 (* PLUS *);
  259 (* MINUS *);
  260 (* TIMES *);
  261 (* DIVIDE *);
  262 (* LT *);
  263 (* GT *);
  264 (* LTE *);
  265 (* GTE *);
  266 (* EQ *);
  267 (* NE *);
  268 (* AND *);
  269 (* OR *);
  270 (* MOD *);
  271 (* EXP *);
  272 (* SEMC *);
  273 (* PP *);
  274 (* MM *);
  275 (* IF *);
  276 (* ELSE *);
  277 (* ELIF *);
  278 (* WHILE *);
  279 (* FOR *);
  280 (* DEF *);
  281 (* RETURN *);
  282 (* LPAREN *);
  283 (* RPAREN *);
  284 (* RBRACE *);
  285 (* LBRACE *);
  286 (* COMMA *);
  287 (* INT *);
  288 (* FLOAT *);
  289 (* BOOL *);
  290 (* STRING *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  291 (* ILITERAL *);
  292 (* FLITERAL *);
  293 (* VARIABLE *);
  294 (* BLITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\005\000\005\000\
\003\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\007\000\007\000\
\007\000\007\000\007\000\007\000\004\000\008\000\008\000\010\000\
\010\000\009\000\009\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\007\000\011\000\011\000\007\000\009\000\000\000\001\000\002\000\
\004\000\000\000\002\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\044\000\000\000\000\000\005\000\006\000\007\000\
\008\000\001\000\003\000\004\000\000\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\
\042\000\000\000\000\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\014\000\015\000\000\000\016\000\000\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\000\000\
\000\000\000\000\032\000\029\000\000\000\000\000\000\000\012\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\035\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\040\000\041\000\020\000\
\027\000\021\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\001\000\232\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\230\254\255\254\026\255\022\255\
\000\000\232\254\012\255\041\255\055\255\000\000\058\255\232\254\
\000\000\047\255\015\255\000\000\009\255\060\255\064\255\065\255\
\009\255\009\255\000\000\000\000\000\000\110\255\000\000\078\000\
\000\000\138\000\009\255\009\255\009\255\093\000\120\255\009\255\
\009\255\009\255\009\255\009\255\009\255\009\255\009\255\009\255\
\009\255\009\255\009\255\009\255\009\255\009\255\000\000\146\255\
\172\255\108\000\000\000\000\000\138\000\062\000\062\000\000\000\
\000\000\174\000\174\000\174\000\174\000\166\000\166\000\152\000\
\152\000\000\000\000\000\083\255\084\255\009\255\057\255\057\255\
\123\000\086\255\087\255\009\255\096\255\000\000\198\255\088\255\
\089\255\057\255\057\255\091\255\092\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\109\255\000\000\000\000\111\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\094\255\000\000\000\000\
\000\000\038\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\220\255\232\255\000\000\
\000\000\244\255\010\000\032\000\044\000\253\255\052\000\246\254\
\054\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\051\000\227\255\246\255\000\000\
\000\000\000\000"

let yytablesize = 445
let yytable = "\042\000\
\010\000\027\000\027\000\046\000\047\000\027\000\006\000\007\000\
\008\000\009\000\015\000\029\000\001\000\064\000\065\000\066\000\
\027\000\029\000\069\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\083\000\030\000\034\000\016\000\031\000\032\000\033\000\033\000\
\034\000\017\000\035\000\036\000\037\000\038\000\039\000\018\000\
\022\000\036\000\037\000\038\000\039\000\030\000\033\000\014\000\
\089\000\033\000\033\000\029\000\033\000\033\000\095\000\033\000\
\030\000\028\000\028\000\023\000\019\000\028\000\033\000\033\000\
\033\000\033\000\026\000\030\000\090\000\091\000\031\000\032\000\
\028\000\033\000\034\000\028\000\024\000\043\000\025\000\100\000\
\101\000\044\000\045\000\036\000\037\000\038\000\039\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\048\000\087\000\
\088\000\093\000\094\000\096\000\098\000\099\000\102\000\103\000\
\017\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\038\000\
\000\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\068\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\084\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\085\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\010\000\000\000\
\097\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\011\000\011\000\010\000\000\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\000\000\010\000\011\000\
\000\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\000\000\011\000\019\000\000\000\000\000\025\000\025\000\
\025\000\025\000\000\000\000\000\025\000\000\000\019\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\025\000\
\005\000\020\000\000\000\000\000\000\000\000\000\000\000\006\000\
\007\000\008\000\009\000\018\000\020\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\000\000\018\000\023\000\
\000\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\000\000\023\000\024\000\000\000\026\000\026\000\026\000\
\026\000\051\000\052\000\026\000\000\000\000\000\024\000\000\000\
\000\000\000\000\000\000\061\000\062\000\000\000\026\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\067\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\086\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\092\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\000\000\000\000\061\000\062\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\049\000\
\050\000\051\000\052\000\061\000\062\000\000\000\000\000\000\000\
\000\000\000\000\000\000\061\000\062\000"

let yycheck = "\029\000\
\000\000\012\001\013\001\033\000\034\000\016\001\031\001\032\001\
\033\001\034\001\037\001\003\001\001\000\043\000\044\000\045\000\
\027\001\003\001\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\019\001\026\001\037\001\022\001\023\001\003\001\025\001\
\026\001\016\001\028\001\035\001\036\001\037\001\038\001\026\001\
\037\001\035\001\036\001\037\001\038\001\016\001\019\001\005\000\
\086\000\022\001\023\001\003\001\025\001\026\001\092\000\028\001\
\027\001\012\001\013\001\027\001\018\000\016\001\035\001\036\001\
\037\001\038\001\024\000\019\001\087\000\088\000\022\001\023\001\
\027\001\025\001\026\001\037\001\030\001\026\001\029\001\098\000\
\099\000\026\001\026\001\035\001\036\001\037\001\038\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\001\001\029\001\
\029\001\028\001\028\001\020\001\029\001\029\001\028\001\028\001\
\027\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\027\001\
\255\255\027\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\027\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\027\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\027\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\002\001\003\001\255\255\
\027\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\002\001\003\001\016\001\255\255\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\027\001\016\001\
\255\255\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\255\255\027\001\016\001\255\255\255\255\010\001\011\001\
\012\001\013\001\255\255\255\255\016\001\255\255\027\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\027\001\
\024\001\016\001\255\255\255\255\255\255\255\255\255\255\031\001\
\032\001\033\001\034\001\016\001\027\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\027\001\016\001\
\255\255\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\255\255\027\001\016\001\255\255\010\001\011\001\012\001\
\013\001\004\001\005\001\016\001\255\255\255\255\027\001\255\255\
\255\255\255\255\255\255\014\001\015\001\255\255\027\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\014\001\015\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\002\001\
\003\001\004\001\005\001\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001"

let yynames_const = "\
  ASSIGN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  LT\000\
  GT\000\
  LTE\000\
  GTE\000\
  EQ\000\
  NE\000\
  AND\000\
  OR\000\
  MOD\000\
  EXP\000\
  SEMC\000\
  PP\000\
  MM\000\
  IF\000\
  ELSE\000\
  ELIF\000\
  WHILE\000\
  FOR\000\
  DEF\000\
  RETURN\000\
  LPAREN\000\
  RPAREN\000\
  RBRACE\000\
  LBRACE\000\
  COMMA\000\
  INT\000\
  FLOAT\000\
  BOOL\000\
  STRING\000\
  EOF\000\
  "

let yynames_block = "\
  ILITERAL\000\
  FLITERAL\000\
  VARIABLE\000\
  BLITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 36 "parser.mly"
            ( _1 )
# 331 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                 ( ([], [])               )
# 337 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 40 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 345 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 41 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 353 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                       ( Int )
# 359 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                     ( Float )
# 365 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                     ( Boolean )
# 371 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                     ( String )
# 377 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 52 "parser.mly"
                     ( (_1, _2) )
# 385 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                       ( Binop(_1, Add, _3) )
# 393 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                       ( Binop(_1, Sub, _3) )
# 401 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                       ( Binop(_1, Mul, _3) )
# 409 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 417 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                       ( Liti(_1) )
# 424 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 60 "parser.mly"
                       ( Litf(_1) )
# 431 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 61 "parser.mly"
                       ( Litb(_1) )
# 438 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                       ( Var(_1) )
# 445 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                         ( Assign(_1, _3) )
# 453 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                       ( Binop(_1, Lt, _3) )
# 461 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                       ( Binop(_1, Gt, _3) )
# 469 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                       ( Binop(_1, Exp, _3) )
# 477 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                       ( Binop(_1, Mod, _3) )
# 485 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                       ( Binop(_1, Lte, _3) )
# 493 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                       ( Binop(_1, Gte, _3) )
# 501 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                       ( Binop(_1, Eq, _3) )
# 509 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                       ( Binop(_1, Ne, _3) )
# 517 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                       ( Binop(_1, And, _3) )
# 525 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                       ( Binop(_1, Or, _3) )
# 533 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                       ( _2 )
# 540 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                            ( Uniop(Neg, _2) )
# 547 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
              ( Expr _1 )
# 554 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Return _2 )
# 561 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 81 "parser.mly"
                                                          ( If(_3, _6, Block([])))
# 569 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 82 "parser.mly"
                                                                     ( If(_3, _6, _10))
# 578 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
                                                                  ( For(_3, _5, _7, _10) )
# 588 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 84 "parser.mly"
                                                ( While(_3, _6) )
# 596 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 89 "parser.mly"
  ( { typ = _2;
  fname = _3;
  formals = List.rev _5;
  body = List.rev _8 } )
# 609 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                  ( [] )
# 615 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 96 "parser.mly"
                ( _1 )
# 622 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "parser.mly"
               ( [(_1,_2)] )
# 630 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
                                   ( (_3,_4) :: _1 )
# 639 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                  ( [] )
# 645 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                   ( _2 :: _1 )
# 653 "parser.ml"
               : 'stmt_list))
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
