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
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\007\000\007\000\007\000\007\000\007\000\007\000\004\000\008\000\
\008\000\010\000\010\000\009\000\009\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\003\000\002\000\
\002\000\003\000\007\000\011\000\011\000\007\000\009\000\000\000\
\001\000\002\000\004\000\000\000\002\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\046\000\000\000\000\000\005\000\006\000\007\000\
\008\000\001\000\003\000\004\000\000\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\042\000\000\000\000\000\
\044\000\000\000\000\000\043\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\014\000\015\000\000\000\
\016\000\000\000\045\000\000\000\029\000\030\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\000\000\000\000\000\000\034\000\031\000\
\000\000\000\000\000\000\012\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\021\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\037\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\042\000\043\000\020\000\
\027\000\021\000"

let yysindex = "\009\000\
\000\000\000\000\000\000\001\000\021\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\230\254\233\254\255\254\254\254\
\000\000\021\255\010\255\032\255\018\255\000\000\041\255\021\255\
\000\000\031\255\039\255\000\000\100\255\100\255\100\255\045\255\
\046\255\047\255\100\255\100\255\000\000\000\000\000\000\077\255\
\000\000\115\000\000\000\175\000\000\000\000\000\100\255\100\255\
\100\255\130\000\163\255\100\255\100\255\100\255\100\255\100\255\
\100\255\100\255\100\255\100\255\100\255\100\255\100\255\100\255\
\100\255\100\255\000\000\189\255\215\255\145\000\000\000\000\000\
\175\000\036\255\036\255\000\000\000\000\211\000\211\000\211\000\
\211\000\203\000\203\000\189\000\189\000\000\000\000\000\050\255\
\054\255\100\255\087\255\087\255\160\000\056\255\059\255\100\255\
\074\255\000\000\241\255\066\255\068\255\087\255\087\255\083\255\
\086\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\075\255\000\000\000\000\081\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\137\255\
\000\000\000\000\000\000\247\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\252\254\011\000\034\000\000\000\000\000\046\000\058\000\070\000\
\082\000\033\255\089\000\080\255\103\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\063\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\251\255\229\255\170\255\000\000\
\000\000\000\000"

let yytablesize = 482
let yytable = "\014\000\
\010\000\044\000\045\000\046\000\094\000\095\000\032\000\050\000\
\051\000\001\000\015\000\018\000\019\000\016\000\017\000\104\000\
\105\000\032\000\026\000\068\000\069\000\070\000\018\000\018\000\
\073\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\055\000\
\056\000\029\000\025\000\025\000\025\000\025\000\022\000\024\000\
\025\000\065\000\066\000\006\000\007\000\008\000\009\000\030\000\
\031\000\032\000\023\000\025\000\033\000\034\000\093\000\035\000\
\036\000\035\000\037\000\028\000\099\000\025\000\047\000\048\000\
\049\000\038\000\039\000\040\000\041\000\052\000\091\000\035\000\
\035\000\035\000\092\000\097\000\035\000\035\000\098\000\035\000\
\035\000\029\000\035\000\027\000\027\000\100\000\102\000\027\000\
\103\000\035\000\035\000\035\000\035\000\040\000\029\000\030\000\
\031\000\032\000\027\000\041\000\033\000\034\000\106\000\035\000\
\036\000\107\000\028\000\028\000\030\000\031\000\028\000\000\000\
\000\000\038\000\039\000\040\000\041\000\036\000\000\000\000\000\
\000\000\028\000\000\000\000\000\000\000\000\000\038\000\039\000\
\040\000\041\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\072\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\088\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\089\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\101\000\010\000\010\000\000\000\000\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\005\000\000\000\010\000\000\000\000\000\000\000\000\000\006\000\
\007\000\008\000\009\000\011\000\011\000\010\000\000\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\000\000\
\000\000\011\000\000\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\000\000\011\000\019\000\000\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\000\000\
\019\000\020\000\000\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\000\000\020\000\023\000\000\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\000\000\
\023\000\024\000\026\000\026\000\026\000\026\000\000\000\000\000\
\026\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\071\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\090\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\096\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\000\000\000\000\065\000\066\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\053\000\054\000\055\000\056\000\
\065\000\066\000\000\000\000\000\000\000\000\000\000\000\000\000\
\065\000\066\000"

let yycheck = "\005\000\
\000\000\029\000\030\000\031\000\091\000\092\000\016\001\035\000\
\036\000\001\000\037\001\016\001\018\000\037\001\016\001\102\000\
\103\000\027\001\024\000\047\000\048\000\049\000\027\001\026\001\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\004\001\
\005\001\003\001\010\001\011\001\012\001\013\001\037\001\030\001\
\016\001\014\001\015\001\031\001\032\001\033\001\034\001\017\001\
\018\001\019\001\027\001\027\001\022\001\023\001\090\000\025\001\
\026\001\003\001\028\001\037\001\096\000\029\001\026\001\026\001\
\026\001\035\001\036\001\037\001\038\001\001\001\029\001\017\001\
\018\001\019\001\029\001\028\001\022\001\023\001\028\001\025\001\
\026\001\003\001\028\001\012\001\013\001\020\001\029\001\016\001\
\029\001\035\001\036\001\037\001\038\001\027\001\003\001\017\001\
\018\001\019\001\027\001\027\001\022\001\023\001\028\001\025\001\
\026\001\028\001\012\001\013\001\017\001\018\001\016\001\255\255\
\255\255\035\001\036\001\037\001\038\001\026\001\255\255\255\255\
\255\255\027\001\255\255\255\255\255\255\255\255\035\001\036\001\
\037\001\038\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\027\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\027\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\027\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001\002\001\003\001\255\255\255\255\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\024\001\255\255\016\001\255\255\255\255\255\255\255\255\031\001\
\032\001\033\001\034\001\002\001\003\001\027\001\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\016\001\255\255\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\255\255\027\001\016\001\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\027\001\016\001\255\255\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\255\255\027\001\016\001\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\027\001\016\001\010\001\011\001\012\001\013\001\255\255\255\255\
\016\001\255\255\255\255\255\255\027\001\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\014\001\015\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\002\001\003\001\004\001\005\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\014\001\015\001"

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
# 344 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                 ( ([], [])               )
# 350 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 40 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 358 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 41 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 366 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                       ( Int )
# 372 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                     ( Float )
# 378 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                     ( Boolean )
# 384 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                     ( String )
# 390 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 52 "parser.mly"
                     ( (_1, _2) )
# 398 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                       ( Binop(_1, Add, _3) )
# 406 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                       ( Binop(_1, Sub, _3) )
# 414 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                       ( Binop(_1, Mul, _3) )
# 422 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 430 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                       ( Liti(_1) )
# 437 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 60 "parser.mly"
                       ( Litf(_1) )
# 444 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 61 "parser.mly"
                       ( Litb(_1) )
# 451 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                       ( Var(_1) )
# 458 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                         ( Assign(_1, _3) )
# 466 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                       ( Binop(_1, Lt, _3) )
# 474 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                       ( Binop(_1, Gt, _3) )
# 482 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                       ( Binop(_1, Exp, _3) )
# 490 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                       ( Binop(_1, Mod, _3) )
# 498 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                       ( Binop(_1, Lte, _3) )
# 506 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                       ( Binop(_1, Gte, _3) )
# 514 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                       ( Binop(_1, Eq, _3) )
# 522 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                       ( Binop(_1, Ne, _3) )
# 530 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                       ( Binop(_1, And, _3) )
# 538 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                       ( Binop(_1, Or, _3) )
# 546 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                       ( Uniop(Pp, _2) )
# 553 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                       ( Uniop(Mm, _2) )
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                       ( _2 )
# 567 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                            ( Uniop(Neg, _2) )
# 574 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
              ( Expr _1 )
# 581 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                     ( Return _2 )
# 588 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
                                                          ( If(_3, _6, Block([])))
# 596 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 84 "parser.mly"
                                                                     ( If(_3, _6, _10))
# 605 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 85 "parser.mly"
                                                                  ( For(_3, _5, _7, _10) )
# 615 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 86 "parser.mly"
                                                ( While(_3, _6) )
# 623 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 91 "parser.mly"
  ( { typ = _2;
  fname = _3;
  formals = List.rev _5;
  body = List.rev _8 } )
# 636 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                  ( [] )
# 642 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 98 "parser.mly"
                ( _1 )
# 649 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "parser.mly"
               ( [(_1,_2)] )
# 657 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "parser.mly"
                                   ( (_3,_4) :: _1 )
# 666 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                  ( [] )
# 672 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                   ( _2 :: _1 )
# 680 "parser.ml"
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
