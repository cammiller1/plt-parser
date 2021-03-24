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
  | NONE
  | ILITERAL of (int)
  | FLITERAL of (float)
  | ID of (string)
  | BLITERAL of (bool)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 48 "parser.ml"
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
  291 (* NONE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  292 (* ILITERAL *);
  293 (* FLITERAL *);
  294 (* ID *);
  295 (* BLITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\007\000\007\000\
\003\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\012\000\012\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\010\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\000\000\002\000\
\003\000\000\000\002\000\002\000\003\000\003\000\007\000\011\000\
\011\000\007\000\000\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\000\000\010\000\011\000\012\000\
\013\000\014\000\001\000\003\000\004\000\000\000\000\000\000\000\
\000\000\017\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\015\000\000\000\000\000\009\000\016\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\018\000\029\000\030\000\
\000\000\031\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\020\000\000\000\000\000\000\000\021\000\
\049\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\000\036\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\000\039\000\000\000\000\000\000\000\048\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\021\000\028\000\031\000\
\022\000\043\000\044\000\049\000\077\000\078\000"

let yysindex = "\255\255\
\000\000\000\000\000\000\001\000\067\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\227\254\228\254\251\254\
\029\255\000\000\067\255\021\255\034\255\035\255\000\000\037\255\
\067\255\000\000\054\255\067\255\000\000\000\000\235\255\069\255\
\071\255\077\255\012\000\012\000\000\000\000\000\000\000\000\000\
\057\255\000\000\000\000\140\000\012\000\012\000\012\000\185\000\
\088\255\107\255\018\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\000\000\133\255\159\255\155\000\000\000\
\000\000\000\000\185\000\185\000\079\255\093\255\002\255\002\255\
\000\000\000\000\221\000\221\000\221\000\221\000\213\000\213\000\
\199\000\199\000\000\000\000\000\095\255\096\255\012\000\000\000\
\012\000\081\000\081\000\170\000\185\000\098\255\099\255\012\000\
\087\255\000\000\185\255\100\255\101\255\081\000\081\000\103\255\
\104\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\106\255\000\000\000\000\122\255\000\000\000\000\
\000\000\000\000\000\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\112\255\000\000\000\000\000\000\000\000\000\000\
\033\255\000\000\000\000\000\000\000\000\000\000\000\000\134\255\
\000\000\000\000\000\000\000\000\124\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\244\254\234\254\000\000\125\255\066\255\078\255\
\000\000\000\000\195\255\207\255\220\255\232\255\040\255\111\000\
\240\255\253\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\255\000\000\000\000\000\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\125\000\000\000\230\000\000\000\000\000\116\000\
\000\000\217\000\223\255\000\000\000\000\000\000"

let yytablesize = 492
let yytable = "\001\000\
\011\000\048\000\050\000\047\000\052\000\056\000\057\000\052\000\
\016\000\017\000\018\000\069\000\070\000\071\000\047\000\066\000\
\067\000\047\000\075\000\076\000\079\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\090\000\
\091\000\092\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\043\000\043\000\043\000\043\000\053\000\019\000\043\000\
\053\000\052\000\023\000\032\000\024\000\100\000\032\000\101\000\
\025\000\026\000\043\000\033\000\033\000\043\000\107\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\034\000\
\034\000\033\000\053\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\029\000\033\000\034\000\045\000\033\000\
\046\000\006\000\007\000\008\000\009\000\010\000\047\000\072\000\
\034\000\096\000\108\000\034\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\097\000\098\000\099\000\105\000\106\000\027\000\
\110\000\111\000\114\000\115\000\006\000\073\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\007\000\028\000\050\000\051\000\
\030\000\051\000\000\000\000\000\000\000\000\000\000\000\093\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\094\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\000\000\000\000\037\000\109\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\000\000\037\000\038\000\000\000\
\037\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\038\000\015\000\041\000\038\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\000\000\041\000\042\000\
\020\000\041\000\000\000\045\000\045\000\032\000\027\000\045\000\
\033\000\034\000\042\000\035\000\036\000\042\000\037\000\038\000\
\046\000\046\000\045\000\000\000\046\000\045\000\039\000\040\000\
\041\000\042\000\000\000\000\000\000\000\000\000\000\000\046\000\
\005\000\000\000\046\000\000\000\000\000\000\000\000\000\006\000\
\007\000\008\000\009\000\010\000\032\000\036\000\000\000\033\000\
\034\000\000\000\035\000\036\000\000\000\074\000\038\000\039\000\
\040\000\041\000\042\000\000\000\000\000\039\000\040\000\041\000\
\042\000\018\000\102\000\103\000\018\000\018\000\000\000\018\000\
\018\000\000\000\018\000\018\000\000\000\000\000\112\000\113\000\
\000\000\000\000\018\000\018\000\018\000\018\000\023\000\000\000\
\000\000\023\000\023\000\000\000\023\000\023\000\000\000\023\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\023\000\023\000\023\000\032\000\000\000\000\000\033\000\034\000\
\000\000\035\000\036\000\000\000\000\000\038\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\040\000\041\000\042\000\
\044\000\044\000\044\000\044\000\000\000\000\000\044\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\000\000\000\000\044\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\095\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\104\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\000\000\000\000\066\000\067\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\054\000\055\000\
\056\000\057\000\066\000\067\000\000\000\000\000\000\000\000\000\
\000\000\000\000\066\000\067\000"

let yycheck = "\001\000\
\000\000\035\000\036\000\016\001\027\001\004\001\005\001\030\001\
\038\001\038\001\016\001\045\000\046\000\047\000\027\001\014\001\
\015\001\030\001\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\010\001\011\001\012\001\013\001\027\001\026\001\016\001\
\030\001\001\001\038\001\027\001\027\001\095\000\030\001\097\000\
\030\001\029\001\027\001\002\001\003\001\030\001\104\000\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\002\001\
\003\001\016\001\026\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\038\001\027\001\016\001\026\001\030\001\
\026\001\031\001\032\001\033\001\034\001\035\001\026\001\016\001\
\027\001\027\001\020\001\030\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\030\001\029\001\029\001\028\001\028\001\016\001\
\029\001\029\001\028\001\028\001\027\001\027\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\027\001\016\001\027\001\027\001\
\028\000\038\000\255\255\255\255\255\255\255\255\255\255\027\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\027\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\255\255\255\255\016\001\027\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\255\255\027\001\016\001\255\255\
\030\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\027\001\005\000\016\001\030\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\027\001\016\001\
\019\000\030\001\255\255\012\001\013\001\019\001\025\000\016\001\
\022\001\023\001\027\001\025\001\026\001\030\001\028\001\029\001\
\012\001\013\001\027\001\255\255\016\001\030\001\036\001\037\001\
\038\001\039\001\255\255\255\255\255\255\255\255\255\255\027\001\
\024\001\255\255\030\001\255\255\255\255\255\255\255\255\031\001\
\032\001\033\001\034\001\035\001\019\001\026\001\255\255\022\001\
\023\001\255\255\025\001\026\001\255\255\028\001\029\001\036\001\
\037\001\038\001\039\001\255\255\255\255\036\001\037\001\038\001\
\039\001\019\001\098\000\099\000\022\001\023\001\255\255\025\001\
\026\001\255\255\028\001\029\001\255\255\255\255\110\000\111\000\
\255\255\255\255\036\001\037\001\038\001\039\001\019\001\255\255\
\255\255\022\001\023\001\255\255\025\001\026\001\255\255\028\001\
\029\001\255\255\255\255\255\255\255\255\255\255\255\255\036\001\
\037\001\038\001\039\001\019\001\255\255\255\255\022\001\023\001\
\255\255\025\001\026\001\255\255\255\255\029\001\255\255\255\255\
\255\255\255\255\255\255\255\255\036\001\037\001\038\001\039\001\
\010\001\011\001\012\001\013\001\255\255\255\255\016\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\027\001\255\255\255\255\030\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\002\001\003\001\
\004\001\005\001\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\014\001\015\001"

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
  NONE\000\
  EOF\000\
  "

let yynames_block = "\
  ILITERAL\000\
  FLITERAL\000\
  ID\000\
  BLITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 36 "parser.mly"
            ( _1 )
# 354 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                 ( ([], [])               )
# 360 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 40 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 368 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 41 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 376 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 45 "parser.mly"
     ( { typ = _2;
   fname = _3;
   formals = List.rev _5;
   locals = List.rev _8;
   body = List.rev _9 } )
# 391 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                  ( [] )
# 397 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 53 "parser.mly"
                ( _1 )
# 404 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
         ( [(_1,_2)] )
# 412 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                             ( (_3,_4) :: _1 )
# 421 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                       ( Int )
# 427 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                     ( Float )
# 433 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                     ( Boolean )
# 439 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                     ( String )
# 445 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                     ( None )
# 451 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                     ( [] )
# 457 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 72 "parser.mly"
                     ( _2 :: _1 )
# 465 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 75 "parser.mly"
               ( (_1, _2) )
# 473 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                   ( [] )
# 479 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "parser.mly"
                   ( _2 :: _1 )
# 487 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
              ( Expr _1 )
# 494 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 83 "parser.mly"
                         ( Return _2 )
# 501 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 84 "parser.mly"
                                            ( Block(List.rev _2)    )
# 508 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 87 "parser.mly"
                                                          ( If(_3, _6, Block([])))
# 516 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 88 "parser.mly"
                                                                     ( If(_3, _6, _10))
# 525 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 89 "parser.mly"
                                                                  ( For(_3, _5, _7, _10) )
# 535 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 90 "parser.mly"
                                                ( While(_3, _6) )
# 543 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                  ( Noexpr )
# 549 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                  ( _1 )
# 556 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 98 "parser.mly"
                       ( Liti(_1) )
# 563 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 99 "parser.mly"
                       ( Litf(_1) )
# 570 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 100 "parser.mly"
                       ( Litb(_1) )
# 577 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "parser.mly"
                       ( Id(_1) )
# 584 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                       ( Binop(_1, Add, _3) )
# 592 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                       ( Binop(_1, Sub, _3) )
# 600 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                       ( Binop(_1, Mul, _3) )
# 608 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 616 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                       ( Binop(_1, Lt, _3) )
# 624 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                       ( Binop(_1, Gt, _3) )
# 632 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                       ( Binop(_1, Exp, _3) )
# 640 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                       ( Binop(_1, Mod, _3) )
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                       ( Binop(_1, Lte, _3) )
# 656 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                       ( Binop(_1, Gte, _3) )
# 664 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                       ( Binop(_1, Eq, _3) )
# 672 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                       ( Binop(_1, Ne, _3) )
# 680 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                       ( Binop(_1, And, _3) )
# 688 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                       ( Binop(_1, Or, _3) )
# 696 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                   ( Assign(_1, _3) )
# 704 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 118 "parser.mly"
                              ( Call(_1, _3)  )
# 712 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                       ( _2 )
# 719 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
                  ( [] )
# 725 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 124 "parser.mly"
               ( List.rev _1 )
# 732 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                            ( [_1] )
# 739 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                         ( _3 :: _1 )
# 747 "parser.ml"
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
