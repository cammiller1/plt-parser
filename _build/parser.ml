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
\003\000\008\000\008\000\008\000\008\000\010\000\010\000\010\000\
\010\000\010\000\010\000\012\000\012\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\010\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\000\000\002\000\
\003\000\000\000\002\000\000\000\002\000\002\000\003\000\007\000\
\011\000\011\000\007\000\000\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\004\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\056\000\000\000\000\000\010\000\011\000\012\000\
\013\000\014\000\001\000\003\000\004\000\000\000\000\000\000\000\
\000\000\017\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\015\000\000\000\000\000\009\000\016\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\030\000\031\000\
\000\000\032\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\023\000\
\051\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\037\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\040\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\026\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\021\000\028\000\031\000\
\022\000\043\000\044\000\050\000\076\000\077\000"

let yysindex = "\009\000\
\000\000\000\000\000\000\001\000\118\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\255\254\000\255\023\255\
\018\255\000\000\118\255\011\255\021\255\024\255\000\000\063\255\
\118\255\000\000\020\255\118\255\000\000\000\000\044\255\014\255\
\051\255\070\255\073\255\014\255\014\255\000\000\000\000\000\000\
\010\255\000\000\000\000\132\000\177\000\014\255\014\255\014\255\
\177\000\078\255\199\255\014\255\014\255\014\255\014\255\014\255\
\014\255\014\255\014\255\014\255\014\255\014\255\014\255\014\255\
\014\255\014\255\014\255\000\000\225\255\251\255\147\000\000\000\
\000\000\177\000\177\000\079\255\077\255\254\254\254\254\000\000\
\000\000\004\255\004\255\004\255\004\255\205\000\205\000\191\000\
\191\000\000\000\000\000\071\255\081\255\014\255\000\000\014\255\
\094\255\094\255\162\000\177\000\087\255\090\255\014\255\101\255\
\000\000\035\000\097\255\098\255\094\255\094\255\100\255\126\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\102\255\000\000\000\000\128\255\000\000\000\000\
\000\000\000\000\000\000\065\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\140\255\000\000\000\000\000\000\000\000\
\132\255\000\000\000\000\000\000\029\255\000\000\000\000\000\000\
\141\255\000\000\000\000\000\000\131\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\049\255\048\255\000\000\136\255\158\255\170\255\000\000\
\000\000\045\000\057\000\070\000\082\000\091\000\103\000\030\255\
\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\068\255\000\000\000\000\000\000\086\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\156\000\000\000\036\000\000\000\000\000\000\000\
\000\000\232\255\224\255\000\000\000\000\000\000"

let yytablesize = 476
let yytable = "\045\000\
\011\000\056\000\057\000\049\000\051\000\054\000\055\000\056\000\
\057\000\001\000\052\000\066\000\067\000\069\000\070\000\071\000\
\032\000\066\000\067\000\074\000\075\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\091\000\053\000\016\000\017\000\018\000\037\000\
\015\000\046\000\046\000\019\000\048\000\046\000\032\000\024\000\
\023\000\039\000\040\000\041\000\042\000\025\000\020\000\048\000\
\046\000\029\000\048\000\046\000\027\000\099\000\033\000\100\000\
\049\000\034\000\035\000\018\000\036\000\037\000\106\000\038\000\
\101\000\102\000\054\000\049\000\046\000\054\000\049\000\039\000\
\040\000\041\000\042\000\018\000\111\000\112\000\018\000\018\000\
\024\000\018\000\018\000\026\000\018\000\072\000\055\000\047\000\
\032\000\055\000\048\000\097\000\018\000\018\000\018\000\018\000\
\024\000\095\000\096\000\024\000\024\000\098\000\024\000\024\000\
\033\000\024\000\104\000\034\000\035\000\105\000\036\000\037\000\
\107\000\024\000\024\000\024\000\024\000\109\000\110\000\113\000\
\006\000\039\000\040\000\041\000\042\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\006\000\007\000\008\000\009\000\
\010\000\114\000\007\000\028\000\029\000\052\000\033\000\034\000\
\034\000\033\000\053\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\035\000\035\000\034\000\000\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\035\000\030\000\
\034\000\035\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\000\000\000\000\035\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\073\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\092\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\047\000\047\000\000\000\000\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\093\000\000\000\000\000\
\005\000\047\000\000\000\000\000\047\000\000\000\000\000\006\000\
\007\000\008\000\009\000\010\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\000\000\000\000\038\000\108\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\000\000\038\000\
\039\000\000\000\038\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\039\000\000\000\042\000\039\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\000\000\
\042\000\043\000\000\000\042\000\044\000\044\000\044\000\044\000\
\000\000\000\000\044\000\000\000\043\000\000\000\000\000\043\000\
\045\000\045\000\045\000\045\000\000\000\044\000\045\000\000\000\
\044\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\000\000\000\000\045\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\094\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\103\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\000\000\000\000\066\000\067\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\000\000\000\000\
\000\000\000\000\066\000\067\000"

let yycheck = "\032\000\
\000\000\004\001\005\001\036\000\037\000\002\001\003\001\004\001\
\005\001\001\000\001\001\014\001\015\001\046\000\047\000\048\000\
\003\001\014\001\015\001\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\026\001\038\001\038\001\016\001\026\001\
\005\000\012\001\013\001\026\001\016\001\016\001\003\001\027\001\
\038\001\036\001\037\001\038\001\039\001\030\001\019\000\027\001\
\027\001\038\001\030\001\030\001\025\000\094\000\019\001\096\000\
\016\001\022\001\023\001\003\001\025\001\026\001\103\000\028\001\
\097\000\098\000\027\001\027\001\026\001\030\001\030\001\036\001\
\037\001\038\001\039\001\019\001\109\000\110\000\022\001\023\001\
\003\001\025\001\026\001\029\001\028\001\016\001\027\001\026\001\
\003\001\030\001\026\001\029\001\036\001\037\001\038\001\039\001\
\019\001\027\001\030\001\022\001\023\001\029\001\025\001\026\001\
\019\001\028\001\028\001\022\001\023\001\028\001\025\001\026\001\
\020\001\036\001\037\001\038\001\039\001\029\001\029\001\028\001\
\027\001\036\001\037\001\038\001\039\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\031\001\032\001\033\001\034\001\
\035\001\028\001\027\001\016\001\016\001\027\001\027\001\002\001\
\003\001\030\001\027\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\002\001\003\001\016\001\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\028\000\
\027\001\016\001\255\255\030\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\027\001\255\255\255\255\030\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\027\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\012\001\013\001\255\255\255\255\016\001\255\255\
\255\255\255\255\255\255\255\255\255\255\027\001\255\255\255\255\
\024\001\027\001\255\255\255\255\030\001\255\255\255\255\031\001\
\032\001\033\001\034\001\035\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\255\255\255\255\016\001\027\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\027\001\
\016\001\255\255\030\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\027\001\255\255\016\001\030\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\027\001\016\001\255\255\030\001\010\001\011\001\012\001\013\001\
\255\255\255\255\016\001\255\255\027\001\255\255\255\255\030\001\
\010\001\011\001\012\001\013\001\255\255\027\001\016\001\255\255\
\030\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\004\001\005\001\006\001\007\001\008\001\009\001\255\255\255\255\
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
# 350 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                 ( ([], [])               )
# 356 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 40 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 364 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 41 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 372 "parser.ml"
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
# 387 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                  ( [] )
# 393 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 53 "parser.mly"
                ( _1 )
# 400 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
         ( [(_1,_2)] )
# 408 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                             ( (_3,_4) :: _1 )
# 417 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                       ( Int )
# 423 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                     ( Float )
# 429 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                     ( Boolean )
# 435 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                     ( String )
# 441 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                     ( None )
# 447 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                     ( [] )
# 453 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 72 "parser.mly"
                     ( _2 :: _1 )
# 461 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 75 "parser.mly"
               ( (_1, _2) )
# 469 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                   ( [] )
# 475 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "parser.mly"
                   ( _2 :: _1 )
# 483 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                  ( [] )
# 489 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
                   ( _2 :: _1 )
# 497 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
              ( Expr _1 )
# 504 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 87 "parser.mly"
                         ( Return _2 )
# 511 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 90 "parser.mly"
                                                          ( If(_3, _6, Block([])))
# 519 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 91 "parser.mly"
                                                                     ( If(_3, _6, _10))
# 528 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 92 "parser.mly"
                                                                  ( For(_3, _5, _7, _10) )
# 538 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 93 "parser.mly"
                                                ( While(_3, _6) )
# 546 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                  ( Noexpr )
# 552 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                  ( _1 )
# 559 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 101 "parser.mly"
                       ( Liti(_1) )
# 566 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 102 "parser.mly"
                       ( Litf(_1) )
# 573 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 103 "parser.mly"
                       ( Litb(_1) )
# 580 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
                       ( Id(_1) )
# 587 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                       ( Binop(_1, Add, _3) )
# 595 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                       ( Binop(_1, Sub, _3) )
# 603 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                       ( Binop(_1, Mul, _3) )
# 611 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 619 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                       ( Binop(_1, Lt, _3) )
# 627 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                       ( Binop(_1, Gt, _3) )
# 635 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                       ( Binop(_1, Exp, _3) )
# 643 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                       ( Binop(_1, Mod, _3) )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                       ( Binop(_1, Lte, _3) )
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                       ( Binop(_1, Gte, _3) )
# 667 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                       ( Binop(_1, Eq, _3) )
# 675 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                       ( Binop(_1, Ne, _3) )
# 683 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                       ( Binop(_1, And, _3) )
# 691 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                       ( Binop(_1, Or, _3) )
# 699 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                            ( Uniop(Neg, _2) )
# 706 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                   ( Assign(_1, _3) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 121 "parser.mly"
                              ( Call(_1, _3)  )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                       ( _2 )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
                  ( [] )
# 735 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 127 "parser.mly"
               ( List.rev _1 )
# 742 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                            ( [_1] )
# 749 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                         ( _3 :: _1 )
# 757 "parser.ml"
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
