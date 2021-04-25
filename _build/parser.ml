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
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\014\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\000\000\002\000\004\000\000\000\
\002\000\000\000\002\000\010\000\000\000\001\000\002\000\004\000\
\001\000\001\000\001\000\001\000\001\000\004\000\000\000\001\000\
\001\000\001\000\001\000\001\000\004\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\003\000\002\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\061\000\000\000\025\000\026\000\027\000\028\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\033\000\034\000\000\000\036\000\035\000\001\000\003\000\
\004\000\005\000\000\000\000\000\056\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000\055\000\
\008\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\000\042\000\045\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\054\000\000\000\000\000\000\000\
\015\000\000\000\012\000\000\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\000\000\000\000\010\000\000\000\
\018\000\000\000\000\000\000\000\024\000\011\000\019\000\000\000\
\000\000\020\000"

let yydgoto = "\002\000\
\003\000\004\000\024\000\025\000\066\000\027\000\036\000\038\000\
\030\000\087\000\116\000\103\000\104\000\069\000\070\000"

let yysindex = "\004\000\
\000\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\000\000\200\255\241\254\242\254\003\255\030\255\200\255\200\255\
\000\000\000\000\000\000\252\254\000\000\000\000\000\000\000\000\
\000\000\000\000\246\000\047\255\000\000\244\254\200\255\200\255\
\200\255\054\255\041\001\011\255\134\000\106\255\200\255\200\255\
\200\255\200\255\200\255\200\255\200\255\000\000\200\255\200\255\
\200\255\200\255\200\255\200\255\200\255\200\255\200\255\002\255\
\036\255\153\000\172\000\007\001\013\255\004\255\000\000\000\000\
\000\000\000\000\041\001\041\001\029\255\027\255\253\254\253\254\
\000\000\000\000\000\000\160\255\160\255\160\255\160\255\075\001\
\075\001\058\001\058\001\000\000\032\255\200\255\059\255\194\255\
\194\255\200\255\043\255\030\255\000\000\200\255\000\000\041\001\
\000\000\055\255\000\000\024\001\000\000\060\255\050\255\053\255\
\041\001\194\255\200\255\000\000\069\255\030\255\000\000\191\000\
\000\000\090\255\194\255\030\255\000\000\000\000\000\000\150\255\
\092\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\104\255\000\000\
\000\000\000\000\000\000\082\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\114\255\000\000\000\000\000\000\000\000\095\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\130\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\255\066\255\000\000\110\255\046\000\104\000\
\000\000\000\000\000\000\103\255\144\255\195\255\232\255\247\254\
\206\000\249\254\234\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\111\255\000\000\000\000\057\255\143\255\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\124\255\
\081\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\209\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\043\000\000\000\254\255\007\000\000\000\057\000\
\252\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 611
let yytable = "\028\000\
\009\000\026\000\039\000\049\000\001\000\051\000\043\000\044\000\
\045\000\034\000\049\000\049\000\049\000\049\000\051\000\051\000\
\029\000\031\000\032\000\053\000\055\000\035\000\037\000\063\000\
\049\000\056\000\051\000\049\000\040\000\051\000\005\000\006\000\
\007\000\008\000\009\000\033\000\092\000\058\000\059\000\060\000\
\053\000\085\000\086\000\053\000\023\000\067\000\068\000\071\000\
\072\000\073\000\074\000\075\000\091\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\093\000\094\000\
\037\000\037\000\037\000\037\000\037\000\037\000\095\000\097\000\
\037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\101\000\106\000\109\000\056\000\098\000\099\000\102\000\
\057\000\110\000\037\000\061\000\096\000\037\000\030\000\062\000\
\100\000\061\000\030\000\059\000\105\000\108\000\059\000\111\000\
\113\000\114\000\005\000\006\000\007\000\008\000\009\000\121\000\
\118\000\112\000\060\000\043\000\031\000\060\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\032\000\061\000\
\057\000\061\000\010\000\117\000\011\000\057\000\012\000\013\000\
\043\000\015\000\016\000\043\000\065\000\017\000\016\000\058\000\
\021\000\018\000\019\000\020\000\021\000\022\000\005\000\006\000\
\007\000\008\000\009\000\017\000\044\000\022\000\119\000\044\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\041\000\
\042\000\043\000\044\000\045\000\120\000\000\000\010\000\000\000\
\011\000\044\000\012\000\013\000\044\000\015\000\016\000\055\000\
\122\000\017\000\000\000\000\000\000\000\018\000\019\000\020\000\
\021\000\022\000\005\000\006\000\007\000\008\000\009\000\000\000\
\005\000\006\000\007\000\008\000\009\000\000\000\000\000\047\000\
\000\000\000\000\047\000\047\000\047\000\047\000\047\000\047\000\
\047\000\047\000\010\000\000\000\011\000\000\000\012\000\013\000\
\010\000\015\000\016\000\000\000\047\000\017\000\000\000\047\000\
\016\000\018\000\019\000\020\000\021\000\022\000\000\000\018\000\
\019\000\020\000\021\000\022\000\048\000\000\000\052\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\052\000\
\052\000\009\000\009\000\009\000\009\000\009\000\000\000\000\000\
\000\000\048\000\000\000\052\000\048\000\000\000\052\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\000\000\009\000\000\000\009\000\009\000\009\000\
\009\000\009\000\000\000\009\000\009\000\000\000\000\000\000\000\
\009\000\009\000\009\000\009\000\009\000\005\000\006\000\007\000\
\008\000\009\000\000\000\000\000\000\000\039\000\039\000\000\000\
\000\000\000\000\039\000\000\000\000\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\010\000\000\000\011\000\
\000\000\012\000\013\000\014\000\015\000\016\000\000\000\039\000\
\017\000\000\000\039\000\000\000\018\000\019\000\020\000\021\000\
\022\000\038\000\038\000\038\000\038\000\038\000\038\000\000\000\
\000\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\000\000\000\000\000\000\000\000\000\000\040\000\
\040\000\000\000\000\000\038\000\040\000\000\000\038\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\040\000\000\000\000\000\040\000\041\000\042\000\043\000\
\044\000\045\000\000\000\000\000\000\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\000\000\000\000\
\041\000\042\000\043\000\044\000\045\000\000\000\000\000\064\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\000\000\000\000\041\000\042\000\043\000\044\000\045\000\
\000\000\000\000\088\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\000\000\000\000\041\000\042\000\
\043\000\044\000\045\000\000\000\000\000\089\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\000\000\
\000\000\000\000\050\000\000\000\000\000\000\000\000\000\000\000\
\115\000\050\000\050\000\050\000\050\000\000\000\000\000\000\000\
\000\000\013\000\000\000\013\000\000\000\013\000\013\000\050\000\
\013\000\013\000\050\000\013\000\013\000\000\000\000\000\000\000\
\013\000\013\000\013\000\013\000\013\000\041\000\042\000\043\000\
\044\000\045\000\046\000\000\000\000\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\041\000\042\000\
\043\000\044\000\045\000\090\000\000\000\000\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\041\000\
\042\000\043\000\044\000\045\000\107\000\000\000\000\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\041\000\042\000\043\000\044\000\045\000\000\000\000\000\000\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\041\000\042\000\043\000\044\000\045\000\000\000\000\000\
\000\000\047\000\048\000\049\000\050\000\051\000\052\000\000\000\
\000\000\055\000\041\000\042\000\043\000\044\000\045\000\000\000\
\000\000\000\000\047\000\048\000\049\000\050\000\000\000\000\000\
\000\000\000\000\055\000"

let yycheck = "\004\000\
\000\000\004\000\007\001\013\001\001\000\013\001\010\001\011\001\
\012\001\014\000\020\001\021\001\022\001\023\001\022\001\023\001\
\010\000\033\001\033\001\013\001\024\001\015\000\016\000\013\001\
\034\001\038\001\034\001\037\001\033\001\037\001\001\001\002\001\
\003\001\004\001\005\001\033\001\033\001\031\000\032\000\033\000\
\034\001\040\001\007\001\037\001\000\000\039\000\040\000\041\000\
\042\000\043\000\044\000\045\000\040\001\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\034\001\037\001\
\008\001\009\001\010\001\011\001\012\001\013\001\039\001\013\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\039\001\028\001\034\001\038\001\088\000\089\000\092\000\
\042\001\037\001\034\001\038\001\086\000\037\001\038\001\042\001\
\090\000\038\001\042\001\034\001\094\000\042\001\037\001\106\000\
\036\001\110\000\001\001\002\001\003\001\004\001\005\001\116\000\
\115\000\107\000\034\001\013\001\013\001\037\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\013\001\038\001\
\034\001\038\001\025\001\042\001\027\001\042\001\029\001\030\001\
\034\001\032\001\033\001\037\001\035\001\036\001\013\001\034\001\
\034\001\040\001\041\001\042\001\043\001\044\001\001\001\002\001\
\003\001\004\001\005\001\013\001\013\001\034\001\116\000\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\008\001\
\009\001\010\001\011\001\012\001\116\000\255\255\025\001\255\255\
\027\001\034\001\029\001\030\001\037\001\032\001\033\001\024\001\
\035\001\036\001\255\255\255\255\255\255\040\001\041\001\042\001\
\043\001\044\001\001\001\002\001\003\001\004\001\005\001\255\255\
\001\001\002\001\003\001\004\001\005\001\255\255\255\255\013\001\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\025\001\255\255\027\001\255\255\029\001\030\001\
\025\001\032\001\033\001\255\255\034\001\036\001\255\255\037\001\
\033\001\040\001\041\001\042\001\043\001\044\001\255\255\040\001\
\041\001\042\001\043\001\044\001\013\001\255\255\013\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\022\001\
\023\001\001\001\002\001\003\001\004\001\005\001\255\255\255\255\
\255\255\034\001\255\255\034\001\037\001\255\255\037\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\025\001\255\255\027\001\255\255\029\001\030\001\031\001\
\032\001\033\001\255\255\035\001\036\001\255\255\255\255\255\255\
\040\001\041\001\042\001\043\001\044\001\001\001\002\001\003\001\
\004\001\005\001\255\255\255\255\255\255\008\001\009\001\255\255\
\255\255\255\255\013\001\255\255\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\025\001\255\255\027\001\
\255\255\029\001\030\001\031\001\032\001\033\001\255\255\034\001\
\036\001\255\255\037\001\255\255\040\001\041\001\042\001\043\001\
\044\001\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\255\255\255\255\255\255\255\255\255\255\008\001\
\009\001\255\255\255\255\034\001\013\001\255\255\037\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\034\001\255\255\255\255\037\001\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\255\255\255\255\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\034\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\255\255\255\255\008\001\009\001\010\001\011\001\012\001\
\255\255\255\255\034\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\255\255\255\255\008\001\009\001\
\010\001\011\001\012\001\255\255\255\255\034\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\255\255\
\255\255\255\255\013\001\255\255\255\255\255\255\255\255\255\255\
\034\001\020\001\021\001\022\001\023\001\255\255\255\255\255\255\
\255\255\025\001\255\255\027\001\255\255\029\001\030\001\034\001\
\032\001\033\001\037\001\035\001\036\001\255\255\255\255\255\255\
\040\001\041\001\042\001\043\001\044\001\008\001\009\001\010\001\
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
\255\255\255\255\016\001\017\001\018\001\019\001\255\255\255\255\
\255\255\255\255\024\001"

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
# 409 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
                 ( ([], [], [])             )
# 415 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 57 "parser.mly"
                 ( ((_2 :: fst _1), snd _1, trd _1) )
# 423 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 58 "parser.mly"
                 ( (fst _1, (_2 :: snd _1), trd _1) )
# 431 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'highest) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 59 "parser.mly"
                ( (fst _1, snd _1, (trd _1 @ [_2]))  )
# 439 "parser.ml"
               : 'highest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
              ( Expr _1 )
# 446 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 65 "parser.mly"
                         ( Return _2 )
# 453 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 66 "parser.mly"
                                            ( Block(List.rev _2)    )
# 460 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 468 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 68 "parser.mly"
                                         ( If(_3, _5, _7))
# 477 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
                                                    ( For(_3, _5, _7, _9) )
# 487 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                                  ( While(_3, _5) )
# 495 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                   ( [] )
# 501 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 75 "parser.mly"
                   ( _2 :: _1 )
# 509 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_assign) in
    Obj.repr(
# 79 "parser.mly"
                             ( (_1, _2, _3) )
# 518 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
  ( Noexpr )
# 524 "parser.ml"
               : 'vdecl_assign))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                ( _2 )
# 531 "parser.ml"
               : 'vdecl_assign))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                     ( [] )
# 537 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 88 "parser.mly"
                     ( _2 :: _1 )
# 545 "parser.ml"
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
# 560 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                  ( [] )
# 566 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 101 "parser.mly"
                ( _1 )
# 573 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
         ( [(_1,_2, Noexpr)] )
# 581 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
                             ( (_3,_4, Noexpr) :: _1 )
# 590 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                     ( Int )
# 596 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
                     ( Float )
# 602 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                     ( Boolean )
# 608 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                     ( String )
# 614 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                     ( Void )
# 620 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 115 "parser.mly"
                                 ( Array(_1,_3) )
# 628 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
                  ( Noexpr )
# 634 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                  ( _1 )
# 641 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 126 "parser.mly"
                       ( Liti(_1) )
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 127 "parser.mly"
                       ( Litf(_1) )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 128 "parser.mly"
                       ( Litb(_1) )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser.mly"
                       ( Lits(_1) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 130 "parser.mly"
                               (Arrayinit(_1,_3))
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parser.mly"
                       ( Id(_1) )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                       ( Binop(_1, Add, _3) )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                       ( Binop(_1, Sub, _3) )
# 700 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                       ( Binop(_1, Mul, _3) )
# 708 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 716 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                       ( Binop(_1, Lt, _3) )
# 724 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                       ( Binop(_1, Gt, _3) )
# 732 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                       ( Binop(_1, Exp, _3) )
# 740 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                       ( Binop(_1, Mod, _3) )
# 748 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                       ( Binop(_1, Lte, _3) )
# 756 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                       ( Binop(_1, Gte, _3) )
# 764 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                       ( Binop(_1, Eq, _3) )
# 772 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                       ( Binop(_1, Ne, _3) )
# 780 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                       ( Binop(_1, And, _3) )
# 788 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                       ( Binop(_1, Or, _3) )
# 796 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                       ( Assign(_1, _3) )
# 804 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 147 "parser.mly"
                              ( Call(_1, _3)  )
# 812 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                       ( _2 )
# 819 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                       ( Uniop(Not, _2) )
# 826 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
                  ( [] )
# 832 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 153 "parser.mly"
               ( List.rev _1 )
# 839 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
                            ( [_1] )
# 846 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
                         ( _3 :: _1 )
# 854 "parser.ml"
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
