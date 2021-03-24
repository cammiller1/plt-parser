{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let jpie_float = digit*'.'digit+|digit+'.'digit*

rule tokenize = parse
 [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "##"    { comment lexbuf }
| "True"  { BLITERAL (true) }
| "False" { BLITERAL (false) }
| "int"   { INT }
| "float" { FLOAT }
| "bool"  { BOOL }
| "string" { STRING }
| "None"  { NONE }
| ","     { COMMA }
| "=="    { EQ }
| "=<"    { LTE }
| ">="    { GTE }
| "!="    { NE }
| "**"    { EXP }
| "and"   { AND }
| "or"    { OR }
| "++"    { PP }
| "--"    { MM }
| '%'     { MOD }
| '>'     { GT }
| '<'     { LT }
| '+'     { PLUS }
| '-'     { MINUS }
| '*'     { TIMES }
| '/'     { DIVIDE }
| ';'     { SEMC }
| '='     { ASSIGN }
| '('     { LPAREN }
| ')'     { RPAREN }
| '{'     { LBRACE }
| '}'     { RBRACE }
| "if"    { IF }
| "else"  { ELSE }
| "elif"  { ELIF }
| "while" { WHILE }
| "for"   { FOR }
| "def"   { DEF }
| "return" { RETURN }
| digit+ as lxm { ILITERAL(int_of_string lxm) }
| jpie_float as lxm { FLITERAL(float_of_string lxm) }
| letter['a'-'z' 'A'-'Z' '0'-'9' '_']*  as lxm { ID(lxm) }
| eof     { EOF }
| _ as char { raise (Failure("illegal character" ^ Char.escaped char)) } 

and comment = parse
  "##" { tokenize lexbuf }
| _    { comment lexbuf }