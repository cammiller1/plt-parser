{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let float = (['0'-'9']*'.'['0'-'9']+|['0'-'9']+'.'['0'-'9']*)

rule tokenize = parse
 [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| digit+ as lit { ILITERAL(int_of_string lit) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*  as var { VARIABLE(var) }
| float as flt { FLITERAL(float_of_string flt) }
| "int"   { INT }
| "float" { FLOAT }
| "bool"  { BOOL }
| "string"{ STRING }
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
| '{'     { LBRAC }
| '}'     { RBRAC }
| "if"    { IF }
| "else"  { ELSE }
| "elif"  { ELIF }
| "while" { WHILE }
| "for"   { FOR }
| "def"   { DEF }
| "return"{ RETURN }
| "##"    { comment lexbuf }
| eof     { EOF }
| _ as char { raise (Failure(“illegal character” ^ Char.escaped char)) } 

and comment = 
 parse "##" { token lexbuf }
 | _        { comment lexbuf }