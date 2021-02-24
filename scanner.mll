{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let float = digit*'.'digit+

rule tokenize = parse
| [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| | digit+ as lit { ILITERAL(int_of_string lit) }
| ['a'-'z']+ as var { VARIABLE(var) }
| float as flt { FLITERAL(float_of_string flt) }
| "int"   { INT }
| "float" { FLOAT }
| "bool"  { BOOL }
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
| '='     { ASSI }
| "if"    { IF }
| "else"  { ELSE }
| "elif"  { ELIF }
| "while" { WHILE }


| "/*"    { comment lexbuf }
| eof     { EOF }

and comment = 
 parse "*/" { token lexbuf }
 | _        { comment lexbuf }