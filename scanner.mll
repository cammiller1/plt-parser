{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let float = digit*'.'digit+

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "==" { EQ }
| "=<" { LTE }
| ">=" { GTE }
| "!=" { NE }
| "**" { EXP }
| "and" { AND }
| "or" { OR }
| "++" { PP }
| "--" { MM }
| '%' { MOD }
| '>' { GT }
| '<' { LT }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ';' { SEQ }
| '=' { ASSI }
| digit+ as lit { ILITERAL(int_of_string lit) }
| ['a'-'z']+ as var { VARIABLE(var) }
| float as flt { FLITERAL(float_of_string flt) }
| eof { EOF }
