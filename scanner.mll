{ open Parser }

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
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z']+ as var { VARIABLE(var) }
| eof { EOF }
