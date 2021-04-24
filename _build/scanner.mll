{ open Parser 
  let remove_quotes str =
  match String.length str with
  | 0 | 1 | 2 -> "" 
  | len -> String.sub str 1 (len - 2)

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let flt = digit*'.'digit+
let string_literal = ('"'[' '-'~']*'"')

rule tokenize = parse
 [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "##"     { comment lexbuf }
| "True"   { BLITERAL (true) }
| "False"  { BLITERAL (false) }
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "void"   { VOID }
| "string" { STRING }
| "array"  { ARRAY }
| ","      { COMMA }
| "=="     { EQ }
| "=<"     { LTE }
| ">="     { GTE }
| "not"	   { NOT }
| "!="     { NE }
| "**"     { EXP }
| "and"    { AND }
| "or"     { OR }
| "in"	   { IN }
| "++"     { PP }
| "--"     { MM }
| '%'      { MOD }
| '>'      { GT }
| '<'      { LT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| ';'      { SEMC }
| '='      { ASSIGN }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      {LSBRAC}
| ']'      {RSBRAC}
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "for"    { FOR }
| "def"    { DEF }
| "return" { RETURN }
| string_literal as lxm { SLITERAL(remove_quotes lxm) }
| digit+ as lxm { ILITERAL(int_of_string lxm) }
| flt as lxm { FLITERAL(lxm) }
| letter['a'-'z' 'A'-'Z' '0'-'9' '_']*  as lxm { ID(lxm) }
| eof     { EOF }
| _ as char { raise (Failure("illegal character" ^ Char.escaped char)) } 

and comment = parse
  "##" { tokenize lexbuf }
| _    { comment lexbuf }