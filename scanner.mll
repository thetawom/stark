(* Ocamllex scanner for Stark *)

{ open Parser }

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }   (* Whitespace *)
| "/*"                 { comment lexbuf } (* Comments *)

| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| ';'           { SEMI }
| ','           { COMMA }

| '+'           { PLUS }
| '-'           { MINUS }
| '*'           { TIMES }
| '/'           { DIVIDE }
| '%'           { MOD }

| "<-"          { ASSIGN }
| "incr"        { INCR_ASN }
| "decr"        { DECR_ASN }
| "mult"        { MULT_ASN }
| "divi"        { DIVI_ASN }
| "by"          { BY }

| "=="          { EQ }
| "!="          { NEQ }
| '<'           { LT }
| '>'           { GT }
| "<="          { LTE }
| ">="          { GTE }

| "and"         { AND }
| "or"          { OR }
| "not"         { NOT }

| "if"          { IF }
| "else"        { ELSE }
| "while"       { WHILE }
| "for"         { FOR }
| "from"        { FROM }
| "to"          { TO }
| "every"       { EVERY }
| "repeat"      { REPEAT }
| "until"       { UNTIL }

| "function"    { FUNCTION }
| "return"      { RETURN }

| "define"      { DEFINE }
| "as"          { AS }
| "int"         { INT }
| "bool"        { BOOL }
| "char"        { CHAR }
| "float"       { FLOAT }
| "string"      { STRING }

| alpha (digit | alpha | '_')* as lem   { ID(lem) }
| digit+ as lem                         { ILIT(int_of_string lem) }
| "true"                                { BLIT(true) }
| "false"                               { BLIT(false) }
| '\'' (( ascii | digit ) as lem) '\''  { CLIT(lem) }
| digit+ ['.'] digit+ as lem            { FLIT(float_of_string lem)}
| '\"' ((ascii | escape)* as lem) '\"'  { SLIT(lem) }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
