(* Ocamllex scanner for Stark *)

{ open Parser }

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }   (* Whitespace *)
| "/*"                 { multi_comment lexbuf } (* Comments *)
| "//"                 { comment lexbuf } (* Comments *)

| '('           { LPAREN }
| ')'           { RPAREN }
| '['           { LBRACK }
| ']'           { RBRACK }
| '{'           { LBRACE }
| '}'           { RBRACE }
| ';'           { SEMI }
| ','           { COMMA }

| '~'           { TILDE }

| '+'           { PLUS }
| '-'           { MINUS }
| '*'           { TIMES }
| '/'           { DIVIDE }
| '%'           { MOD }

| "<-"          { ASSIGN }
| "incr"        { INCR_ASG }
| "decr"        { DECR_ASG }
| "mult"        { MULT_ASG }
| "divi"        { DIVI_ASG }
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
| "each"        { EACH }
| "in"          { IN }

| "function"    { FUNCTION }
| "return"      { RETURN }

| "define"      { DEFINE }
| "as"          { AS }
| "int"         { INT }
| "bool"        { BOOL }
| "char"        { CHAR }
| "float"       { FLOAT }
| "string"      { STRING }

| "true"        { BLIT(true) }
| "false"       { BLIT(false) }

| "len"         { LEN }

| alpha (digit | alpha | '_')* as lem       { ID(lem) }
| ('-' | '+' )? digit+ as lem               { ILIT(int_of_string lem) }
| '\'' (( ascii | digit ) as lem) '\''      { CLIT(lem) }
| ('-' | '+' )? digit+ ['.'] digit+ as lem  { FLIT(float_of_string lem)}
| '\"' ((ascii | escape)* as lem) '\"'      { SLIT(lem) }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multi_comment = parse
  "*/" { token lexbuf }
| "/*" { raise (Failure("illegal nesting in comments")) }
| _    { multi_comment lexbuf }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }