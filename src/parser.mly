/* Ocamlyacc parser for Stark */

%{
open Ast
%}

%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMI COMMA TILDE LEN
%token PLUS MINUS TIMES DIVIDE MOD
%token ASSIGN INCR_ASG DECR_ASG MULT_ASG DIVI_ASG BY
%token EQ NEQ LT GT LTE GTE
%token AND OR NOT
%token IF ELSE WHILE FOR FROM TO EVERY REPEAT UNTIL EACH IN
%token FUNCTION RETURN DEFINE AS
%token INT BOOL CHAR FLOAT STRING
%token <string> ID
%token <int> ILIT
%token <bool> BLIT
%token <char> CLIT
%token <float> FLIT
%token <string> SLIT
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN BY
%left OR
%left AND
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%right AS
%right TILDE

%%

program:
  decls EOF { $1}

decls:
   /* nothing */           { ([], []) }
 | DEFINE vdecl SEMI decls { (($2 :: fst $4), snd $4) }
 | fdecl decls             { (fst $2, ($1 :: snd $2)) }

fdecl:
  FUNCTION ID LPAREN formals_opt RPAREN AS typ LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=$7;
      fname=$2;
      formals=$4;
      locals=$9;
      body=$10
    }
  }

formals_opt:
  | /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  | vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

vdecl_list:
  | /*nothing*/ { [] }
  | DEFINE vdecl SEMI vdecl_list  { $2 :: $4 }

vdecl:
  | ID AS typ { ($3, $1) }

prim_typ : 
  | INT                     { Int  }
  | BOOL                    { Bool }
  | CHAR                    { Char }
  | FLOAT                   { Float }
  | STRING                  { String }

typ:
  | prim_typ { $1 }
  | prim_typ LBRACK ILIT RBRACK  { Array ($1, $3) }

stmt_list:
  | /* nothing */   { [] }
  | stmt stmt_list  { $1 :: $2 }         

stmt:
  | block                                             { $1 }
  | cond                                              { $1 }
  | WHILE expr block                                  { While ($2, $3) }
  | REPEAT block UNTIL expr SEMI                      { RepUntil ($4, $2) }
  | FOR ID FROM expr TO expr EVERY expr block         { For ($2, $4, $6, $8, $9) }  
  | FOR EACH ID IN ID block                           { ForEach ($3, $5, $6) }     
  | ID ASSIGN expr SEMI                               { Assign ($1, $3) }
  | ID LBRACK expr RBRACK ASSIGN expr SEMI            { ArrayW ($1, $3, $6) }
  | asg_op ID BY expr SEMI                            { Assign ($2, Binop (Id $2, $1, $4)) }
  | asg_op ID LBRACK expr RBRACK BY expr SEMI         { ArrayW ($2, $4, Binop (ArrayR ($2, $4), $1, $7)) }
  | TILDE ID ASSIGN expr SEMI                         { Block [ Assign ($2, $4); Expr (Unop (Til, Id $2)) ] }
  | TILDE ID LBRACK expr RBRACK ASSIGN expr SEMI      { Block [ ArrayW ($2, $4, $7); Expr (Unop (Til, ArrayR ($2, $4))) ] }
  | asg_op TILDE ID BY expr SEMI                      { Block [ Assign ($3, Binop (Id $3, $1, $5)); Expr (Unop (Til, Id $3)) ] }
  | asg_op TILDE ID LBRACK expr RBRACK BY expr SEMI   { Block [ ArrayW ($3, $5, Binop (ArrayR ($3, $5), $1, $8)); Expr (Unop (Til, ArrayR ($3, $5))) ] }
  | RETURN expr SEMI                                  { Return $2 }
  | expr SEMI                                         { Expr $1 }

asg_op:
  | INCR_ASG  { Plus }
  | DECR_ASG  { Minus }
  | MULT_ASG  { Times }
  | DIVI_ASG  { Divide }

block:
  | LBRACE stmt_list RBRACE         { Block $2 }

cond:
  | IF expr block ELSE cond         { IfElse ($2, $3, $5) }
  | IF expr block ELSE block        { IfElse ($2, $3, $5) }
  | IF expr block                   { If ($2, $3) }

expr:
  | ILIT                      { IntLit $1 }
  | BLIT                      { BoolLit $1 }
  | CLIT                      { CharLit $1 }
  | FLIT                      { FloatLit $1 }
  | SLIT                      { StringLit $1 }
  | ID                        { Id $1 }
  | ID LBRACK expr RBRACK     { ArrayR ($1, $3) }
  | PLUS expr                 { Unop (Pos, $2) }
  | MINUS expr                { Unop (Neg, $2) }
  | NOT expr                  { Unop (Not, $2) }
  | TILDE expr                { Unop (Til, $2) }
  | expr PLUS expr            { Binop ($1, Plus, $3) }
  | expr MINUS expr           { Binop ($1, Minus, $3) }
  | expr TIMES expr           { Binop ($1, Times, $3) }
  | expr DIVIDE expr          { Binop ($1, Divide, $3) }
  | expr MOD expr             { Binop ($1, Mod, $3) }
  | expr EQ expr              { Binop ($1, Eq, $3) }
  | expr NEQ expr             { Binop ($1, Neq, $3) }
  | expr LT expr              { Binop ($1, Lt, $3) }
  | expr GT expr              { Binop ($1, Gt, $3) }
  | expr LTE expr             { Binop ($1, Lte, $3) }
  | expr GTE expr             { Binop ($1, Gte, $3) }
  | expr AND expr             { Binop ($1, And, $3) }
  | expr OR expr              { Binop ($1, Or, $3) }
  | expr AS typ               { Cast ($3, $1) }
  | LPAREN expr RPAREN        { $2 }
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  | LEN LPAREN ID RPAREN      { Len ($3) }

/* args_opt*/
args_opt:
  | /*nothing*/ { [] }
  | args        { $1 }

args:
  | expr            { [$1] }
  | expr COMMA args { $1::$3 }