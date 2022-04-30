(* Abstract Syntax Tree and functions for printing it *)

type uop = Pos | Neg | Not | Til

type bop =
  | Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or

type typ = Int | Bool | Char | Float | String

type expr =
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | FloatLit of float
  | StringLit of string
  | Id of string
  | Unop of uop * expr
  | Binop of expr * bop * expr
  | Cast of typ * expr
  | Call of string * expr list

type stmt =
  | Block of stmt list
  | IfElse of expr * stmt * stmt
  | If of expr * stmt
  | While of expr * stmt
  | For of string * expr * expr * expr * stmt
  | RepUntil of expr * stmt
  | Assign of string * expr
  | Expr of expr
  | Return of expr

type bind = typ * string

type func_def =
  { rtyp: typ
  ; fname: string
  ; formals: bind list
  ; locals: bind list
  ; body: stmt list }

type program = bind list * func_def list

(* Pretty-printing functions *)

let string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string"

let string_of_bop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
  | Pos -> ""
  | Neg -> "-"
  | Not -> "!"
  | Til -> "~"

let rec string_of_expr = function
  | IntLit l -> string_of_int l
  | BoolLit true -> "true"
  | BoolLit false -> "false"
  | CharLit c -> "'" ^ String.make 1 c ^ "'"
  | FloatLit f -> string_of_float f
  | StringLit s -> "\"" ^ s ^ "\""
  | Id s -> s
  | Unop (o, e) -> string_of_uop o ^ string_of_expr e
  | Binop (e1, o, e2) ->
      "(" ^ string_of_expr e1 ^ " " ^ string_of_bop o ^ " "
      ^ string_of_expr e2 ^ ")"
  | Cast (t, e) -> string_of_typ t ^ "(" ^ string_of_expr e ^ ")"
  | Call (f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
  | Block stmts ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | IfElse (e, s1, s2) ->
      "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n"
      ^ string_of_stmt s2
  | If (e, s) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | While (e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Assign (v, e) -> v ^ " = " ^ string_of_expr e ^ ";\n"
  | For (v, e1, e2, e3, s) ->
      "for (" ^ v ^ " = " ^ string_of_expr e1 ^ "; i <= " ^ string_of_expr e2
      ^ "; i = i + " ^ string_of_expr e3 ^ ")\n" ^ string_of_stmt s
  | RepUntil (e, s) ->
      "do\n" ^ string_of_stmt s ^ "while (" ^ string_of_expr e ^ ");\n"
  | Expr expr -> string_of_expr expr ^ ";\n"
  | Return expr -> "return " ^ string_of_expr expr ^ ";\n"

let string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^ fdecl.fname ^ "("
  ^ String.concat ", "
      (List.map
         (fun (typ, id) -> string_of_typ typ ^ " " ^ id)
         fdecl.formals )
  ^ ")\n{\n"
  ^ String.concat "" (List.map string_of_vdecl fdecl.locals)
  ^ String.concat "" (List.map string_of_stmt fdecl.body)
  ^ "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_fdecl funcs)
