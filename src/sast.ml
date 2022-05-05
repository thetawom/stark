(* Semantically-Checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx

and sx =
  | SIntLit of int
  | SBoolLit of bool
  | SCharLit of char
  | SFloatLit of float
  | SStringLit of string
  | SId of string
  | SArrayR of string * sexpr
  | SUnop of uop * sexpr
  | SBinop of sexpr * bop * sexpr
  | SCast of typ * sexpr
  | SCall of string * sexpr list
  | SLen of string

type sstmt =
  | SBlock of sstmt list
  | SIfElse of sexpr * sstmt * sstmt
  | SIf of sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SFor of string * sexpr * sexpr * sexpr * sstmt
  | SForEach of string * string * sstmt
  | SRepUntil of sexpr * sstmt
  | SAssign of string * sexpr
  | SArrayW of string * sexpr * sexpr
  | SExpr of sexpr
  | SReturn of sexpr

type sfunc_def =
  { srtyp: typ
  ; sfname: string
  ; sformals: bind list
  ; slocals: bind list
  ; sbody: sstmt list }

type sprogram = bind list * sfunc_def list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : "
  ^ ( match e with
    | SIntLit l -> string_of_int l
    | SBoolLit true -> "true"
    | SBoolLit false -> "false"
    | SCharLit c -> "'" ^ String.make 1 c ^ "'"
    | SFloatLit f -> string_of_float f
    | SStringLit s -> "\"" ^ s ^ "\""
    | SId s -> s
    | SArrayR (s, e) -> s ^ "[" ^ string_of_sexpr e ^ "]"
    | SUnop (o, e1) -> string_of_uop o ^ string_of_sexpr e1
    | SBinop (e1, o, e2) ->
        "(" ^ string_of_sexpr e1 ^ " " ^ string_of_bop o ^ " "
        ^ string_of_sexpr e2 ^ ")"
    | SCast (t, e) -> string_of_typ t ^ "(" ^ string_of_sexpr e ^ ")"
    | SCall (_, _) -> ""
    | SLen s -> "len (" ^ s ^ ")" )
  ^ ")"

let rec string_of_sstmt = function
  | SBlock stmts ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SIfElse (e, s1, s2) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1 ^ "else\n"
      ^ string_of_sstmt s2
  | SIf (e, s) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SWhile (e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SAssign (v, e) -> v ^ " = " ^ string_of_sexpr e ^ ";\n"
  | SArrayW (v, e1, e2) ->
      v ^ "[" ^ string_of_sexpr e1 ^ "] = " ^ string_of_sexpr e2 ^ ";\n"
  | SFor (v, e1, e2, e3, s) ->
      "for (" ^ v ^ " = " ^ string_of_sexpr e1 ^ "; i <= "
      ^ string_of_sexpr e2 ^ "; i = i + " ^ string_of_sexpr e3 ^ ")\n"
      ^ string_of_sstmt s
  | SForEach (v, arr, s) ->
      " for (" ^ v ^ " : " ^ arr ^ ")\n" ^ string_of_sstmt s
  | SRepUntil (e, s) ->
      "do\n" ^ string_of_sstmt s ^ "while (" ^ string_of_sexpr e ^ ");\n"
  | SExpr expr -> string_of_sexpr expr ^ ";\n"
  | SReturn expr -> "return " ^ string_of_sexpr expr ^ ";\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^ fdecl.sfname ^ "("
  ^ String.concat ", "
      (List.map
         (fun (typ, id) -> string_of_typ typ ^ " " ^ id)
         fdecl.sformals )
  ^ ")\n{\n"
  ^ String.concat "" (List.map string_of_vdecl fdecl.slocals)
  ^ String.concat "" (List.map string_of_sstmt fdecl.sbody)
  ^ "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSemantically checked program: \n\n"
  ^ String.concat "" (List.map string_of_vdecl vars)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_sfdecl funcs)
