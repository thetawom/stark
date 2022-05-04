(* Semantic checking for the Stark compiler *)

open Ast
open Sast
module StringMap = Map.Make (String)

(* Semantic checking of the AST. Returns an SAST if successful, throws an
   exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =
  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
      | [] -> ()
      | (_, n1) :: (_, n2) :: _ when n1 = n2 ->
          raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in
    dups (List.sort (fun (_, a) (_, b) -> compare a b) binds)
  in
  (* Make sure no globals duplicate *)
  check_binds "global" globals ;
  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty) =
      StringMap.add name
        {rtyp= ty; fname= name; formals= [(ty, "x")]; locals= []; body= []}
        map
    in
    List.fold_left add_bind StringMap.empty
      [ ("print", Int)
      ; ("printb", Bool)
      ; ("printf", Float)
      ; ("printc", Char)
      ; ("prints", String) ]
  in
  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *) in
    match fd with
    (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in
  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions in
  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  let _ = find_func "main" in
  (* Ensure "main" is defined *)
  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals ;
    check_binds "local" func.locals ;
    (* Raise an exception if the given rvalue type cannot be assigned to the
       given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in
    (* Build local symbol table of variables for this function *)
    let symbols =
      List.fold_left
        (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty
        (globals @ func.formals @ func.locals)
    in
    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
      | IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | CharLit l -> (Char, SCharLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | StringLit l -> (String, SStringLit l)
      (* | ArrayLit el as e -> let err = "array contains inconsistent types
         in " ^ string_of_expr e in let el' = List.map check_expr el in let
         rec check_array = function | [] -> raise (Failure "invalid array") |
         [(t, _)] -> t | (t, _) :: tl -> if t == check_array tl then t else
         raise (Failure err) in let ty = check_array el' in (Array (ty,
         List.length el), SArrayLit el') *)
      | Id var -> (type_of_identifier var, SId var)
      | ArrayR (var, e1) as e -> (
          let t1, e1' = check_expr e1 in
          let err =
            "illegal array index of type " ^ string_of_typ t1 ^ " in "
            ^ string_of_expr e
          in
          if t1 != Int then raise (Failure err)
          else
            let err ty =
              "illegal indexing of " ^ string_of_typ ty ^ " in "
              ^ string_of_expr e
            in
            match type_of_identifier var with
            | Array (t, _) -> (t, SArrayR (var, (t1, e1')))
            | _ as t -> raise (Failure (err t)) )
      | Unop (op, e1) as e ->
          let t1, e1' = check_expr e1 in
          let err =
            "illegal unary operator " ^ string_of_uop op ^ " "
            ^ string_of_typ t1 ^ " " ^ " in " ^ string_of_expr e
          in
          let t =
            match op with
            | (Pos | Neg) when t1 = Int -> Int
            | (Pos | Neg) when t1 = Float -> Float
            | Not when t1 = Bool -> Bool
            | Til
              when t1 = Int || t1 = Bool || t1 = Float || t1 = Char
                   || t1 = String ->
                t1
            | _ -> raise (Failure err)
          in
          (t, SUnop (op, (t1, e1')))
      | Binop (e1, op, e2) as e ->
          let t1, e1' = check_expr e1 and t2, e2' = check_expr e2 in
          let err =
            "illegal binary operator " ^ string_of_typ t1 ^ " "
            ^ string_of_bop op ^ " " ^ string_of_typ t2 ^ " in "
            ^ string_of_expr e
          in
          (* All binary operators require operands of the same type*)
          if t1 = t2 then
            (* Determine expression type based on operator and operand
               types *)
            let t =
              match op with
              | (Plus | Minus | Times | Divide) when t1 = Int -> Int
              | (Plus | Minus | Times | Divide) when t1 = Float -> Float
              | Mod when t1 = Int -> Int
              | Eq | Neq -> Bool
              | (Lt | Gt | Lte | Gte) when t1 = Int -> Bool
              | (Lt | Gt | Lte | Gte) when t1 = Float -> Bool
              | (And | Or) when t1 = Bool -> Bool
              | _ -> raise (Failure err)
            in
            (t, SBinop ((t1, e1'), op, (t2, e2')))
          else raise (Failure err)
      | Cast (to_ty, e1) as e ->
          let fr_ty, e1' = check_expr e1 in
          if to_ty == fr_ty then (fr_ty, e1')
          else
            let err =
              "illegal cast of " ^ string_of_typ fr_ty ^ " to "
              ^ string_of_typ to_ty ^ " in " ^ string_of_expr e
            in
            let t =
              match fr_ty with
              | (Bool | Char | Float) when to_ty = Int -> Int
              | (Bool | Char | Int) when to_ty = Float -> Float
              | (Bool | Int | Float) when to_ty = Char -> Char
              | (Char | Int) when to_ty = Bool -> Bool
              | _ -> raise (Failure err)
            in
            (t, SCast (t, (fr_ty, e1')))
      | Call (fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting "
                 ^ string_of_int param_length
                 ^ " arguments in " ^ string_of_expr call ) )
          else
            let check_call (ft, _) e =
              let et, e' = check_expr e in
              let err =
                "illegal argument found " ^ string_of_typ et ^ " expected "
                ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in
              (check_assign ft et err, e')
            in
            let args' = List.map2 check_call fd.formals args in
            (fd.rtyp, SCall (fname, args'))
    in
    let check_bool_expr e =
      let t, e' = check_expr e in
      match t with
      | Bool -> (t, e')
      | _ ->
          raise
            (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in
    let rec check_stmt_list = function
      | [] -> []
      | Block sl :: sl' -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt = function
      (* A block is correct if each statement is correct and nothing follows
         any Return statement. Nested blocks are flattened. *)
      | Block sl -> SBlock (check_stmt_list sl)
      | IfElse (e, st1, st2) ->
          SIfElse (check_bool_expr e, check_stmt st1, check_stmt st2)
      | If (e, st1) -> SIf (check_bool_expr e, check_stmt st1)
      | While (e, st) -> SWhile (check_bool_expr e, check_stmt st)
      | For (var, e1, e2, e3, s) ->
          let t = type_of_identifier var
          and t1, e1' = check_expr e1
          and t2, e2' = check_expr e2
          and t3, e3' = check_expr e3 in
          let err =
            "mismatch of types " ^ string_of_typ t ^ ", " ^ string_of_typ t1
            ^ ", " ^ string_of_typ t2 ^ ", and " ^ string_of_typ t3
          in
          let _ =
            if t = t1 && t = t2 && t = t3 then t else raise (Failure err)
          in
          let err =
            "for loop variables must be of type int instead of type "
            ^ string_of_typ t
          in
          let _ = if t == Int then t else raise (Failure err) in
          SFor (var, (t1, e1'), (t2, e2'), (t3, e3'), check_stmt s)
      | RepUntil (e, st) -> SRepUntil (check_bool_expr e, check_stmt st)
      | Assign (var, e) as ex ->
          let lt = type_of_identifier var and rt, e' = check_expr e in
          let err =
            "illegal assignment " ^ string_of_typ lt ^ " = "
            ^ string_of_typ rt ^ " in " ^ string_of_stmt ex
          in
          let _ = check_assign lt rt err in
          SAssign (var, (rt, e'))
      | ArrayW (var, e1, e2) as ex -> (
          let t1, e1' = check_expr e1 and t2, e2' = check_expr e2 in
          let err =
            "illegal array index of type " ^ string_of_typ t1 ^ " in "
            ^ string_of_stmt ex
          in
          if t1 != Int then raise (Failure err)
          else
            let t1 = type_of_identifier var in
            match t1 with
            | Array (t, _) when t == t2 -> SArrayW (var, (t1, e1'), (t2, e2'))
            | _ ->
                let err =
                  "illegal assignment " ^ string_of_typ t1 ^ "[_] = "
                  ^ string_of_typ t2 ^ " in " ^ string_of_stmt ex
                in
                raise (Failure err) )
      | Expr e -> SExpr (check_expr e)
      | Return e ->
          let t, e' = check_expr e in
          if t = func.rtyp then SReturn (t, e')
          else
            raise
              (Failure
                 ( "return gives " ^ string_of_typ t ^ " expected "
                 ^ string_of_typ func.rtyp ^ " in " ^ string_of_expr e ) )
    in
    (* body of check_func *)
    { srtyp= func.rtyp
    ; sfname= func.fname
    ; sformals= func.formals
    ; slocals= func.locals
    ; sbody= check_stmt_list func.body }
  in
  (globals, List.map check_func functions)
