module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context = L.global_context () in
  (* Create the LLVM compilation module into which we will generate code *)
  let the_module = L.create_module context "Stark" in
  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.float_type context
  and double_t = L.double_type context
  and arr_t ltype = L.pointer_type ltype in
  (* Return the LLVM type for a Stark type *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Char -> i8_t
    | A.Float -> float_t
    | A.String -> L.pointer_type i8_t
    | A.Array (ty, _) -> arr_t (ltype_of_typ ty)
  in
  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0 in
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty globals
  in
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [|L.pointer_type i8_t|]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in
  (* Define each function (arguments and return type) so we can call it even
     before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list
          (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions
  in
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let the_function, _ = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in
    let char_format_str = L.build_global_stringptr "%c\n" "fmt" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    (* Construct the function's "locals": formal arguments and locally
       declared variables. Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals"
       map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p ;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder) ;
        StringMap.add n local m
      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let ltype = ltype_of_typ t in
        let local_var =
          ( match t with
          | A.Array (_, sz) ->
              L.build_array_alloca (L.element_type ltype)
                (L.const_int i32_t sz)
          | _ -> L.build_alloca ltype )
            n builder
        in
        StringMap.add n local_var m
      in
      let formals =
        List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function))
      in
      List.fold_left add_local formals fdecl.slocals
    in
    (* Return the value for a variable or formal argument. Check local names
       first, then global names *)
    let lookup n =
      try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in
    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) =
      match e with
      | SIntLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit f -> L.const_float float_t f
      | SCharLit c -> L.const_int i8_t (Char.code c)
      | SStringLit s -> L.build_global_stringptr s "str" builder
      (* | SArrayLit el -> let el' = List.map (build_expr builder) el in let
         arr = L.build_array_alloca (L.type_of (List.hd el')) (L.const_int
         i32_t (List.length el')) "" builder in ignore (let store p e' = let
         _ = L.build_store e' p builder in L.build_in_bounds_gep p
         [|L.const_int i32_t 1|] "" builder in List.fold_left store arr el' )
         ; arr *)
      | SId s -> L.build_load (lookup s) s builder
      | SArrayR (s, e) ->
          let e' = build_expr builder e in
          L.build_load
            (L.build_in_bounds_gep (lookup s) [|e'|] "" builder)
            "" builder
      | SUnop (op, e) -> (
          let e' = build_expr builder e in
          match op with
          | A.Pos -> e'
          | A.Neg ->
              (if L.type_of e' = i32_t then L.build_neg else L.build_fneg)
                e' "tmp" builder
          | A.Not -> L.build_not e' "tmp" builder
          | A.Til ->
              let ty = L.type_of e' in
              ignore
                (L.build_call printf_func
                   ( if ty == float_t then
                     [| float_format_str
                      ; L.build_fpext (build_expr builder e) double_t ""
                          builder |]
                   else if ty == i8_t then
                     [|char_format_str; build_expr builder e|]
                   else if ty == L.pointer_type i8_t then
                     [|string_format_str; build_expr builder e|]
                   else [|int_format_str; e'|] )
                   "printf" builder ) ;
              e' )
      | SBinop (e1, op, e2) ->
          let e1' = build_expr builder e1 and e2' = build_expr builder e2 in
          ( match op with
          | A.Plus ->
              if L.type_of e1' = i32_t then L.build_add else L.build_fadd
          | A.Minus ->
              if L.type_of e1' = i32_t then L.build_sub else L.build_fsub
          | A.Times ->
              if L.type_of e1' = i32_t then L.build_mul else L.build_fmul
          | A.Divide ->
              if L.type_of e1' = i32_t then L.build_sdiv else L.build_fdiv
          | A.Mod -> L.build_srem
          | A.Eq -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | A.Lt -> L.build_icmp L.Icmp.Slt
          | A.Gt -> L.build_icmp L.Icmp.Sgt
          | A.Lte -> L.build_icmp L.Icmp.Sle
          | A.Gte -> L.build_icmp L.Icmp.Sge
          | A.And -> L.build_and
          | A.Or -> L.build_or )
            e1' e2' "tmp" builder
      | SCast (t1, e) -> (
          let e' = build_expr builder e in
          let t2 = L.type_of e' in
          match t1 with
          | A.Int ->
              ( if t2 == float_t then L.build_fptosi
              else if t2 == i8_t then L.build_sext
              else L.build_zext )
                e' i32_t "tmp" builder
          | A.Float ->
              (if t2 == i1_t then L.build_uitofp else L.build_sitofp)
                e' float_t "tmp" builder
          | A.Char ->
              ( if t2 == float_t then L.build_fptosi
              else if t2 == i32_t then L.build_trunc
              else L.build_zext )
                e' i8_t "tmp" builder
          | A.Bool -> L.build_trunc e' i1_t "tmp" builder
          | _ -> e' )
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
          L.build_call printf_func
            [|int_format_str; build_expr builder e|]
            "printf" builder
      | SCall ("printf", [e]) ->
          L.build_call printf_func
            [| float_format_str
             ; L.build_fpext (build_expr builder e) double_t "" builder |]
            "printf" builder
      | SCall ("printc", [e]) ->
          L.build_call printf_func
            [|char_format_str; build_expr builder e|]
            "printf" builder
      | SCall ("prints", [e]) ->
          L.build_call printf_func
            [|string_format_str; build_expr builder e|]
            "printf" builder
      | SCall (f, args) ->
          let fdef, _ = StringMap.find f function_decls in
          let llargs =
            List.rev (List.map (build_expr builder) (List.rev args))
          in
          let result = f ^ "_result" in
          L.build_call fdef (Array.of_list llargs) result builder
    in
    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control. This function runs "instr builder"
       if the current block does not already have a terminator. Used, e.g.,
       to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in
    (* Build the code for the given statement; return the builder for the
       statement's successor (i.e., the next instruction will be built after
       the one generated by this call) *)
    let rec build_stmt builder = function
      | SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e ->
          ignore (build_expr builder e) ;
          builder
      | SReturn e ->
          ignore (L.build_ret (build_expr builder e) builder) ;
          builder
      | SAssign (s, e) ->
          let e' = build_expr builder e in
          ignore (L.build_store e' (lookup s) builder) ;
          builder
      | SArrayW (s, e1, e2) ->
          let e1' = build_expr builder e1 and e2' = build_expr builder e2 in
          let p = L.build_in_bounds_gep (lookup s) [|e1'|] "" builder in
          ignore (L.build_store e2' p builder) ;
          builder
      | SIf (predicate, then_stmt) ->
          let bool_val = build_expr builder predicate in
          let then_bb = L.append_block context "then" the_function in
          ignore (build_stmt (L.builder_at_end context then_bb) then_stmt) ;
          let end_bb = L.append_block context "if_end" the_function in
          let build_br_end = L.build_br end_bb in
          (* partial function *)
          add_terminal (L.builder_at_end context then_bb) build_br_end ;
          ignore (L.build_cond_br bool_val then_bb end_bb builder) ;
          L.builder_at_end context end_bb
      | SIfElse (predicate, then_stmt, else_stmt) ->
          let bool_val = build_expr builder predicate in
          let then_bb = L.append_block context "then" the_function in
          ignore (build_stmt (L.builder_at_end context then_bb) then_stmt) ;
          let else_bb = L.append_block context "else" the_function in
          ignore (build_stmt (L.builder_at_end context else_bb) else_stmt) ;
          let end_bb = L.append_block context "if_end" the_function in
          let build_br_end = L.build_br end_bb in
          (* partial function *)
          add_terminal (L.builder_at_end context then_bb) build_br_end ;
          add_terminal (L.builder_at_end context else_bb) build_br_end ;
          ignore (L.build_cond_br bool_val then_bb else_bb builder) ;
          L.builder_at_end context end_bb
      | SWhile (predicate, body) ->
          let while_bb = L.append_block context "while" the_function in
          let build_br_while = L.build_br while_bb in
          (* partial function *)
          ignore (build_br_while builder) ;
          let while_builder = L.builder_at_end context while_bb in
          let bool_val = build_expr while_builder predicate in
          let body_bb = L.append_block context "while_body" the_function in
          add_terminal
            (build_stmt (L.builder_at_end context body_bb) body)
            build_br_while ;
          let end_bb = L.append_block context "while_end" the_function in
          ignore (L.build_cond_br bool_val body_bb end_bb while_builder) ;
          L.builder_at_end context end_bb
      | SRepUntil (predicate, body) ->
          let loop_bb = L.append_block context "loop_body" the_function in
          ignore (L.build_br loop_bb builder) ;
          let end_bb = L.append_block context "loop_end" the_function in
          (* partial function *)
          let rep_builder =
            build_stmt (L.builder_at_end context loop_bb) body
          in
          let bool_val = build_expr rep_builder predicate in
          ignore (L.build_cond_br bool_val end_bb loop_bb rep_builder) ;
          L.builder_at_end context end_bb
      | SFor (var, e1, e2, e3, body) ->
          let ty, _ = e1 in
          build_stmt builder
            (SBlock
               [ SAssign (var, e1)
               ; SWhile
                   ( (A.Bool, SBinop ((ty, SId var), A.Lte, e2))
                   , SBlock
                       [ body
                       ; SAssign
                           (var, (ty, SBinop ((ty, SId var), A.Plus, e3))) ]
                   ) ] )
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in
    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in
  List.iter build_function_body functions ;
  the_module
