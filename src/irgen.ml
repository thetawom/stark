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
  and double_t = L.double_type context in
  let string_t = L.pointer_type i8_t
  and arr_t t = L.struct_type context [|i32_t; L.pointer_type t|] in
  (* Return the LLVM type for a Stark type *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Char -> i8_t
    | A.Float -> float_t
    | A.String -> string_t
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
    let error_msg = L.build_alloca string_t "" builder in
    let error_bb = L.append_block context "err" the_function in
    let error_builder = L.builder_at_end context error_bb in
    ignore
      (L.build_call printf_func
         [|string_format_str; L.build_load error_msg "" error_builder|]
         "printf" error_builder ) ;
    ignore (L.build_ret (L.const_int i32_t 1) error_builder) ;
    let throw_error bool_val builder str =
      ignore
        (L.build_store
           (L.build_global_stringptr str "" builder)
           error_msg builder ) ;
      let cont_bb = L.append_block context "no_err" the_function in
      let cont_builder = L.builder_at_end context cont_bb in
      ignore (L.build_cond_br bool_val error_bb cont_bb builder) ;
      cont_builder
    in
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
          match t with
          | A.Array (ty, sz) ->
              let elltype = ltype_of_typ ty in
              let sz' = L.const_int i32_t sz in
              let e = L.build_alloca (arr_t elltype) n builder in
              let ptr =
                L.build_array_alloca elltype sz' (n ^ "_arr") builder
              in
              ignore
                (L.build_store sz'
                   (L.build_struct_gep e 0 "" builder)
                   builder ) ;
              ignore
                (L.build_store ptr
                   (L.build_struct_gep e 1 "" builder)
                   builder ) ;
              let rec init_arr n =
                ignore
                  (L.build_store (L.const_null elltype)
                     (L.build_in_bounds_gep ptr
                        [|L.const_int i32_t n|]
                        "" builder )
                     builder ) ;
                if n == 0 then e else init_arr (n - 1)
              in
              init_arr (sz - 1)
          | _ ->
              let e = L.build_alloca ltype n builder in
              ignore (L.build_store (L.const_null ltype) e builder) ;
              e
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
    let array_sz var builder =
      L.build_load (L.build_struct_gep (lookup var) 0 "" builder) "" builder
    in
    let array_el_ptr var e builder =
      let s' = L.build_struct_gep (lookup var) 1 "" builder in
      let out_of_bounds =
        L.build_or
          (L.build_icmp L.Icmp.Slt e (L.const_int i32_t 0) "" builder)
          (L.build_icmp L.Icmp.Sge e (array_sz var builder) "" builder)
          "" builder
      in
      let builder =
        throw_error out_of_bounds builder
          "[runtime error] array out of bounds"
      in
      ( L.build_in_bounds_gep (L.build_load s' "" builder) [|e|] "" builder
      , builder )
    in
    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) =
      match e with
      | SIntLit i -> (L.const_int i32_t i, builder)
      | SBoolLit b -> (L.const_int i1_t (if b then 1 else 0), builder)
      | SFloatLit f -> (L.const_float float_t f, builder)
      | SCharLit c -> (L.const_int i8_t (Char.code c), builder)
      | SStringLit s -> (L.build_global_stringptr s "str" builder, builder)
      | SId s -> (L.build_load (lookup s) s builder, builder)
      | SArrayR (s, e) ->
          let e', builder = build_expr builder e in
          let ptr, builder = array_el_ptr s e' builder in
          (L.build_load ptr "" builder, builder)
      | SUnop (op, e) ->
          let e', builder = build_expr builder e in
          ( ( match op with
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
                        ; L.build_fpext e' double_t "" builder |]
                     else if ty == i8_t then [|char_format_str; e'|]
                     else if ty == L.pointer_type i8_t then
                       [|string_format_str; e'|]
                     else [|int_format_str; e'|] )
                     "printf" builder ) ;
                e' )
          , builder )
      | SBinop (e1, op, e2) ->
          let e1', builder = build_expr builder e1 in
          let e2', builder = build_expr builder e2 in
          let builder =
            if op == A.Divide then
              let divide_by_zero =
                ( if L.type_of e2' = i32_t then
                  L.build_icmp L.Icmp.Eq (L.const_int i32_t 0)
                else L.build_fcmp L.Fcmp.Oeq (L.const_float float_t 0.0) )
                  e2' "" builder
              in
              throw_error divide_by_zero builder
                "[runtime error] divide by zero"
            else builder
          in
          ( ( match op with
            | A.Plus ->
                if L.type_of e1' = i32_t then L.build_add else L.build_fadd
            | A.Minus ->
                if L.type_of e1' = i32_t then L.build_sub else L.build_fsub
            | A.Times ->
                if L.type_of e1' = i32_t then L.build_mul else L.build_fmul
            | A.Divide ->
                if L.type_of e1' = i32_t then L.build_sdiv else L.build_fdiv
            | A.Mod -> L.build_srem
            | A.Eq ->
                if L.type_of e1' = i32_t then L.build_icmp L.Icmp.Eq
                else L.build_fcmp L.Fcmp.Ueq
            | A.Neq ->
                if L.type_of e1' = i32_t then L.build_icmp L.Icmp.Ne
                else L.build_fcmp L.Fcmp.Une
            | A.Lt ->
                if L.type_of e1' = i32_t then L.build_icmp L.Icmp.Slt
                else L.build_fcmp L.Fcmp.Ult
            | A.Gt ->
                if L.type_of e1' = i32_t then L.build_icmp L.Icmp.Sgt
                else L.build_fcmp L.Fcmp.Ugt
            | A.Lte ->
                if L.type_of e1' = i32_t then L.build_icmp L.Icmp.Sle
                else L.build_fcmp L.Fcmp.Ule
            | A.Gte ->
                if L.type_of e1' = i32_t then L.build_icmp L.Icmp.Sge
                else L.build_fcmp L.Fcmp.Uge
            | A.And -> L.build_and
            | A.Or -> L.build_or )
              e1' e2' "tmp" builder
          , builder )
      | SCast (t1, e) ->
          let e', builder = build_expr builder e in
          let t2 = L.type_of e' in
          ( ( match t1 with
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
            | A.String when t2 == i8_t ->
                let s =
                  L.build_array_alloca i8_t (L.const_int i32_t 2) "str"
                    builder
                in
                ignore (L.build_store e' s builder) ;
                ignore
                  (L.build_store (L.const_null i8_t)
                     (L.build_gep s [|L.const_int i32_t 1|] "" builder)
                     builder ) ;
                s
            | _ -> e' )
          , builder )
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
          let e', builder = build_expr builder e in
          ( L.build_call printf_func [|int_format_str; e'|] "printf" builder
          , builder )
      | SCall ("printf", [e]) ->
          let e', builder = build_expr builder e in
          ( L.build_call printf_func
              [|float_format_str; L.build_fpext e' double_t "" builder|]
              "printf" builder
          , builder )
      | SCall ("printc", [e]) ->
          let e', builder = build_expr builder e in
          ( L.build_call printf_func [|char_format_str; e'|] "printf" builder
          , builder )
      | SCall ("prints", [e]) ->
          let e', builder = build_expr builder e in
          let is_null = L.build_is_null e' "" builder in
          let builder =
            throw_error is_null builder "[runtime error] null pointer"
          in
          ( L.build_call printf_func [|string_format_str; e'|] "printf"
              builder
          , builder )
      | SCall (f, args) ->
          let fdef, _ = StringMap.find f function_decls in
          let llargs =
            List.rev
              (List.map
                 (let f e = fst (build_expr builder e) in
                  f )
                 (List.rev args) )
          in
          let result = f ^ "_result" in
          (L.build_call fdef (Array.of_list llargs) result builder, builder)
      | SLen var -> (array_sz var builder, builder)
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
      | SExpr e -> snd (build_expr builder e)
      | SReturn e ->
          let e', builder = build_expr builder e in
          ignore (L.build_ret e' builder) ;
          builder
      | SAssign (s, e) ->
          let e', builder = build_expr builder e in
          ignore (L.build_store e' (lookup s) builder) ;
          builder
      | SArrayW (s, e1, e2) ->
          let e1', builder = build_expr builder e1 in
          let e2', builder = build_expr builder e2 in
          let ptr, builder = array_el_ptr s e1' builder in
          ignore (L.build_store e2' ptr builder) ;
          builder
      | SIf (predicate, then_stmt) ->
          let bool_val, builder = build_expr builder predicate in
          let then_bb = L.append_block context "then" the_function in
          let then_builder =
            build_stmt (L.builder_at_end context then_bb) then_stmt
          in
          let end_bb = L.append_block context "if_end" the_function in
          let build_br_end = L.build_br end_bb in
          (* partial function *)
          add_terminal then_builder build_br_end ;
          ignore (L.build_cond_br bool_val then_bb end_bb builder) ;
          L.builder_at_end context end_bb
      | SIfElse (predicate, then_stmt, else_stmt) ->
          let bool_val, builder = build_expr builder predicate in
          let then_bb = L.append_block context "then" the_function in
          let then_builder =
            build_stmt (L.builder_at_end context then_bb) then_stmt
          in
          let else_bb = L.append_block context "else" the_function in
          let else_builder =
            build_stmt (L.builder_at_end context else_bb) else_stmt
          in
          let end_bb = L.append_block context "if_end" the_function in
          let build_br_end = L.build_br end_bb in
          (* partial function *)
          add_terminal then_builder build_br_end ;
          add_terminal else_builder build_br_end ;
          ignore (L.build_cond_br bool_val then_bb else_bb builder) ;
          L.builder_at_end context end_bb
      | SWhile (predicate, body) ->
          let while_bb = L.append_block context "while" the_function in
          let build_br_while = L.build_br while_bb in
          (* partial function *)
          ignore (build_br_while builder) ;
          let while_builder = L.builder_at_end context while_bb in
          let bool_val, while_builder = build_expr while_builder predicate in
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
          let bool_val, rep_builder = build_expr rep_builder predicate in
          ignore (L.build_cond_br bool_val end_bb loop_bb rep_builder) ;
          L.builder_at_end context end_bb
      | SFor (var, e1, e2, e3, body) ->
          let e1', builder = build_expr builder e1 in
          let e2', builder = build_expr builder e2 in
          let e3', builder = build_expr builder e3 in
          let build_sle =
            if L.type_of e1' = float_t then L.build_fcmp L.Fcmp.Ole
            else L.build_icmp L.Icmp.Sle
          in
          let build_sge =
            if L.type_of e1' = float_t then L.build_fcmp L.Fcmp.Oge
            else L.build_icmp L.Icmp.Sge
          in
          ignore (L.build_store e1' (lookup var) builder) ;
          let while_bb = L.append_block context "while" the_function in
          let build_br_while = L.build_br while_bb in
          ignore (build_br_while builder) ;
          let while_builder = L.builder_at_end context while_bb in
          let var' = L.build_load (lookup var) "" while_builder in
          let is_incr = build_sle e1' e2' "" while_builder in
          let is_decr = build_sge e1' e2' "" while_builder in
          let is_under = build_sle var' e2' "" while_builder in
          let is_over = build_sge var' e2' "" while_builder in
          let bool_val =
            L.build_or
              (L.build_and is_incr is_under "" while_builder)
              (L.build_and is_decr is_over "" while_builder)
              "" while_builder
          in
          let body_bb = L.append_block context "while_body" the_function in
          let body_builder = L.builder_at_end context body_bb in
          let body_builder = build_stmt body_builder body in
          ignore
            (L.build_store
               (( if L.type_of e1' = float_t then L.build_fadd
                else L.build_add )
                  (L.build_load (lookup var) "" body_builder)
                  e3' "" body_builder )
               (lookup var) body_builder ) ;
          add_terminal body_builder build_br_while ;
          let end_bb = L.append_block context "while_end" the_function in
          ignore (L.build_cond_br bool_val body_bb end_bb while_builder) ;
          L.builder_at_end context end_bb
      | SForEach (var, arr, body) ->
          let iptr = L.build_alloca i32_t "i" builder in
          ignore (L.build_store (L.const_int i32_t 0) iptr builder) ;
          let sz = array_sz arr builder in
          let while_bb = L.append_block context "while" the_function in
          let build_br_while = L.build_br while_bb in
          ignore (build_br_while builder) ;
          let while_builder = L.builder_at_end context while_bb in
          let i = L.build_load iptr "i" while_builder in
          let bool_val = L.build_icmp L.Icmp.Slt i sz "bool" while_builder in
          let body_bb = L.append_block context "while_body" the_function in
          let body_builder = L.builder_at_end context body_bb in
          ignore (L.build_store i iptr body_builder) ;
          ignore
            (L.build_store
               (L.build_add i (L.const_int i32_t 1) "" body_builder)
               iptr body_builder ) ;
          let ptr, body_builder = array_el_ptr arr i body_builder in
          let e = L.build_load ptr "e" body_builder in
          ignore (L.build_store e (lookup var) body_builder) ;
          add_terminal (build_stmt body_builder body) build_br_while ;
          let end_bb = L.append_block context "while_end" the_function in
          ignore (L.build_cond_br bool_val body_bb end_bb while_builder) ;
          L.builder_at_end context end_bb
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in
    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in
  List.iter build_function_body functions ;
  the_module
