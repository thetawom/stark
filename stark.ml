(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, and dump the module *)

type action = Ast | Sast

let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  
  let sast = Semant.check ast in
  print_string (Sast.string_of_sprogram sast)
  (*print_string (Ast.string_of_program ast)*)