(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)

let print_expression exp =
  print_string (Expr.string_of_expr exp);
  print_newline()

let execute lexbuf verbose = 
  print_endline "Parsing...";
  
  let exp_list = Parser.expressions Lexer.nexttoken lexbuf in
  List.iter print_expression exp_list;
  exit 0;
  
  (*let exp = Parser.filecontent Lexer.nexttoken lexbuf in
  AstClass.printClassAst (exp)*)

