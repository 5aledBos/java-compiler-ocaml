(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)

let print_expression exp =
  print_string (Expr.string_of_expr exp);
  print_newline()

let execute lexbuf verbose = 
  print_endline "Parsing...";
  
  try
    let exp_list = Parser.expressions Lexer.nexttoken lexbuf in
    List.iter print_expression exp_list;
    print_newline()
  with
  | Lexer.Error (kind, start, fin) ->
     Lexer.report_error kind;
     Lexer.print_position start fin;
     print_newline()
  
  (*let exp = Parser.filecontent Lexer.nexttoken lexbuf in
  AstClass.printClassAst (exp)*)

