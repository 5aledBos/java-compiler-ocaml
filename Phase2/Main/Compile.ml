open Parser

let execute lexbuf verbose =
  try
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    (*if verbose then AST.print_program ast;*)
    let ast_type = TypeAST.type_program ast in
    if verbose then AST.print_program ast_type
  with
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
