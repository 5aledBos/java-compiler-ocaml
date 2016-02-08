open Parser

let execute lexbuf verbose =
  try
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    TypeAST.type_program ast;
    if verbose then AST.print_program ast
  with
    | CheckAST.Wrong_types_aop(x, op, y) ->
      print_string ("L'operateur " ^ (AST.string_of_assign_op op));
      print_string (" attend deux arguments de meme type");
      print_string (" et il reçoit " ^ (CheckAST.stringOf_prim x));
      print_endline (" et " ^ (CheckAST.stringOf_prim y))
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
