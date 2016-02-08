open Parser

let execute lexbuf verbose =
  try
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    TypeAST.type_program ast;
    if verbose then AST.print_program ast
  with
    | CheckAST.Wrong_types_aop(x, op, y) -> CheckAST.print_wrong_types_aop x op y
    | CheckAST.Wrong_types_op(x, op, y) -> CheckAST.print_wrong_types_op x op y
    | CheckAST.Wrong_type_tern(test) -> CheckAST.print_wrong_type_tern test
    | CheckAST.Wrong_type_if(test) -> CheckAST.print_wrong_type_if test
    | CheckAST.Type_mismatch_tern(x, y) -> CheckAST.print_type_mismatch_tern x y
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
