open Parser

let execute lexbuf verbose =
  try
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    TypeAST.type_program ast;
(*      let data = Compilation.compile ast in*)
(*      Compilation.printCompilationData(data);*)
(*	  Execution.execute_program ast data;*)
    if verbose then AST.print_program ast
  with
    | CheckAST.Wrong_types_aop(x, op, y) -> CheckAST.print_wrong_types_aop x op y
    | CheckAST.Wrong_types_op(x, op, y) -> CheckAST.print_wrong_types_op x op y
    | CheckAST.Wrong_types_bool_op(x, op) -> CheckAST.print_wrong_types_bool_op x op
    | CheckAST.Wrong_type_tern(test) -> CheckAST.print_not_bool_exception "ternaire" test
    | CheckAST.Wrong_type_if(test) -> CheckAST.print_not_bool_exception "if" test
    | CheckAST.Wrong_type_for(test) -> CheckAST.print_not_bool_exception "for" test
    | CheckAST.Wrong_type_post(x) -> CheckAST.print_wrong_type_post x
    | CheckAST.Wrong_type_unop(op, x) -> CheckAST.print_wrong_type_pre op x
    | CheckAST.Type_mismatch_tern(x, y) -> CheckAST.print_type_mismatch "expression ternaire" x y
    | CheckAST.Type_mismatch_decl(x, y) -> CheckAST.print_type_mismatch "declaration" x y
    | CheckAST.Function_exist(name, typ, argstype) -> CheckAST.print_method_exist name typ argstype
    | CheckAST.Variable_name_exist(name) -> CheckAST.print_name_exist "variable" name
    | CheckAST.Attribute_name_exist(name) -> CheckAST.print_name_exist "attribut" name
    | CheckAST.Class_name_exist(name) -> CheckAST.print_name_exist "classe" name
    | CheckAST.Unknown_variable(name) -> CheckAST.print_unknown_variable name
    | CheckAST.Unknown_method(name, exps, str) -> CheckAST.print_unknown_method name exps str
    | CheckAST.Unknown_class(l) -> CheckAST.print_unknown_class (String.concat "." l)
    | CheckAST.Unknown_constructor(l, exps) -> CheckAST.print_unknown_constructor (String.concat "." l) exps
    | CheckAST.Unknown_attribute(name, c) -> CheckAST.print_unknown_attribute name c
    | CheckAST.Wrong_type_list(x, y) -> CheckAST.print_wrong_type_list x y
    | CheckAST.Wrong_return_type(x, y) -> CheckAST.print_wrong_return_type x y
    | CheckAST.Return_expression_no_type -> print_endline "Syntax error on return type"
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
