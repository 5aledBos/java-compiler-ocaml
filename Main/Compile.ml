open AstExpr
open AstUtil

(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)

let print_statement stat =
  print_string (AstExpr.string_of_statement stat);
  print_newline()

let execute lexbuf verbose = 
  print_endline "Parsing...";
  
(*  try*)
(*    let stat_list = Parser.statements Lexer.nexttoken lexbuf in*)
(*    List.iter print_statement stat_list;*)
(*    print_newline()*)
(*  with*)
(*  | Lexer.Error (kind, start, fin) ->*)
(*    Lexer.report_error kind;*)
(*    Lexer.print_position start fin;*)
(*    print_newline()*)
(*  | AstExpr.Err kind ->*)
(*    AstExpr.report_err kind;*)
(*    print_newline()*)


try
  let exp = Parser.compilationUnit Lexer.nexttoken lexbuf in
  AstClass.printFileTree (exp)
with
  | Lexer.Error (kind, start, fin) ->
     Lexer.report_error kind;
     Lexer.print_position start fin;
     print_newline()
  | Parser.Error ->
     print_string("erreur inconnu");
     print_newline()
  | Illegal_variable ->
     print_string("erreur variable");
     print_newline()
| Illegal_package ->
     print_string("erreur package");
     print_newline()
| Illegal_result ->
     print_string("erreur de résultat");
     print_newline()
| Illegal_methodeBody ->
     print_string("erreur au sein d'une methode");
     print_newline()
| Illegal_variableDeclarator ->
     print_string("erreur declaration d'une variable");
     print_newline()

| Illegal_interfaceBody ->
     print_string("erreur declaration d'une interface");
     print_newline()
| Illegal_enumConstant ->
     print_string("erreur declaration d'une enumeration constant");
     print_newline()

| Illegal_import ->
     print_string("erreur declaration d'un import ou à la suite d'un import");
     print_newline()
 |External_error ->
     print_string("erreur entre deux classes/interfaces ");
     print_newline()
|Illegal_expression -> 
print_string("erreur d'une expression ");
     print_newline()
