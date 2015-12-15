(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)

let execute lexbuf verbose = 
    print_endline "Parsing...";
    let exp = ParseExpr.expression LexExpr.nexttoken lexbuf in
    print_endline (Expr.string_of_expr exp)
    
  (*let exp = ParseClass.filecontent LexClass.nexttoken lexbuf in
  print_endline (exp)*)
  
