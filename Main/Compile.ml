(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)

let execute lexbuf verbose = 
  print_endline "Parsing...";
  (*print_endline (ParseClass.classe LexClass.nexttoken lexbuf )*)
    
  let exp = ParseExpr.expression LexExpr.nexttoken lexbuf in
  print_endline (Expr.string_of_expr exp)
  
