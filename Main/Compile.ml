(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)

let execute lexbuf verbose = 
  print_endline "parsing todo";
  (*print_endline (ParseClass.classe LexClass.nexttoken lexbuf )*)
  
  print_endline (ParseExpr.expression LexExpr.nexttoken lexbuf)
  
