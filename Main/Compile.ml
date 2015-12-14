(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)

let execute lexbuf verbose = 
  print_endline "Parsing...";
  (*print_endline (ParseClass.classe LexClass.nexttoken lexbuf )*)
    
  let exp = ParseClass.filecontent LexClass.nexttoken lexbuf in
  print_endline (exp)
  
