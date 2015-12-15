{
    open ParseExpr
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let real = digit* ('.' digit*)?
let ident = letter (letter | digit | '_')*
let space = [' ' '\t' '\n']

rule nexttoken = parse
  | space+        { nexttoken lexbuf }
  | eof           { EOF }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { TIMES }
  | "/"           { DIV }
  | real as nb    { FLOAT (float_of_string nb) }
  | ident         { IDENT (Lexing.lexeme lexbuf) }

