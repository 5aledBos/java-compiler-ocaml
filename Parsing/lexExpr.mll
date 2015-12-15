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
  | "("           { LPAR }
  | ")"           { RPAR }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { TIMES }
  | "/"           { DIV }
  | "%"           { MOD }
  | real as nb    { FLOAT (float_of_string nb) }
  | ident         { IDENT (Lexing.lexeme lexbuf) }
  | "true"        { TRUE } 
  | "false"       { FALSE } 
  | "&&"          { AND }
  | "||"          { OR }
  | "!"           { NOT }
  | "=="          { EQ }
  | "!="          { NEQ }
  | ">"           { GT }
  | ">="          { GE }
  | "<"           { LT }
  | "<="          { LE }

