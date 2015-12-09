{
  open parseClass
}

let space = [' ' '\t' '\n']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*

rule nexttoken = parse
  | space+		{ nexttoken lexbuf }
  | eof			{ EOF }
  | "{"			{ LBRACE }
  | "}"			{ RBRACE }
  | "class"		{ CLASS }
  | ident as str 	{ IDENT str }


