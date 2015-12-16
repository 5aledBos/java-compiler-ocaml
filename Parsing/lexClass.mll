{
  open ParseClass
}

let space = [' ' '\t' '\n']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*
let semicolon = ';'

rule nexttoken = parse
  | space+		{ nexttoken lexbuf }
  | eof			{ EOF }
  | "{"			{ LBRACE }
  | "}"			{ RBRACE }
  | "("			{ LBRACKET }
  | ")"			{ RBRACKET }
  | "class"		{ CLASS }
  | "public"		{ PUBLIC }
  | "protected"		{ PROTECTED }
  | "private"		{ PRIVATE }
  | "package" 		{ PACKAGE }
  | "import"		{ IMPORT }
  | "int"		{ INT }
  | "void"		{ VOID }
  | "extends"		{ EXTENDS }
  | "implements"	{ IMPLEMENTS }
  | "return"		{ RETURN }
  | ident as str 	{ IDENT str }
  | semicolon		{ SC }



