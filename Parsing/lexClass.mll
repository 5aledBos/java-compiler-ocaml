{
  open ParseClass
}

let space = [' ' '\t' '\n']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let caracteres = ['[''{''}''('')''['']''=''!''&''|']
let ident = letter (letter | digit | '_')*
let semicolon = ';'
(*let commentaires1 ='/''/' [^'\n']* '\n'*)
let commentaires1 ='/''/' (ident | ' ' | semicolon | caracteres )* '\n'
let commentaires2 ='/''*' (ident | space | semicolon | caracteres)* '*''/'

rule nexttoken = parse
  | space+		{ nexttoken lexbuf }
  | commentaires1	{ nexttoken lexbuf }
  | commentaires2	{ nexttoken lexbuf }
  | eof			{ EOF }
  | "{"			{ LBRACE }
  | "}"			{ RBRACE }
  | "("			{ LBRACKET }
  | ")"			{ RBRACKET }
  | "="			{ EQUAL }
  | "!="		{ EPOINT }
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
  | "abstract"		{ ABSTRACT }
  | "if"		{ IF }
  | "return"		{ RETURN }
  | ident as str 	{ IDENT str }
  | semicolon		{ SC }



