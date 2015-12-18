{
    open Parser
    
    open Lexing
    exception Eof
    
    (* ERRORS *)
    type error =
      | Illegal_character of char
    exception Error of error * position * position
    
    let raise_error err lexbuf =
      raise (Error(err, lexeme_start_p lexbuf, lexeme_end_p lexbuf))
    
    let report_error = function
      | Illegal_character c ->
    print_string "Illegal character '";
	  print_char c;
	  print_string "' "
	  
	  let print_position start finish =
      if (start.pos_lnum = finish.pos_lnum) then
        begin
	  print_string "line ";
	  print_int start.pos_lnum;
	  print_string " characters ";
	  print_int (start.pos_cnum - start.pos_bol);
	  print_string "-";
	  print_int (finish.pos_cnum - finish.pos_bol)
        end
      else
        begin
	  print_string "from line ";
	  print_int start.pos_lnum;
	  print_string " character ";
	  print_int (start.pos_cnum - start.pos_bol);
	  print_string " to line ";
	  print_int finish.pos_lnum;
	  print_string " character ";
	  print_int (finish.pos_cnum - finish.pos_bol)
        end

    let incr_line lexbuf =
      let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- 
	  { 
	    pos with 
	      pos_lnum = pos.pos_lnum + 1; 
	      pos_bol = pos.pos_cnum;
	  }
}

(* REGULAR EXPRESSIONS *)

let letter = ['a'-'z' 'A'-'Z']
let non_zero_digit = ['1'-'9']
let digit = ('0' | non_zero_digit)
let digits = digit+
let space = [' ' '\t' '\n']

let input_character = (letter | digit | ' ')  (* a completer *)
let escape_sequence = ('\b')(* | "\t" | "\n" | "\r" | "\"" | "\'" | "\\")*)

let caracteres = ['[''{''}''('')''['']''=''!''&''|']

(* Identifier *)
let ident = letter (letter | digit | '_')*

(* Comments *)
let comment_single ='/''/' (ident | ' ' | ';' | caracteres )* '\n'
let comment_mul ='/''*' (ident | space | ';' | caracteres)* '*''/'

(* Integer *)
let integer = ('0' | non_zero_digit digits?) 'L'?

(* Floating point *)
let signed_integer = ('+' | '-')? digits
let exponent_part = ('e' | 'E') signed_integer
let float_type_suffix = ('f' | 'F' | 'd' | 'D')
let floating_point = (digits '.' digits? exponent_part? float_type_suffix?
  | '.' digits exponent_part? float_type_suffix?
  | digits exponent_part float_type_suffix?
  | digits exponent_part? float_type_suffix)

(* Boolean *)
let boolean = ("true" | "false")

(* Character *)
let character = "'" (input_character | escape_sequence) "'"

(* String *)
let str_char = (input_character | escape_sequence)
let str = '"' str_char* '"'


(* RULE NEXTTOKEN *)

rule nexttoken = parse
  | space+                   { nexttoken lexbuf }
  | eof                      { EOF }
  | comment_single         	 { nexttoken lexbuf }
  | comment_mul  	           { nexttoken lexbuf }
  
  (* Classes *)
  | "class"		               { CLASS }
  | "public"		             { PUBLIC }
  | "protected"		           { PROTECTED }
  | "private"		             { PRIVATE }
  | "package" 		           { PACKAGE }
  | "import"		             { IMPORT }
  | "extends"		             { EXTENDS }
  | "implements"	           { IMPLEMENTS }
  | "abstract"		           { ABSTRACT }
  | "return"		             { RETURN }
  | "int"                    { PINT }
  | ";"		                   { SC }
  
  (* Statements *)
  | "if"		                 { IF }
  | "then"		               { THEN }
  | "else"		               { ELSE }
  | "while"		               { WHILE }
  | "for"		                 { FOR }
  
  (*  *)
  | "("                      { LPAR }
  | ")"                      { RPAR }
  | "{"			                 { LBRACE }
  | "}"			                 { RBRACE }
    
  (* Unary operators *)
  | "++"                     { INCR }
  | "--"                     { DECR }
  (* TODO: ++ and -- are also postfix expressions to handle - 15.14 *)
  | "!"                      { NOT }
  | "~"                      { BITWISE }
  
  (* Multiplicative and additive operators *)
  | "*"                      { TIMES }
  | "/"                      { DIV }
  | "%"                      { MOD }
  | "+"                      { PLUS }
  | "-"                      { MINUS }
  
  (* TODO: Shift operators - 15.19 *)
  
  (* Relational and equality operators *)
  | ">"                      { GT }
  | ">="                     { GE }
  | "<"                      { LT }
  | "<="                     { LE }
  | "=="                     { EQUAL }
  | "!="                     { NEQUAL }
  
  (* TODO: Bitwise and logical operators - 15.22 *)
  
  (* Conditional operators *)
  | "&&"                     { AND }
  | "||"                     { OR }
  (* TODO: Conditional operator ? : - 15.25 *)
  
  (* Assignment operators *)
  | "="                      { ASS }
  | "*="                     { MULASS }
  | "/="                     { DIVASS }
  | "%="                     { MODASS }
  | "+="                     { PLUSASS }
  | "-="                     { MINUSASS }
  (* TODO: Assignment operators <<= >>= >>>= &= ^= |= - 15.26 *)
    
  (* Literals *)
  | "null"                   { NULL }
  | integer as nb            { INT (int_of_string nb) }
  | floating_point as nb     { FLOAT (float_of_string nb) }
  | ident                    { IDENT (Lexing.lexeme lexbuf) }
  | '"' (str_char* as s) '"' { STRING s }
  | character as c           { CHAR c }       (* TODO: Fix *)
  | boolean as b             { BOOL (bool_of_string b) }
  
  (* Other => error *)
  | _ as c                   { raise_error (Illegal_character(c)) lexbuf }  (* TODO: check and fix *)
