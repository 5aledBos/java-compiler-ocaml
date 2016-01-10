{
    open Parser
    open Lexing

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

	  let print_position start fin =
      if (start.pos_lnum = fin.pos_lnum) then
        begin
	  print_string "line ";
	  print_int start.pos_lnum;
	  print_string " characters ";
	  print_int (start.pos_cnum - start.pos_bol);
	  print_string "-";
	  print_int (fin.pos_cnum - fin.pos_bol)
        end
      else
        begin
	  print_string "from line ";
	  print_int start.pos_lnum;
	  print_string " character ";
	  print_int (start.pos_cnum - start.pos_bol);
	  print_string " to line ";
	  print_int fin.pos_lnum;
	  print_string " character ";
	  print_int (fin.pos_cnum - fin.pos_bol)
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
let space = [' ' '\t']
let newline = ('\010' | '\013' | "\013\010")

let input_character = (letter | digit |  [' ''[''{''}''('')''['']''=''!''&''|'';'])  (* a completer *)
let escape_sequence = ('\b')(* | "\t" | "\n" | "\r" | "\"" | "\'" | "\\")*)

(* Identifier *)
let ident = letter (letter | digit | '_')*

(* Comments *)
let comment_single ='/''/' input_character* '\n'
let comment_mul ='/''*' (space | newline | input_character)* '*''/'

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

(* Character and string *)
let str_char = (input_character | escape_sequence)
let character = "'" str_char "'"
let str = '"' str_char* '"'


(* RULE NEXTTOKEN *)

rule nexttoken = parse
  | newline                  { incr_line lexbuf; nexttoken lexbuf }
  | space+                   { nexttoken lexbuf }
  | eof                      { EOF }
  | comment_single           { nexttoken lexbuf }
  | comment_mul              { nexttoken lexbuf }

  (* Classes *)
  | "class"                  { CLASS }
  | "interface"              { INTERFACE }
  | "enum"					         { ENUM }
  | "public"                 { PUBLIC }
  | "protected"              { PROTECTED }
  | "private"                { PRIVATE }
  | "package"                { PACKAGE }
  | "import"                 { IMPORT }
  | "extends"                { EXTENDS }
  | "implements"             { IMPLEMENTS }
  | "abstract"               { ABSTRACT }
  | "static"                 { STATIC }
  | "final"                  { FINAL }
  | "strictfp"               { STRICTFP }
  | "transient"			         { TRANSIENT }
  | "volatile"					     { VOLATILE }
  | "this"                   { THIS }
  | "super"                  { SUPER }
  | "break"                  { BREAK }
  | "continue"               { CONTINUE }
  | "return"                 { RETURN }
  | "throw"                  { THROW }
  | "synchronized"           { SYNCHRONIZED }
  | "try"                    { TRY }
  | "finally"                { FINALLY }
  | "void"                   { VOID }
  | "int"                    { PINT }
  | "float"					         { PFLOAT }
  | "char"					         { PCHAR }
  | "boolean"				         { PBOOLEAN }
  | "byte"					         { PBYTE }
  | "short"					         { PSHORT }
  | "long"					         { PLONG }
  | "double"				         { PDOUBLE }
  | ";"	                     { SC }
  | ","	                     { COMA }
  | "."                      { POINT }
  | "new"                    { NEW }

  (* Statements *)
  | "if"                     { IF }
  | "else"                   { ELSE }
  | "while"                  { WHILE }
  | "do"                     { DO }
  | "for"                    { FOR }
  | "switch"                 { SWITCH }
  | "case"                   { CASE }
  | "default"                { DEFAULT }
  | "assert"                 { ASSERT }

  (* Brackets *)
  | "("                      { LPAR }
  | ")"                      { RPAR }
  | "{"	                     { LBRACE }
  | "}"                      { RBRACE }
  | "["                      { LBRACKET }
  | "]"                      { RBRACKET }

  (* Unary operators *)
  | "++"                     { INCR }
  | "--"                     { DECR }
  | "!"                      { NOT }
  | "~"                      { BITWISE }

  (* Multiplicative and additive operators *)
  | "*"                      { TIMES }
  | "/"                      { DIV }
  | "%"                      { MOD }
  | "+"                      { PLUS }
  | "-"                      { MINUS }

  (* Shift operators *)
  | "<<"                     { LSHIFT }
  | ">>"                     { SRSHIFT }
  | ">>>"                    { URSHIFT }

  (* Relational and equality operators *)
  | ">"                      { GT }
  | ">="                     { GE }
  | "<"                      { LT }
  | "<="                     { LE }
  | "=="                     { EQUAL }
  | "!="                     { NEQUAL }

  (* Conditional operators *)
  | "&&"                     { AND }
  | "||"                     { OR }

  (* Assignment operators *)
  | "="                      { ASS }
  | "*="                     { MULASS }
  | "/="                     { DIVASS }
  | "%="                     { MODASS }
  | "+="                     { PLUSASS }
  | "-="                     { MINUSASS }
  | "<<="                    { LSHIFTASS }
  | ">>="                    { SRSHIFTASS }
  | ">>>="                   { URSHIFTASS }
  | "&="                     { AMPASS }
  | "^="                     { CIRCASS }
  | "|="                     { PIPEASS }

  (* Literals *)
  | "null"                   { NULL }
  | integer as nb            { INT (int_of_string nb) }
  | floating_point as nb     { FLOAT (float_of_string nb) }
  | boolean as b             { BOOL (bool_of_string b) }
  | ident                    { IDENT (Lexing.lexeme lexbuf) }
  | '"' (str_char* as s) '"' { STRING s }
  | "'" (str_char as c) "'"  { CHAR (String.make 1 c) }

  | "?"                      { QUESTMARK }
  | ":"                      { COLON }
  | "|"                      { PIPE }
  | "^"                      { CIRCUMFLEX }
  | "&"                      { AMP }

  (* Other => error *)
  | _ as c                   { raise_error (Illegal_character(c)) lexbuf }
