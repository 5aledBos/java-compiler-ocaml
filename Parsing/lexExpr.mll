{
    open ParseExpr
}

let letter = ['a'-'z' 'A'-'Z']
let non_zero_digit = ['1'-'9']
let digit = ('0' | non_zero_digit)
let digits = digit+

let input_character = (letter | digit | ' ')  (* a completer *)
let escape_sequence = ('\b')(* | "\t" | "\n" | "\r" | "\"" | "\'" | "\\")*)

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

(* Identifier *)
let ident = letter (letter | digit | '_')*

let space = [' ' '\t' '\n']

rule nexttoken = parse
  | space+                   { nexttoken lexbuf }
  | eof                      { EOF }
  | "("                      { LPAR }
  | ")"                      { RPAR }
  | "++"                     { INCR }
  | "--"                     { DECR }
  | "+"                      { PLUS }
  | "-"                      { MINUS }
  | "*"                      { TIMES }
  | "/"                      { DIV }
  | "%"                      { MOD }
  | integer as nb            { INT (int_of_string nb) }
  | floating_point as nb     { FLOAT (float_of_string nb) }
  | ident                    { IDENT (Lexing.lexeme lexbuf) }
  | '"' (str_char* as s) '"' { STRING s }
  | character as c           { CHAR c }
  | boolean as b             { BOOL (bool_of_string b) }
  | "null"                   { NULL }
  | "&&"                     { AND }
  | "||"                     { OR }
  | "!"                      { NOT }
  | "=="                     { EQ }
  | "!="                     { NEQ }
  | ">"                      { GT }
  | ">="                     { GE }
  | "<"                      { LT }
  | "<="                     { LE }
  | "~"                      { BITWISE }

