(* Build with `ocamlbuild -pkg alcotest testlexer.byte` *)

let pp_print_token f t =
    Format.pp_print_string f (Tokens.string_of_token t)

let token =
  let module M = struct
    type t = Parser.token
    let pp = pp_print_token
    let equal = (=)
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

(* A module with functions to test *)
module To_test = struct
    let lex token = Lexer.nexttoken token
end

(* simple_token tests *)
let plus () =
  Alcotest.(check token) "token plus"  Parser.PLUS (To_test.lex (Lexing.from_string "+"))
let minus () =
  Alcotest.(check token) "token minus"  Parser.MINUS (To_test.lex (Lexing.from_string "-"))
let times () =
  Alcotest.(check token) "token times"  Parser.TIMES (To_test.lex (Lexing.from_string "*"))
let div () =
  Alcotest.(check token) "token div"  Parser.DIV (To_test.lex (Lexing.from_string "/"))
let fl () =
  Alcotest.(check token) "token float" (Parser.FLOAT 3.4) (To_test.lex (Lexing.from_string "3.4"))
let integer () =
  Alcotest.(check token) "token integer" (Parser.INT 32) (To_test.lex (Lexing.from_string "32"))
let clazz () =
  Alcotest.(check token) "token class" Parser.CLASS (To_test.lex (Lexing.from_string "class"))
let interface () =
  Alcotest.(check token) "token interface" Parser.INTERFACE (To_test.lex (Lexing.from_string "interface"))
let enum () =
  Alcotest.(check token) "token enum" Parser.ENUM (To_test.lex (Lexing.from_string "enum"))
let public () =
  Alcotest.(check token) "token public" Parser.PUBLIC (To_test.lex (Lexing.from_string "public"))
let protected () =
  Alcotest.(check token) "token protected" Parser.PROTECTED (To_test.lex (Lexing.from_string "protected"))

(* multiple_token tests *)
(*let several () =
  Alcotest.(check token) "tokens plus minus protected"  (Parser.PLUS, Parser.MINUS, Parser.PROTECTED) (To_test.lex (Lexing.from_string "+ - protected"))*)

let simple_token = [
  "Plus" , `Quick, plus;
  "Minus" , `Quick, minus;
  "Times" , `Quick, times;
  "Div" , `Quick, div;
  "Float", `Quick, fl;
  "Int", `Quick, integer;
  "Class" , `Quick, clazz;
  "Interface" , `Quick, interface;
  "Enum" , `Quick, enum;
  "Public", `Quick, public;
  "Protected", `Quick, protected;
]

(* Run it *)
let () =
  Alcotest.run "My first test" [
    "Simple token tests", simple_token;
  ]
