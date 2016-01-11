(* Build with `ocamlbuild -pkg alcotest test.byte` *)

let string_of_token t =
    match t with
    | Parser.PLUS -> "+"
    | Parser.MINUS -> "-"
    | Parser.TIMES -> "*"
    | Parser.DIV -> "/"
    | Parser.FLOAT f -> string_of_float f
    | Parser.INT i -> string_of_int i
    | Parser.BOOL b -> string_of_bool b
    | Parser.IDENT s -> s

let pp_print_token f t =
    Format.pp_print_string f (string_of_token t)

let token =
  let module M = struct
    type t = Parser.token
    let pp = pp_print_token
    let equal = (=)
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

(* A module with functions to test *)
module To_test = struct
    let lex token = LexExpr.nexttoken token
end

(* The tests *)
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


let simple_token = [
  "Plus" , `Quick, plus;
  "Minus" , `Quick, minus;
  "Times" , `Quick, times;
  "Div" , `Quick, div;
  "Float", `Quick, fl;
  "Int", `Quick, integer;
]

(* Run it *)
let () =
  Alcotest.run "My first test" [
    "Simple token tests", simple_token;
  ]
