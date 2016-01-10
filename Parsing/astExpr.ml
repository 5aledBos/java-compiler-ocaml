(* AST *)

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Band | Bor | Bpipe | Bcirc | Bamp
  | Beq | Bneq | Bgt | Bge | Blt | Ble
  | Blshift | Bsrshift | Burshift

type unopleft =
  | Uminus | Uplus
  | Ulincr | Uldecr
  | Unot
  | Ubitwise

type unopright =
  | Urincr | Urdecr

type assign =
  | Ass
  | Assmul
  | Assdiv
  | Assmod
  | Assplus
  | Assminus
  | Asslshift
  | Asssrshift
  | Assurshift
  | Assamp
  | Asscirc
  | Asspipe

type expression =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of string
  | String of string
  | Null
  | Var of string
  | Binop of expression * binop * expression
  | Unopleft of unopleft * expression
  | Unopright of expression * unopright
  | Assign of expression * assign * expression
  | Fieldaccess of expression * string
  | Fieldaccesssuper of string
  | Case of expression
  | Default

type statement =
  | Expression of expression
  | Expressions of expression list
  | Statements of statement list
  | EmptyStatement
  | Assert of expression
  | BAssert of expression * expression
  | Switch of expression * statement
  | SwitchBlock of statement list option * expression list option
  | SwitchGroup of expression list * statement list
  | Break of string
  | Continue of string
  | Return of expression
  | Throw of expression
  | Synchro of expression * statement list
  | Try of statement * statement list
  | Tryfin of statement * statement list * statement
  | Label of string * statement
  | If of expression * statement
  | Ifelse of expression * statement * statement
  | While of expression * statement
  (*| For of expression list * expression list*)


(* ERRORS *)

type error =
  | Illegal_bracket of char

exception Err of error

let report_err = function
      | Illegal_bracket c ->
    print_string "'";
	  print_char c;
	  print_string "' expected"


(* STRING_OF *)

let string_of_binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Band -> "&&"
  | Bor  -> "||"
  | Bpipe -> "|"
  | Bcirc -> "^"
  | Bamp -> "&"
  | Beq  -> "="
  | Bneq -> "!="
  | Bgt  -> ">"
  | Bge  -> ">="
  | Blt  -> "<"
  | Ble  -> "<="
  | Blshift -> "<<"
  | Bsrshift -> ">>"
  | Burshift -> ">>>"

let string_of_unopleft = function
  | Unot -> "not"
  | Uminus -> "-"
  | Uplus -> "+"
  | Ulincr -> "++"
  | Uldecr -> "--"
  | Ubitwise -> "~"

let string_of_unopright = function
  | Urincr -> "++"
  | Urdecr -> "--"

let string_of_assign = function
  | Ass -> "="
  | Assmul -> "*="
  | Assdiv -> "/="
  | Assmod -> "%="
  | Assplus -> "+="
  | Assminus -> "-="
  | Asslshift -> "<<="
  | Asssrshift -> ">>="
  | Assurshift -> ">>>="
  | Assamp -> "&="
  | Asscirc -> "^="
  | Asspipe -> "|="

let rec string_of_list f l =
  match l with
  | [] -> ""
  | h::t -> (f h) ^ (string_of_list f t)

let string_of_optlist f l =
  match l with
  | None -> ""
  | Some(li) -> string_of_list f li

let rec string_of_expr expr =
  match expr with
  (* Literals *)
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> "'" ^ c ^ "'"
  | String s -> "\"" ^ s ^ "\""
  | Null -> "null"
  | Var v -> v

  (* Operations *)
  | Binop(e1, op, e2) -> "(" ^ (string_of_expr e1) ^ (string_of_binop op) ^ (string_of_expr e2) ^ ")"
  | Bool true -> "true"
  | Bool false -> "false"
  | Unopleft(op, e) -> "(" ^ (string_of_unopleft op) ^ (string_of_expr e) ^ ")"
  | Unopright(e, op) -> "(" ^ (string_of_expr e) ^ (string_of_unopright op) ^ ")"
  | Assign(e1, ass, e2) -> "(" ^ (string_of_expr e1) ^ (string_of_assign ass) ^ (string_of_expr e2) ^ ")"
  | Fieldaccess(e, str) -> "(" ^ (string_of_expr e) ^ "." ^ str ^ ")"
  | Fieldaccesssuper(str) -> "(super." ^ str ^ ")"
  | Case(e) -> "(case: " ^ (string_of_expr e) ^ ")"
  | Default -> "default:"

let rec string_of_statement stat =
  match stat with
  | Expression e -> string_of_expr e
  | Expressions e -> string_of_list string_of_expr e
  | Statements s -> string_of_list string_of_statement s
  | EmptyStatement -> "Empty statement"
  | Assert(e) -> "(assert " ^ (string_of_expr e) ^ ")"
  | BAssert(e1, e2) -> "(assert " ^ (string_of_expr e1) ^ ":" ^ (string_of_expr e2) ^ ")"
  | Switch(e, s) -> "(switch (" ^ (string_of_expr e) ^ ") " ^ (string_of_statement s) ^ ")"
  | SwitchBlock(s, e) -> "{" ^ (string_of_optlist string_of_statement s) ^ (string_of_optlist string_of_expr e) ^ "}"
  | SwitchGroup(e, s) -> (string_of_list string_of_expr e) ^ (string_of_list string_of_statement s)
  | Break(v) -> "(break " ^ v ^ ")"
  | Continue(v) -> "(continue " ^ v ^ ")"
  | Return(e) -> "(return " ^ (string_of_expr e) ^ ")"
  | Throw(e) -> "(throw " ^ (string_of_expr e) ^ ")"
  | Synchro(e, s) -> "(synchronized (" ^ (string_of_expr e) ^ ") {" ^ (string_of_list string_of_statement s) ^ "}"
  | Label(v, s) -> v ^ " : " ^ (string_of_statement s)
  | If(e, s) -> "if(" ^ (string_of_expr e) ^ ") {" ^ (string_of_statement s) ^ "}"
  | Ifelse(e, s1, s2) -> "if(" ^ (string_of_expr e) ^ ") {" ^ (string_of_statement s1) ^ "}"
                          ^ " else {" ^ (string_of_statement s2) ^ "}"
  | While(e, s) -> "while(" ^ (string_of_expr e) ^ ") {" ^ (string_of_statement s) ^ "}"
  (*| For(f, b) -> "for(" ^ (string_of_list string_of_expr f) ^ ") {" ^ (string_of_list string_of_expr b) ^ "}"*)
