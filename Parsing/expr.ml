type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Band | Bor | Bpipe | Bcirc | Bamp
  | Beq | Bneq | Bgt | Bge | Blt | Ble

type unop =
  | Uminus
  | Uplus
  | Uincr
  | Udecr
  | Unot
  | Ubit

type assign =
  | Ass
  | Assmul
  | Assdiv
  | Assmod
  | Assplus
  | Assminus

type expression =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of string
  | String of string
  | Null
  | Var of string
  | Binop of expression * binop * expression
  | Unop of unop * expression
  | Assign of expression * assign * expression

type statement =
  | Expression of expression
  | Expressions of expression list
  | Statements of statement list
  | If of expression * statement
  (*| Ifelse of expression * statement * statement*)
  | While of expression * statement
  (*| For of expression list * expression list*)

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

let string_of_unop = function
  | Unot -> "not"
  | Uminus -> "-"
  | Uplus -> "+"
  | Uincr -> "++"
  | Udecr -> "--"
  | Ubit -> "~"

let string_of_assign = function
  | Ass -> "="
  | Assmul -> "*="
  | Assdiv -> "/="
  | Assmod -> "%="
  | Assplus -> "+="
  | Assminus -> "-="

let rec string_of_list f l =
  match l with
  | [] -> ""
  | h::t -> (f h) ^ (string_of_list f t)

let rec string_of_expr expr =
  match expr with
  (* Literals *)
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> "'" ^ c ^ "'"
  | String s -> "\"" ^ s ^ "\""
  | Null -> "null"
  | Var v -> v
  
  (*  *)
  | Binop(e1, op, e2) -> "(" ^ (string_of_expr e1) ^ (string_of_binop op) ^ (string_of_expr e2) ^ ")"
  | Bool true -> "true"
  | Bool false -> "false"
  | Unop(op, e) -> "(" ^ (string_of_unop op) ^ (string_of_expr e) ^ ")"
  | Assign(e1, ass, e2) -> "(" ^ (string_of_expr e1) ^ (string_of_assign ass) ^ (string_of_expr e2) ^ ")"

let rec string_of_statement stat =
  match stat with
  | Expression e -> string_of_expr e
  | Expressions e -> string_of_list string_of_expr e
  | Statements s -> string_of_list string_of_statement s
  | If(e, s) -> "if(" ^ (string_of_expr e) ^ ") {" ^ (string_of_statement s) ^ "}"
  (*| Ifelse(e1, e2, e3) -> "if(" ^ (string_of_expr e1) ^ ") {" ^ (string_of_list string_of_expr e2) ^ "}"
                          ^ " else {" ^ (string_of_list string_of_expr e3) ^ "}"*)
  | While(e, s) -> "while(" ^ (string_of_expr e) ^ ") {" ^ (string_of_statement s) ^ "}"
  (*| For(f, b) -> "for(" ^ (string_of_list string_of_expr f) ^ ") {" ^ (string_of_list string_of_expr b) ^ "}"*)

