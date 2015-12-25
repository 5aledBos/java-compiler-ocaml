type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Band | Bor
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
  | If of expression * expression list
  | Ifelse of expression * expression list * expression list
  | While of expression * expression list
  | For of expression list * expression list

let string_of_binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Band -> "&&"
  | Bor  -> "||"
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

  (* Statements *)
  | If(e1, e2) -> "if(" ^ (string_of_expr e1) ^ ") {" ^ (string_of_list string_of_expr e2) ^ "}"
  | Ifelse(e1, e2, e3) -> "if(" ^ (string_of_expr e1) ^ ") {" ^ (string_of_list string_of_expr e2) ^ "}"
                          ^ " else {" ^ (string_of_list string_of_expr e3) ^ "}"
  | While(e1, e2) -> "while(" ^ (string_of_expr e1) ^ ") {" ^ (string_of_list string_of_expr e2) ^ "}"
  | For(f, b) -> "for(" ^ (string_of_list string_of_expr f) ^ ") {" ^ (string_of_list string_of_expr b) ^ "}"

