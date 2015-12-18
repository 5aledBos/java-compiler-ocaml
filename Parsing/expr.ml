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
  | Float of float
  | Int of int
  | Var of string
  | String of string
  | Char of string
  | Binop of expression * binop * expression
  | Bool of bool
  | Unop of unop * expression
  | Assign of expression * assign * expression

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

let rec string_of_expr expr =
  match expr with
  | Float f -> string_of_float f
  | Int i -> string_of_int i
  | Var v -> v
  | String s -> "\"" ^ s ^ "\""
  | Char c -> "'" ^ c ^ "'"
  | Binop(e1, op, e2) -> "(" ^ (string_of_expr e1) ^ (string_of_binop op) ^ (string_of_expr e2) ^ ")"
  | Bool true -> "true"
  | Bool false -> "false"
  | Unop(op, e) -> "(" ^ (string_of_unop op) ^ (string_of_expr e) ^ ")"
  | Assign(e1, ass, e2) -> "(" ^ (string_of_expr e1) ^ (string_of_assign ass) ^ (string_of_expr e2) ^ ")"

