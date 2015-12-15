type binop =
    | Badd | Bsub | Bmul | Bdiv | Bmod

type expression =
    | Const of float
    | Var of string
    | Binop of binop * expression * expression

let string_of_binop = function
    | Badd -> "+"
    | Bsub -> "-"
    | Bmul -> "*"
    | Bdiv -> "/"
    | Bmod -> "%"

let rec string_of_expr expr =
    match expr with
    | Const c -> string_of_float c
    | Var v -> v
    | Binop(op, e1, e2) -> "(" ^ (string_of_expr e1) ^ (string_of_binop op) ^ (string_of_expr e2) ^ ")"

