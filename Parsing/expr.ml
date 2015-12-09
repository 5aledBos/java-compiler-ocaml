type expression =
    | Const of float
    | Sum of expression * expression
    | Sub of expression * expression
    | Mul of expression * expression
    | Div of expression * expression

let rec string_of_expr expr =
    match expr with
    | Const c -> string_of_float c
    | Sum(e1, e2) -> "(" ^ (string_of_expr e1) ^ "+" ^ (string_of_expr e2) ^ ")"
    | Sub(e1, e2) -> "(" ^ (string_of_expr e1) ^ "-" ^ (string_of_expr e2) ^ ")"
    | Mul(e1, e2) -> "(" ^ (string_of_expr e1) ^ "*" ^ (string_of_expr e2) ^ ")"
    | Div(e1, e2) -> "(" ^ (string_of_expr e1) ^ "/" ^ (string_of_expr e2) ^ ")"

