let string_of_token t =
    match t with
    | Parser.CLASS -> "class"
    | Parser.INTERFACE -> "interface"
    | Parser.ENUM -> "enum"
    | Parser.PUBLIC -> "public"
    | Parser.PROTECTED -> "protected"
    | Parser.PRIVATE -> "private"
    | Parser.PLUS -> "+"
    | Parser.MINUS -> "-"
    | Parser.TIMES -> "*"
    | Parser.DIV -> "/"
    | Parser.FLOAT f -> string_of_float f
    | Parser.INT i -> string_of_int i
    | Parser.BOOL b -> string_of_bool b
    | Parser.IDENT s -> s
