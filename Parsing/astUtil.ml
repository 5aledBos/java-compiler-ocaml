type modifier = 
  | Public | Protected | Private | Static | Abstract | Final | Strictfp | Volatile | Transient

type modifiers = modifier list

type attributType =
  | String of string | Primitive of primitive 
and primitive = 
  | Int | Float | Double | Char | Boolean | Byte | Short | Long



let string_of_modifier c = match c with
  | Public -> "public"
  | Protected -> "protected"
  | Private -> "private"
  | Abstract -> "abstract"
  | Static -> "static"
  | Final -> "final"
  | Strictfp -> "strictfp"

let rec string_of_modifiers liste = match liste with
  | Some([]) -> ""
  | Some(x::xs) -> string_of_modifier(x) ^ " " ^ string_of_modifiers(Some(xs))
  | None -> "No modifier"

let string_of_attributType t = match t with
  | String(str) -> str
  | Primitive(Int) -> "int"
  | Primitive(Float) -> "float"
  | Primitive(Double) ->"double"
  | Primitive(Boolean) -> "boolean"
  | Primitive(Char) -> "Char"
  | Primitive(Long) -> "long"
  | Primitive(Byte) -> "byte"
  | Primitive(Short) -> "short"
