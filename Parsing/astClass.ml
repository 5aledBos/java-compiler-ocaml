
type modifierAccess = 
  | Public | Protected | Private | Empty

type classAst =
  {
    classename : string;
    access : modifierAccess;
  }

let string_of_modifieraccess c = match c with
  | Public -> "public"
  | Protected -> "protected"
  | Private -> "private"
  | Empty -> ""

let printClassAst c = print_endline(  string_of_modifieraccess(c.access) ^ c.classename)

