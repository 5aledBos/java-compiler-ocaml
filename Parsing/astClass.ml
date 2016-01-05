
type modifierAccess = 
  | Public | Protected | Private | Empty

type classAst =
  {
    classename : string;
    access : modifierAccess;
  }


type classTree = ClassTree of classAst

let string_of_modifieraccess c = match c with
  | Public -> "public"
  | Protected -> "protected"
  | Private -> "private"
  | Empty -> ""

let printClassTree c = match c with
 | ClassTree({classename=name; access=acc}) -> print_endline( "Classe: " ^ name ^ ", " ^ string_of_modifieraccess(acc))

 	
