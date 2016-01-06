
type modifierAccess = 
  | Public | Protected | Private | Empty

type package = Package of string



type import = Import of string
type importList = import list


type classAst =
  {
    classename : string;
    access : modifierAccess;
  }

type classType = 
  | ClassType of classAst

type classList = classType list

type interfaceAst =
{ 
  interfacename : string;
  access : modifierAccess;
}

type fileAst =
{
  packagename: package;
  listImport: importList;
  listClass: classType;
}

type fileType = FileType of fileAst

let string_of_modifieraccess c = match c with
  | Public -> "public"
  | Protected -> "protected"
  | Private -> "private"
  | Empty -> ""

let printPackage p = match p with
  | Package(str) -> print_string("nom du package du fichier: " ^ str ^ "\n")

let printClassTree c = match c with
  | ClassType({classename=name; access=acc}) -> print_endline( "Classe: " ^ name ^ ", " ^ string_of_modifieraccess(acc))

let printFileTree c = match c with
  | FileType({packagename=package; listImport=imports; listClass=classes}) -> printPackage(package); printClassTree(classes)
