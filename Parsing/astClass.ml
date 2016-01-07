
type modifierAccess = 
  | Public | Protected | Private | Empty

type modifier = 
  | Static | Abstract | Final

type package = Package of string


type import = Import of string
type importList = import list

type attributType =
  | String of string | Primitive of primitive
and primitive = 
  | Int | Float | Double


type attributAst = {
  typeof : attributType;
  name : string;
}

type attributList = attributAst list

type classAst =
  {
    classename : string;
    access : modifierAccess option;
(*    modifier : modifier option;*)
  }

type interfaceAst =
{ 
  interfacename : string;
  access : modifierAccess option;
}

type interfaceType = 
  | InterfaceType of interfaceAst

type classType = 
  | ClassType of classAst
  | InterfaceType of interfaceAst

type classList = classType list

type methodClassType =
{
  name : string;
  access : modifierAccess option
}

type classMemberType =
  | MethodClass of methodClassType
  | Attribut of attributAst

type constructeurAst =
{
  name : string;
  access : modifierAccess;
}

type constructorType = ConstructorType of constructeurAst

type classContentAst =
{
  listClassMember : classMemberType list;
  listConstructors : constructorType list;
}

type fileAst =
{
  packagename: package option;
  listImport: import list option;
  listClass: classType;
}

type fileType = FileType of fileAst



(* affichage de l'AST *)

let string_of_modifieraccess c = match c with
  | Some(Public) -> "public"
  | Some(Protected) -> "protected"
  | Some(Private) -> "private"
  | None-> "none"

let string_of_import i = match i with
  | Import(str) -> str

let rec string_of_listImport liste = match liste with
  | Some([]) -> ""
  | Some(x::xs) -> string_of_import(x) ^ ", " ^ string_of_listImport(Some(xs))
  | None -> "No import in File"

let rec printListImport liste = match liste with
  | Some([]) -> print_endline("End of Import")
  | Some(x::xs) -> print_endline("Import: " ^ string_of_import(x)); printListImport(Some(xs))
  | None -> print_endline("No import in File")

let printPackage p = match p with
  | Some(Package(str)) -> print_string("File package name: " ^ str ^ "\n")
  | None -> print_string("file package name: none" ^ "\n")


let printClassTree c = match c with
  | ClassType({classename=name; access=acc}) -> print_endline( "Classe: " ^ name ^ ", " ^ string_of_modifieraccess(acc))
  | InterfaceType({interfacename=name; access=acc}) -> print_endline( "Interface: " ^ name ^ ", " ^ string_of_modifieraccess(acc))

let printFileTree c = match c with
  | FileType({packagename=package; listImport=imports; listClass=classes}) -> printPackage(package); printListImport(imports); printClassTree(classes)
