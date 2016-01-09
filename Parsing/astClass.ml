
type modifier = 
  | Public | Protected | Private | Static | Abstract | Final | Strictfp

type modifiers = modifier list

(*type modifier = *)
(*  | Static | Abstract | Final | Strictfp*)

type package = Package of string


type import = Import of importType
and importType =
  {
	isStatic : bool;
 	name : string;
  }
type importList = import list

type attributType =
  | String of string | Primitive of primitive
and primitive = 
  | Int | Float | Double | Char | Boolean | Byte | Short | Long

type parameter = 
{
  parametertype : attributType;
  name : string
}




type attributAst = {
  typeof : attributType;
  name : string;
}



type attributList = attributAst list

type blockstmts = BlockStatements of AstExpr.statement list

type methodClassType =
{
  name : string;
  access : modifiers option;
  methodbody: blockstmts option
}

type classMemberType =
  | MethodClass of methodClassType
  | Attribut of attributAst

type constructorBodyAst =
{
  invocation : constructorInvocation option;
  liststatements : blockstmts;
}
and constructorInvocation =
  { 
	invocator : thisOrSuper;
	parameters: parameter list option
}
and thisOrSuper =
  |This | Super


type constructorAst =
{
  name : string;
  access : modifiers option;
  constructorbody : constructorBodyAst option;
}


type constructorType = ConstructorType of constructorAst

type classContentAst =
{
  listClassMember : classMemberType list;
  listConstructors : constructorType list;
}

type classBodyDeclaration =
  | ClassMemberType of classMemberType
  | ConstructorType of constructorAst

type classBodyAst = classBodyDeclaration list

type classBodyType = 
  | ClassBodyType of classBodyAst

type classAst =
  {
    classename : string;
    access : modifiers option;
	inheritance : string option;
 	interfaces : string list option;
	classbody : classBodyAst option;
(*    modifier : modifier option;*)
  }

type interfaceAst =
{ 
  interfacename : string;
  access : modifiers option;
}

type interfaceType = 
  | InterfaceType of interfaceAst

type classType = 
  | ClassType of classAst
  | InterfaceType of interfaceAst

type classList = classType list

type fileAst =
{
  packagename: package option;
  listImport: import list option;
  listClass: classType;
}

type fileType = FileType of fileAst



(* affichage de l'AST *)

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

let string_of_import i = match i with
  | Import({ name=str; isStatic=b }) -> if(b) then "static " ^ str else str

let string_of_inheritance i = match i with
  | Some(i) -> "extends " ^ i
  | None -> ""

let rec string_of_interfaces liste = match liste with
  | Some([]) -> ""
  | Some(x::xs) -> x ^ " " ^ string_of_interfaces(Some(xs))
  | None -> ""

let rec printListImport liste = match liste with
  | Some([]) -> print_endline("End of Import" ^ "\n")
  | Some(x::xs) -> print_endline("Import: " ^ string_of_import(x)); printListImport(Some(xs))
  | None -> print_endline("No import in file")

let printPackage p = match p with
  | Some(Package(str)) -> print_string("File package name: " ^ str ^ "\n")
  | None -> print_string("file package name: none" ^ "\n")

let string_of_typeOf t = match t with
  | String(str) -> str
  | Primitive(Int) -> "int"
  | Primitive(Float) -> "float"
  | Primitive(Double) ->"double"
  | Primitive(Boolean) -> "boolean"
  | Primitive(Char) -> "Char"
  | Primitive(Long) -> "long"
  | Primitive(Byte) -> "byte"
  | Primitive(Short) -> "short"

let string_of_paramater p = match p with
  | { parametertype=typeof; name=str } -> string_of_typeOf(typeof) ^ " " ^ str

let rec string_of_listparameters params = match params with
  | Some([]) -> ""
  | Some(x::xs)-> " " ^ string_of_paramater(x) ^" "^ string_of_listparameters(Some(xs))
  | None -> ""


let rec string_of_statements stmts = match stmts with
  | [] -> ""
  | (x::xs) -> "\t" ^ "\t"  ^ "\t" ^ AstExpr.string_of_statement(x) ^ "\n" ^ string_of_statements(xs)

let string_of_thisorsuper str = match str with
  | This -> "this"
  | Super -> "super"

let string_of_constructorinvocation inv = match inv with
  | Some({ invocator = i ; parameters=params }) -> "\t" ^ "\t" ^ "\t" ^ string_of_thisorsuper(i) ^ "(" ^ string_of_listparameters(params) ^ ")"
  | None -> ""


let string_of_constructorBody body = match body with
  | Some( { liststatements=BlockStatements(stmts); invocation=inv } )-> string_of_constructorinvocation(inv) ^ "\n" ^ string_of_statements stmts
  | None -> "\t" ^ "\t" ^ "\t"  ^ "No body"


let string_of_constructor c = match c with
  | { name=str; access= modi; constructorbody=body } -> "\t" ^ "constructor de class: " ^ str ^ ", access: " ^ string_of_modifiers(modi) ^ "\n" ^ "\t" ^ "\t" ^  "constructor body: " ^ string_of_constructorBody(body)

let string_of_classmember c = match c with
  | MethodClass( { name=str; access=modi; methodbody=Some(BlockStatements(body))  }) -> "\tMethod: " ^ str ^ ", access: " ^ string_of_modifiers(modi) ^ "\n \t\tMethod Body : \n" ^ string_of_statements(body)

let printClassDeclaration decl = match decl with
  | ConstructorType(constructor) -> print_endline(string_of_constructor(constructor))
  | ClassMemberType(member) -> print_endline(string_of_classmember(member))

let rec printClassBodyDeclarations liste = match liste with
  | [] -> print_endline("end of declarations")
  | (x::xs) -> printClassDeclaration(x) ; printClassBodyDeclarations(xs)

let printClassTree c = match c with
  | ClassType({classename=name; access=acc; inheritance=herit; interfaces=listeinterface; classbody=Some(body)}) -> print_endline(string_of_modifiers(acc) ^ "class " ^ name ^ " " ^ string_of_inheritance(herit) ^ " implements "^ string_of_interfaces(listeinterface)); printClassBodyDeclarations(body)
  | InterfaceType({interfacename=name; access=acc}) -> print_endline( "Interface: " ^ name ^ ", " ^ string_of_modifiers(acc))

let printFileTree c = match c with
  | FileType({packagename=package; listImport=imports; listClass=classes}) -> printPackage(package); printListImport(imports); printClassTree(classes)
