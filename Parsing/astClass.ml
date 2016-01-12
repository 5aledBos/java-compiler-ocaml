open AstExpr


type package = Package of AstExpr.expression


type import = Import of importType
and importType =
  {
	isStatic : bool;
 	name : AstExpr.expression;
	isOnDemand: bool
  }
type importList = import list



type resultTypeAst = 
  | AttributType of AstExpr.typ | Void

type parameter = 
{
  parametertype : AstExpr.typ;
  name : AstExpr.expression
}


type attributAst = {
  typeof : AstExpr.typ;
  names : AstExpr.expression list;
  modifiers : AstExpr.modifiers option
}



type attributList = attributAst list

type blockstmts = BlockStatements of AstExpr.statement list

type methodClassType =
{
  name : string;
  access : AstExpr.modifiers option;
  parameters : parameter list option;
  resultType : resultTypeAst;
  methodbody: blockstmts
}



and constructorBodyAst =
{
  invocation : constructorInvocation option;
  liststatements : blockstmts;
}
and constructorInvocation =
  { 
	invocator : thisOrSuper;
	argumentlist: AstExpr.expression list
}
and thisOrSuper =
  |This | Super


and constructorAst =
{
  name : string;
  access : AstExpr.modifiers option;
  parameters : parameter list option;
  constructorbody : constructorBodyAst option;
}


and constructorType = ConstructorType of constructorAst





type classAst =
  {
    classename : string;
    access : AstExpr.modifiers option;
	inheritance : string option;
 	interfaces : string list option;
	classbody : classBodyAst;
  }

and classBodyAst = classBodyDeclaration list option
and classBodyDeclaration =
  | ClassMemberType of classMemberType
  | ConstructorType of constructorAst
  | InstanceInitializerType of blockstmts
  | StaticInitializerType of blockstmts

and classMemberType =
  | MethodClass of methodClassType
  | Attribut of attributAst
  | InnerClass of classType
  | InnerInterface of classType

and classBodyType = 
  | ClassBodyType of classBodyAst

and classContentAst =
{
  listClassMember : classMemberType list;
  listConstructors : constructorType list;
}

and enumBodyAst = 
{
  enumConstants : enumConstant list option;
  enumDeclarations : classBodyDeclaration list option option
}
and enumConstant =
  {
	name : string;
    argumentlist : AstExpr.expression list option;
  }



and enumAst =
  {
    enumname : string;
    access : AstExpr.modifiers option;
 	interfaces : string list option;
	enumbody : enumBodyAst;
  }

and interfaceAst =
{ 
  interfacename : string;
  access : AstExpr.modifiers option;
  interfaceBody : interfaceMemberDeclaration list option
}

and interfaceMemberDeclaration = 
  | InterfaceInnerInterface of classType
  | InterfaceInnerClass of classType
  | AbstractMethodDeclarationType of abstractMethodDeclarationType
  | ConstantDeclarationType

and abstractMethodDeclarationType =
{
  name : string;
  access : AstExpr.modifiers option;
  parameters : parameter list option;
  resultType : resultTypeAst;
} 

and interfaceType = 
  | InterfaceType of interfaceAst

and classType = 
  | ClassType of classAst
  | InterfaceType of interfaceAst
  | EnumType of enumAst

type classList = classType list

type fileAst =
{
  packagename: package option;
  listImport: import list option;
  listClass: classType list option;
}

type fileType = FileType of fileAst



(* affichage de l'AST *)

	(*string des déclarations d'import*)
let string_of_import i = match i with
  | Import({ name=Name(expr); isStatic=b; isOnDemand=c }) -> match (b, c) with
		| (true, false) -> "static " ^ AstExpr.string_of_list "." AstExpr.string_of_expr expr
		| (true, true) -> "static " ^ AstExpr.string_of_list "." AstExpr.string_of_expr expr  ^ ".*"
		| (false, false) -> AstExpr.string_of_list "." AstExpr.string_of_expr expr
		| (false, true) -> AstExpr.string_of_list "." AstExpr.string_of_expr expr  ^ ".*"

let string_of_imports stmts = match stmts with
  | Some(liste) -> "import " ^ string_of_list "\nimport " string_of_import liste
  | None -> ""


	(*string des déclarations d'héritage et d'implémentation d'une classe*)
let string_of_inheritance i = match i with
  | Some(i) -> "extends " ^ i
  | None -> ""

let string_of_interface interface = interface

let string_of_interfaces stmts = match stmts with
  | Some(liste) -> "implements " ^ string_of_list ", " string_of_interface liste
  | None -> ""

			(*string des paramètres d'une méthode, constructeur... *)
let string_of_parameter p = match p with
  | { parametertype=typeof; name=str } -> AstExpr.string_of_type(typeof) ^ " " ^ (AstExpr.string_of_expr str)

let string_of_listparameters stmts = match stmts with
  | Some(liste) -> string_of_list ", " string_of_parameter liste
  | None -> ""


			(*string d'expressions*)
let string_of_statements stmts = string_of_list "\n" AstExpr.string_of_statement stmts

let string_of_expressions stmts = string_of_list "\n" AstExpr.string_of_expr stmts

let string_of_expressions2 stmts = string_of_list ", " AstExpr.string_of_expr stmts

let string_of_expressionsOption stmts = match stmts with
  | Some(liste) -> string_of_list "\n" AstExpr.string_of_expr liste
  | None -> ""



		(*string du reusltat d'une signature de fonction*)
let string_of_resultType t = match t with
  | Void -> "void"
  | AttributType(a) -> AstExpr.string_of_type(a)

				(* string pour les utilisations de constructeurs d'une classe héritée ou d'un autre constructeur*)
let string_of_thisorsuper str = match str with
  | This -> "this"
  | Super -> "super"

let string_of_constructorinvocation inv = match inv with
  | Some({ invocator = i ; argumentlist=params }) -> "\t" ^ "\t" ^ "\t" ^ string_of_thisorsuper(i) ^ "(" ^ string_of_expressions2(params) ^ ")"
  | None -> ""
		
			(*string des constructeurs de méthode*)
let string_of_constructorBody body = match body with
  | Some( { liststatements=BlockStatements(stmts); invocation=inv } )-> string_of_constructorinvocation(inv) ^ "\n" ^ string_of_statements stmts
  | None -> "\t" ^ "\t" ^ "\t"  ^ "No body"


let string_of_constructor c = match c with
  | { name=str; access= modi; constructorbody=body; parameters= params } -> "\t" ^ "constructor de class: " ^ str ^ "(" ^ string_of_listparameters(params) ^ ")" ^ ", access: " ^ AstExpr.string_of_modifiers(modi) ^ "\n" ^ "\t" ^ "\t" ^  "constructor body:\n" ^ string_of_constructorBody(body)

		(* string du contenus des classes autre que constructeur *)
let string_of_classmember c = match c with
  | MethodClass( { name=str; access=modi; resultType=result; parameters=liste; methodbody=BlockStatements(body)  }) -> "\tMethod: " ^ string_of_resultType(result) ^ " " ^ str ^ "(" ^ string_of_listparameters(liste) ^ "), access: " ^ AstExpr.string_of_modifiers(modi) ^ "\n \t\tMethod Body : \n" ^ string_of_statements(body)
  | Attribut( { typeof=a; names=str; modifiers=modi } ) -> AstExpr.string_of_modifiers(modi) ^ AstExpr.string_of_type(a) ^ " " ^ (AstExpr.string_of_list ", " AstExpr.string_of_expr str)
  | InnerClass(classe) -> "innerclass: " 
  | InnerInterface(interface) -> "innerInterface: "

let string_of_classDeclaration decl = match decl with
  | ConstructorType(constructor) -> string_of_constructor(constructor)
  | ClassMemberType(member) -> string_of_classmember(member)
  | InstanceInitializerType(BlockStatements(blockstmts)) -> string_of_statements(blockstmts)
  | StaticInitializerType(BlockStatements(blockstmts)) -> string_of_statements(blockstmts)

let string_of_classDeclarationOption decl = match decl with
  | Some(ConstructorType(constructor)) -> string_of_constructor(constructor)
  | Some(ClassMemberType(member)) -> string_of_classmember(member)
  | None -> ""

let string_of_classDeclarations liste = match liste with
  | Some(liste) -> string_of_list "\n" string_of_classDeclaration liste
  | None -> ""

		(* string des contenus des enum *)
let string_of_enumBodyDeclarations liste = match liste with
  | Some(liste) -> string_of_classDeclarations(liste)
  | None -> ""

let string_of_enumConstant cons = match cons with
  | { name=str; argumentlist=liste } -> str ^ "(" ^ string_of_expressionsOption(liste) ^ ")"

let string_of_enumConstants liste = match liste with
  | Some(liste) -> string_of_list ", " string_of_enumConstant liste
  | None -> ""

let string_of_enumBody body = match body with
  | {  enumConstants=constants; enumDeclarations=decl } -> string_of_enumConstants(constants) ^ "\n" ^ string_of_enumBodyDeclarations(decl)

let string_of_abstractMethodDeclaration a = match a with
  | { name = methodname; access=modi; parameters = parameterlist; resultType=result } -> string_of_modifiers(modi) ^ string_of_resultType(result) ^ " " ^ methodname ^ " " ^ "(" ^ string_of_listparameters(parameterlist) ^ ")"


	(* string du contenu des interfaces *)
let string_of_interfaceMemberDeclaration i = match i with
  | InterfaceInnerInterface(decl) -> "InnerInterface: "
  | InterfaceInnerClass(decl) -> "InnerClass: "
  | AbstractMethodDeclarationType(decl) -> string_of_abstractMethodDeclaration(decl)
  | ConstantDeclarationType -> ""

let string_of_interfaceMemberDeclarations liste = match liste with
  | Some(liste) -> string_of_list "\n" string_of_interfaceMemberDeclaration liste
  | None -> ""


let printClassDeclaration decl = match decl with
  | ConstructorType(constructor) -> print_endline(string_of_constructor(constructor))
  | ClassMemberType(member) -> print_endline(string_of_classmember(member))


let string_of_classTree classe = match classe with
  | ClassType({classename=name; access=acc; inheritance=herit; interfaces=listeinterface; classbody=body}) -> AstExpr.string_of_modifiers(acc) ^ "class " ^ name ^ " " ^ string_of_inheritance(herit) ^ string_of_interfaces(listeinterface) ^ "\n" ^ string_of_classDeclarations(body)
  | InterfaceType({interfacename=name; access=acc; interfaceBody=body}) -> AstExpr.string_of_modifiers(acc) ^ " interface " ^ name ^ "\n" ^ string_of_interfaceMemberDeclarations(body)
  | EnumType({enumname=name; access=acc; interfaces=listeinterface; enumbody=body}) -> AstExpr.string_of_modifiers(acc) ^ "enum " ^ name ^ string_of_interfaces(listeinterface) ^ "\n"^ string_of_enumBody(body)


let string_of_classes liste = match liste with
  | Some(liste) -> string_of_list "\n" string_of_classTree liste
  | None -> ""
			
		(* string du package du fichier *)
let string_of_package pack = match pack with
  | Some(Package(Name(expr))) -> "Package " ^ AstExpr.string_of_list "\\" AstExpr.string_of_expr expr
  | None -> "No package"

(*let string_of_File file = match file with*)
(*| FileType({packagename=package; listImport=imports; listClass=classes}) -> "Package: " ^ string_of_package(package) ^ string_of_imports(imports) ^ string_of_classes(classes)*)

			(* print de l'AST *)
let printFileTree c = match c with
  | FileType({packagename=package; listImport=imports; listClass=classes}) -> print_endline(string_of_package(package)); print_endline(string_of_imports(imports)); print_endline(string_of_classes(classes))
