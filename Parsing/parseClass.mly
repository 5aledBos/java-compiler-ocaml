%{
  open AstClass
%}	




/**************/
/* The tokens */
/**************/

/* Separators */
%token EOF LBRACE RBRACE LPAR RPAR SC EQUAL NEQUAL

/* Literal values */


/* Identifiers */
%token <string> IDENT
%token CLASS INTERFACE
%token PUBLIC PROTECTED PRIVATE 
%token IMPORT PACKAGE
%token EXTENDS IMPLEMENTS ABSTRACT
%token RETURN
%token PINT
%token POINT

%start filecontent

%type < AstClass.fileType > filecontent

(*%type < string > content*)

%%

filecontent: 
  | packname=packageDeclaration? imp=importDeclaration? str=classOrElseDeclaration { FileType({packagename=packname; listImport=imp; listClass=str; })}

packageDeclaration:
  | PACKAGE str=packageName SC { Package(str) }

packageName:
  | pack=packageName str=IDENT { pack ^ str }
  | str=IDENT p=POINT? 	{ match p with
 				| None -> str
				| _ -> str ^ "." }

importDeclaration:
  | p=importDeclaration IMPORT str=IDENT SC { Import(str) :: p }
  | IMPORT str=IDENT SC { [Import(str)] }



classOrElseDeclaration:
  | decl = classDeclaration { decl }
  | decl = interfaceDeclaration { decl }

classDeclaration:
  | modi=modifier? CLASS id=IDENT legacy? inheritance?  LBRACE classBody? RBRACE EOF    { ClassType{classename = id; access = modi; } }

interfaceDeclaration:
  | modi=modifier? INTERFACE id=IDENT LBRACE str=classBody? RBRACE EOF    { InterfaceType{interfacename = id; access = modi} }


classBody:
  | classBodyDeclarations { }
(*  | statements		{ }*)

classBodyDeclarations:
  | classBodyDeclaration	{ }
  | classBodyDeclarations classBodyDeclaration { }

classBodyDeclaration:
  | classMemberDeclaration	{}
(*  | instanceInitializer		{}*)
(*  | staticInitializer		{}*)
  | constructorDeclaration	{}

(*classMemberDeclaration*)

classMemberDeclaration:
(*  | nestedClass			{}*)
(*  | nestedInterface		{}*)
(*  | methodDeclaration		{}*)
  | attributDeclaration		{}

	(*declaration des attributs*)
attributDeclaration:
  | modifier? typeDeclaration variableDeclarators SC	{}

variableDeclarators:
  | variableDeclarator					{}
  | variableDeclarators COMA variableDeclarator 	{}

variableDeclarator:
  | str=IDENT 	{}
  | str=IDENT EQUAL variableInitializer	{}

variableInitializer:
  | statements	{ }
  

attributModifiers:
  | modifier	{ }

(*déclaration de constructeurs* Rq: manque encore modifer dans les paramètres*)
constructorDeclaration:
  | modifier? constructorDeclarator LBRACE constructorBody? RBRACE	{ }
 
constructorDeclarator:
  | str=IDENT LPAR parameterList? RPAR	{ }

constructorModifiers:
  | modifier		{ }

parameterList:
  | parameter			{}
  | parameterList COMA parameter	{}

parameter:
  | typeDeclaration str=IDENT	{ }

constructorBody:
  | statements 	{ }

(*attributDeclaration:*)
(*  | atr=attributDeclaration primitive str=IDENT SC {  atr ^ "\n" ^"primitive " ^str }*)
(*  | primitive str=IDENT SC	{ "primitive " ^str }*)



(* utilisé par les ClassBody*)
typeDeclaration:
  | primitive	{}
  | str=IDENT	{}


content:
  | str=declaration {  }

declaration:
  | statements {  }
(*  | d=declaration str=attributDeclaration { }*)
(*  | d=declaration str=methodeDeclaration {  }*)
(*  | str=methodeDeclaration {  }*)
(*  | str=attributDeclaration {}*)


(*methodDeclaration:*)
(*  | modifier primitive str=IDENT LPAR RPAR SC {"methode " ^ str }*)
(*  | modifier primitive str=IDENT LPAR primitive stri=IDENT  RPAR SC {"methode " ^ stri ^ "\n" ^ str }*)
(*  | modifier primitive id=IDENT LPAR RPAR  LBRACE str=methode RBRACE  { "methode " ^ id ^ "\n" ^ str }*)
(*  | modifier primitive id=IDENT LPAR primitive stri=IDENT RPAR  LBRACE str=methode RBRACE  {"methode " ^id^ "\n" ^  str }*)

(*methode:*)
(*  | m=methode str=boucle {m^ "\n" ^ str}*)
(*  | m=methode str=contenuMethode {m ^ "\n" ^ str}*)
(*  | str=boucle {str}*)
(*  | str=contenuMethode {str}*)

(*boucle:*)
(*  | IF LPAR stri=condition RPAR  LBRACE str=contenuMethode RBRACE { str}*)
(*   *)
(* *)
(*contenuMethode:*)
(*  | c=contenuMethode  str=egalite {c^ "\n" ^ str}*)
(*  | c=contenuMethode  RETURN str=IDENT SC  { c^ "\n " ^"return " ^ str}*)
(*  | str=egalite {"egalite " ^ str}*)
(*  | RETURN str=IDENT SC  {"return " ^  str}*)


(*condition:*)
(*  | str=IDENT { str } *)
(*  | NEQUAL  str=IDENT { str } *)

(* Caracteres speciaux *)

(*egalite:*)
(*  | str=IDENT EQUAL stri=IDENT SC {str^ " egale " ^ stri}*)

modifier:
  | PUBLIC { AstClass.Public }
  | PROTECTED { AstClass.Protected }
  | PRIVATE { AstClass.Private }

legacy:
  | EXTENDS str=IDENT {}

inheritance:
  | IMPLEMENTS interfaces {}

interfaces:
  | interfaces str=IDENT {}
  | str=IDENT COMA? {}

primitive:
  | PINT {} 



(*attribut*)

