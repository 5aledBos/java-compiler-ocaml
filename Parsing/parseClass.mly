%{
  open AstClass
%}	




/**************/
/* The tokens */
/**************/

/* Separators */
%token EOF
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET
%token SC EQUAL NEQUAL

/* Literal values */


/* Identifiers */
%token <string> IDENT
%token CLASS INTERFACE
%token PUBLIC PROTECTED PRIVATE 
%token IMPORT PACKAGE
%token EXTENDS IMPLEMENTS ABSTRACT
%token THIS SUPER
%token RETURN
%token PINT
%token POINT
%token VOID

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
  | IMPORT str=IDENT SC { [Import(str)] }
  | p=importDeclaration IMPORT str=IDENT SC {  p @ [Import(str)] }


classOrElseDeclaration:
  | decl = classDeclaration { decl }
  | decl = interfaceDeclaration { decl }
(*  | decl = enumDeclaration	{ }*)

classDeclaration:
  | modi=modifier? CLASS id=IDENT legacy? inheritance?  LBRACE body=classBody? RBRACE EOF    { ClassType {classename = id; access = modi; classbody = body;  } }

interfaceDeclaration:
  | modi=modifier? INTERFACE id=IDENT LBRACE str=classBody? RBRACE EOF    { InterfaceType{interfacename = id; access = modi} }

(*TODO*)
(*enumDeclaration:*)
(*  | *)

classBody:
  | body=classBodyDeclarations { body }

classBodyDeclarations:
  | decl = classBodyDeclaration	{ [decl] }
  | decls = classBodyDeclarations decl = classBodyDeclaration { decls @ [decl] }

classBodyDeclaration:
(*  | decl = classMemberDeclaration	{ }*) (*TODO* faire types pour celui là *)
(*  | instanceInitializer		{}*)
(*  | staticInitializer		{}*)
  | constructor = constructorDeclaration	{ constructor }

(*classMemberDeclaration*)

classMemberDeclaration:
(*  | nestedClass			{}*)
(*  | nestedInterface		{}*)
  | methodDeclaration		{ }
  | attribut = attributDeclaration		{  }

	(*declaration des attributs*)
attributDeclaration:
  | modifier? typeDeclaration listDecl = variableDeclarators SC	{ listDecl }

variableDeclarators:
  | str=variableDeclarator					{ [str] }
  | listdecl = variableDeclarators COMA str=variableDeclarator 	{ listdecl @ [str] }

variableDeclarator:
  | str=IDENT 	{ str }
  | str=IDENT EQUAL variableInitializer	{ str }

variableInitializer:
  | statements	{ }
  

attributModifiers:
  | modifier	{ }

(*déclaration de constructeurs* Rq: manque encore modifer dans les paramètres*)
constructorDeclaration:
  | modi=modifier? result=constructorDeclarator LBRACE body=constructorBody? RBRACE	{ match result with
																					| (str, parameters) -> ConstructorType{name = str; access = modi; constructorbody = body } }
 
constructorDeclarator:
  | str=IDENT LPAR parameters = parameterList? RPAR	{ str, parameters  }

constructorModifiers:
  | modifier		{ }

constructorBody:
  | explicitConstructorInvocation? stmts = blockstmts	{ { liststatements = stmts } }		(* blockstatements peut etre à redéfinir dans Expr*)

blockstmts:
  | stmts = statements	{ BlockStatements(stmts) }

explicitConstructorInvocation: 
  | THIS LPAR parameterList? RPAR SC	{ }
  | SUPER LPAR parameterList? RPAR SC		{ }
(*  | PRIMARY POINT SUPER parameterList? RBRACE SC*)

(*attributDeclaration:*)
(*  | atr=attributDeclaration primitive str=IDENT SC {  atr ^ "\n" ^"primitive " ^str }*)
(*  | primitive str=IDENT SC	{ "primitive " ^str }*)

(* déclaration de méthodes*)
methodDeclaration:
  | methodHeader LBRACE methodBody? RBRACE {} 

methodHeader:
  | modifier? result methodDeclarator	{ }

methodDeclarator:
  | str=IDENT LPAR parameterList? RPAR	{}

methodModifiers:
  | modifier	{ }

methodBody:
  | statements { }	

result:
  | VOID 	{}
  | typeDeclaration	{ }

(*instanceInitializer*)
(*instanceInitializer:*)
(*  | str=IDENT str=IDENT EQUAL *)


(* utilisé par les ClassBody*)
typeDeclaration:
  | primitive	{}
  | str=IDENT	{}

parameterList:
  | parameter			{}
  | parameterList COMA parameter	{}

parameter:
  | typeDeclaration str=IDENT	{ }



content:
  | str=declaration {  }

declaration:
  | statements {  }
(*  | d=declaration str=attributDeclaration { }*)
(*  | d=declaration str=methodeDeclaration {  }*)
(*  | str=methodeDeclaration {  }*)
(*  | str=attributDeclaration {}*)


(*boucle:*)
(*  | IF LPAR stri=condition RPAR  LBRACE str=contenuMethode RBRACE { str}*)
(*   *)
(* *)

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

