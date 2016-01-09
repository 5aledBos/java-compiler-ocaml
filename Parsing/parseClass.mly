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
%token PUBLIC PROTECTED PRIVATE STATIC ABSTRACT FINAL STRICTFP
%token IMPORT PACKAGE
%token EXTENDS IMPLEMENTS
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
  | packname=packageDeclaration? imp=importDeclarations? str=classOrElseDeclaration { FileType({packagename=packname; listImport=imp; listClass=str; })}

packageDeclaration:
  | PACKAGE str=packageName SC { Package(str) }

packageName:
  | str = typeName { str }

importDeclarations:
  | str=importDeclaration { [Import(str)] }
  | p=importDeclarations str=importDeclaration 	{ p @ [Import(str)] }

importDeclaration:
  | decl = 	singleTypeImportDeclaration			{ decl }
  | decl = typeImportOnDemandDeclaration		{ decl }
  | decl = singleStaticImportDeclaration		{ decl }
  | decl = staticImportOnDemandDeclaration		{ decl }

singleTypeImportDeclaration:
  | IMPORT str=typeName SC { { name=str; isStatic=false } }

typeImportOnDemandDeclaration:
  | IMPORT p=typeName POINT TIMES SC		{ { name=p ^ ".*"; isStatic=false } }

singleStaticImportDeclaration:
  | IMPORT STATIC p=typeName SC { { name=p; isStatic=true} }

staticImportOnDemandDeclaration:
  | IMPORT STATIC p=typeName POINT TIMES SC { { name=p ^ ".*"; isStatic=true } }

typeName:
  | str=IDENT	{ str }
  | str=typeName POINT str2=IDENT	{ str ^ "." ^ str2 }


classOrElseDeclaration:
  | decl = classDeclaration { decl }
  | decl = interfaceDeclaration { decl }
(*  | decl = enumDeclaration	{ }*)

classDeclaration:
  | modi=classModifiers? CLASS id=IDENT leg=super? listeinterface= interfaces?  LBRACE body=classBody? RBRACE EOF    { ClassType {classename = id; access = modi; classbody = body; inheritance=leg; interfaces = listeinterface  } }


(*classModifiers:*)
(*  | m=classModifier		{ [m]}*)
(*  | liste=classModifiers m=classModifier	{ liste @ [m] }*)

(*classModifier:*)
(*  | m=accessModifier	{ m }*)
(*  | m=modifier		{ m }*)

interfaceDeclaration:
  | modi=classModifiers? INTERFACE id=IDENT LBRACE str=classBody? RBRACE EOF    { InterfaceType{interfacename = id; access = modi} }

(*TODO*)
(*enumDeclaration:*)
(*  | *)

classBody:
  | body=classBodyDeclarations { body }

classBodyDeclarations:
  | decl = classBodyDeclaration	{ [decl] }
  | decls = classBodyDeclarations decl = classBodyDeclaration { decls @ [decl] }

classBodyDeclaration:
  | decl = classMemberDeclaration	{ ClassMemberType(decl) } (*TODO* faire types pour celui là *)
(*  | instanceInitializer		{}*)
(*  | staticInitializer		{}*)
  | constructor = constructorDeclaration	{ constructor }

(*classMemberDeclaration*)

classMemberDeclaration:
(*  | nestedClass			{}*)
(*  | nestedInterface		{}*)
  | decl = methodDeclaration		{ MethodClass(decl) }
(*  | attribut = attributDeclaration		{  }*)

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
  | modi=classModifiers? result=constructorDeclarator LBRACE body=constructorBody? RBRACE	{ match result with
																					| (str, parameterliste) -> ConstructorType{name = str; access = modi; parameters = parameterliste; constructorbody = body } }
 
constructorDeclarator:
  | str=IDENT LPAR parameters = formalParameterList? RPAR	{ str, parameters  }

constructorModifiers:
  | modifier		{ }

constructorBody:
  | inv=explicitConstructorInvocation? stmts = blockstmts	{ { liststatements = stmts;  invocation=inv } }		(* blockstatements peut etre à redéfinir dans Expr*)



explicitConstructorInvocation: 
  | THIS LPAR liste=argumentList? RPAR SC	{ { invocator=This; argumentlist=liste } }
  | SUPER LPAR liste=argumentList? RPAR SC		{ {invocator=Super; argumentlist=liste } }
(*  | PRIMARY POINT SUPER parameterList? RBRACE SC*)

(*attributDeclaration:*)
(*  | atr=attributDeclaration primitive str=IDENT SC {  atr ^ "\n" ^"primitive " ^str }*)
(*  | primitive str=IDENT SC	{ "primitive " ^str }*)

(* déclaration de méthodes*)
methodDeclaration:
  | decl=methodHeader LBRACE body=methodBody? RBRACE { match decl with
													| (modi, (str, liste) ) -> { name=str; access=modi;methodbody=body; parameters=liste} } 

methodHeader:
  | modi=classModifiers? result temp=methodDeclarator	{ modi, temp }

methodDeclarator:
  | str=IDENT LPAR liste=formalParameterList? RPAR	{ str, liste }

methodModifiers:
  | modifier	{ }

methodBody:
  | stmts = blockstmts { stmts }	
(*  | SC {  }*)

blockstmts:
  | stmts = statements	{ BlockStatements(stmts) }

result:
  | VOID 	{}
  | typeDeclaration	{ }

(*instanceInitializer*)
(*instanceInitializer:*)
(*  | str=IDENT str=IDENT EQUAL *)


(* utilisé par les ClassBody*)

formalParameterList:
  | p=lastFormalParameter 	{ [p] }
  | liste = formalParameters COMA p=lastFormalParameter	{ liste @ [p] }

formalParameters:
  | p=formalParameter		{ [p] }
  | liste=formalParameters COMA p=formalParameter		{ liste @ [p]}

lastFormalParameter:
  | variableModifiers? var=variableType POINT POINT POINT id=variableDeclaratorId	{ { name = id; parametertype=var} }
  | p=formalParameter		{ p }

formalParameter:
  | variableModifiers? var=variableType id=variableDeclaratorId		{ { name = id; parametertype=var} }


variableModifiers:
  | variableModifier	{ }
  | variableModifiers variableModifier		{}

variableModifier:
  | f = FINAL 	{ Final }

variableDeclaratorId:
  | str=IDENT		{ str }
  | str=variableDeclaratorId LBRACKET RBRACKET		{ str ^ "[]" }

variableType:
  | str=typeDeclaration { str }


parameterList:
  | p=parameter			{ [p] }
  | params=parameterList COMA p=parameter	{ params @ [p] }


parameter:
  | t=typeDeclaration str=IDENT	{ {parametertype=t; name=str} }




typeDeclaration:
  | p=primitive	{ AstClass.Primitive(p) }
  | str=IDENT	{ AstClass.String(str) }

argumentList:
  | e=expression	{ [e] }
  | liste=argumentList COMA e=expression { liste @ [e] }



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

interfaceModifiers:
  | m=interfaceModifier		{ [m]}
  | liste=interfaceModifiers m=interfaceModifier	{ liste @ [m] }

interfaceModifier:
  | m=accessModifier	{ m }
  | m=modifier		{ m }

classModifiers:
  | m=classModifier		{ [m]}
  | liste=classModifiers m=classModifier	{ liste @ [m] }

classModifier:
  | m=accessModifier	{ m }
  | m=modifier		{ m }
  | m=modifierFinal { m }

accessModifier:
  | PUBLIC { AstClass.Public }
  | PROTECTED { AstClass.Protected }
  | PRIVATE { AstClass.Private }

modifier:
  | ABSTRACT		{ Abstract }
  | STATIC			{ Static }
  | STRICTFP		{ Strictfp }

modifierFinal:
  | FINAL			{ Final }

(*modifierStatic:*)
(*  | STATIC 	{ Static }*)

(*modifierAbstract:*)
(*  | ABSTRACT { Abstract }*)



super:
  | EXTENDS str=classType { str }

interfaces:
  | IMPLEMENTS liste=interfaceTypeList  { liste }

interfaceTypeList:
  | str= interfaceType		{ [str] }
  | liste=interfaceTypeList COMA str=interfaceType 	{ liste @ [str] }

interfaceType:
  | str=IDENT	{ str }

classType:
  | str=IDENT	{ str }

primitive:
  | PINT { AstClass.Int } 



(*attribut*)

