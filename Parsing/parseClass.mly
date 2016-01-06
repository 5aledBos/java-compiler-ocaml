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
%token CLASS
%token PUBLIC PROTECTED PRIVATE 
%token IMPORT PACKAGE
%token EXTENDS IMPLEMENTS ABSTRACT
%token RETURN
%token PINT
%token POINT

%start filecontent

%type < AstClass.fileType > filecontent

%type < string > content

%%

filecontent: 
  | packname=packageDeclaration? imp=importDeclaration? str=classDeclaration { FileType({packagename=packname; listImport=imp; listClass=str; })}

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



classDeclaration:
  | modi=modifier? CLASS id=IDENT legacy? inheritance?  LBRACE str=content? RBRACE EOF    { ClassType{classename = id; access = modi} }

(*interfaceDeclaration:*)
(*  | modi=modifier CLASS id=IDENT LBRACE str=content RBRACE EOF    { ClassType{classename = id; access = modi} }*)

content:
  | str=declaration { str }

declaration:
  | d=declaration str=attributDeclaration { d ^ "\n" ^ str }
  | d=declaration str=methodeDeclaration { d ^ "\n" ^ str }
  | str=methodeDeclaration { str }
  | str=attributDeclaration { str }

attributDeclaration:
  | atr=attributDeclaration primitive str=IDENT SC {  atr ^ "\n" ^"primitive " ^str }
  | primitive str=IDENT SC	{ "primitive " ^str }

methodeDeclaration:
  | modifier primitive str=IDENT LPAR RPAR SC {"methode " ^ str }
  | modifier primitive str=IDENT LPAR primitive stri=IDENT  RPAR SC {"methode " ^ stri ^ "\n" ^ str }
  | modifier primitive id=IDENT LPAR RPAR  LBRACE str=methode RBRACE  { "methode " ^ id ^ "\n" ^ str }
  | modifier primitive id=IDENT LPAR primitive stri=IDENT RPAR  LBRACE str=methode RBRACE  {"methode " ^id^ "\n" ^  str }

methode:
  | m=methode str=boucle {m^ "\n" ^ str}
  | m=methode str=contenuMethode {m ^ "\n" ^ str}
  | str=boucle {str}
  | str=contenuMethode {str}

boucle:
  | IF LPAR stri=condition RPAR  LBRACE str=contenuMethode RBRACE { str}
   
 
contenuMethode:
  | c=contenuMethode  str=egalite {c^ "\n" ^ str}
  | c=contenuMethode  RETURN str=IDENT SC  { c^ "\n " ^"return " ^ str}
  | str=egalite {"egalite " ^ str}
  | RETURN str=IDENT SC  {"return " ^  str}


condition:
  | str=IDENT { str } 
  | NEQUAL  str=IDENT { str } 

(* Caracteres speciaux *)

egalite:
  | str=IDENT EQUAL stri=IDENT SC {str^ " egale " ^ stri}

modifier:
  | PUBLIC { AstClass.Public }
  | PROTECTED { AstClass.Protected }
  | PRIVATE { AstClass.Public }

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

