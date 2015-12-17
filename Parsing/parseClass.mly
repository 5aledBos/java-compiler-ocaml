%{
  open AstClass
%}




/**************/
/* The tokens */
/**************/

/* Separators */
%token EOF EOL LBRACE RBRACE LBRACKET RBRACKET SC EQUAL EPOINT

/* Literal values */


/* Identifiers */
%token <string> IDENT
%token CLASS
%token PUBLIC PROTECTED PRIVATE 
%token IMPORT PACKAGE
%token EXTENDS IMPLEMENTS ABSTRACT
%token RETURN
%token INT FLOAT DOUBLE BOOLEAN VOID
%token IF WHILE FOR ELSE


%start filecontent

%type < AstClass.classAst > filecontent
%type <AstClass.classAst > classe
%type < string > content

%%

filecontent: 
  | st=packageDeclaration imp=importDeclaration str=classDeclaration { str }
(*  | pa=packageDeclaration str=classDeclaration { { classename =pa^ "\n" ^ str } }*)
(*  | imp=importDeclaration str=classDeclaration { { classename =imp^ "\n" ^ str } }*)
(*  | str=classDeclaration   {{ classename = str }}*)



packageDeclaration:
  | p=packageDeclaration PACKAGE str=IDENT SC { p^"\n"^"package " ^str }
  | PACKAGE str=IDENT SC { "package " ^str }

importDeclaration:
  | p=importDeclaration IMPORT str=IDENT SC {p^"\n"^"import " ^str }
  | IMPORT str=IDENT SC { "import " ^str }

classDeclaration:
  | str=classe { str }

classe:
(*  | CLASS id=IDENT LBRACE RBRACE EOF    { "classe " ^id }*)
(*  | CLASS id=IDENT legacy  LBRACE RBRACE EOF    {"classe " ^ id }*)
(*  | modifier CLASS id=IDENT LBRACE str=content RBRACE EOF    {"classe " ^ id ^ "\n" ^ str }*)
  | modi=modifier CLASS id=IDENT legacy?  LBRACE str=content RBRACE EOF    { { classename = "\n" ^ str; access = modi } }
(*  | CLASS id=IDENT LBRACE str=content RBRACE EOF    {"classe " ^ id ^ "\n" ^ str }*)

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
  | modifier primitive str=IDENT LBRACKET RBRACKET SC {"methode " ^ str }
  | modifier primitive str=IDENT LBRACKET primitive stri=IDENT  RBRACKET SC {"methode " ^ stri ^ "\n" ^ str }
  | modifier primitive id=IDENT LBRACKET RBRACKET  LBRACE str=methode RBRACE  { "methode " ^ id ^ "\n" ^ str }
  | modifier primitive id=IDENT LBRACKET primitive stri=IDENT RBRACKET  LBRACE str=methode RBRACE  {"methode " ^id^ "\n" ^  str }

methode:
  | m=methode str=boucle {m^ "\n" ^ str}
  | m=methode str=contenuMethode {m ^ "\n" ^ str}
  | str=boucle {str}
  | str=contenuMethode {str}

boucle:
  | IF LBRACKET stri=condition RBRACKET  LBRACE str=contenuMethode RBRACE { str}
   
 
contenuMethode:
  | c=contenuMethode  str=egalite {c^ "\n" ^ str}
  | c=contenuMethode  RETURN str=IDENT SC  { c^ "\n " ^"return " ^ str}
  | str=egalite {"egalite " ^ str}
  | RETURN str=IDENT SC  {"return " ^  str}


condition:
  | str=IDENT { str } 
  | EPOINT  str=IDENT { str } 

(* Caracteres speciaux *)

egalite:
  | str=IDENT EQUAL stri=IDENT SC {str^ " egale " ^ stri}

modifier:
  | PUBLIC { AstClass.Public }
  | PROTECTED { AstClass.Protected }
  | PRIVATE { AstClass.Public }

legacy:
  | EXTENDS str=IDENT {}
  | IMPLEMENTS str=IDENT {}

primitive:
  | INT {} | FLOAT {}



(*attribut*)

