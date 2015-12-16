%{

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
%token EXTENDS IMPLEMENTS RETURN
%token INT FLOAT DOUBLE BOOLEAN VOID
%token IF WHILE FOR ELSE


%start filecontent

%type < string > filecontent
%type < string > classe
%type < string > content

%%

filecontent: 
  | st=packageDeclaration importDeclaration str=classDeclaration { str }
  | packageDeclaration str=classDeclaration { str }
  | importDeclaration str=classDeclaration { str }
  | str=classDeclaration   { str }



packageDeclaration:
  | packageDeclaration PACKAGE str=IDENT SC { str }
  | PACKAGE str=IDENT SC { str }

importDeclaration:
  | importDeclaration IMPORT str=IDENT SC { str }
  | IMPORT str=IDENT SC { str }

classDeclaration:
  | str=classe { str }

classe:
  | CLASS id=IDENT LBRACE RBRACE EOF    { id }
  | CLASS id=IDENT legacy  LBRACE RBRACE EOF    { id }
  | modifier CLASS id=IDENT LBRACE str=content RBRACE EOF    { str }
  | modifier CLASS id=IDENT legacy  LBRACE str=content RBRACE EOF    { str }
  | CLASS id=IDENT LBRACE str=content RBRACE EOF    { id }

content:
  | str=declaration { str }

declaration:
  | declaration str=attributDeclaration { str }
  | declaration str=methodeDeclaration { str }
  | str=methodeDeclaration { str }
  | str=attributDeclaration { str }

attributDeclaration:
  | attributDeclaration primitive str=IDENT SC { str }
  | primitive str=IDENT SC	{ str }

methodeDeclaration:
  | modifier primitive str=IDENT LBRACKET RBRACKET SC { str }
  | modifier primitive str=IDENT LBRACKET primitive stri=IDENT  RBRACKET SC { stri }
  | modifier primitive IDENT LBRACKET RBRACKET  LBRACE str=methode RBRACE  { str }
  | modifier primitive IDENT LBRACKET primitive stri=IDENT RBRACKET  LBRACE str=methode RBRACE  { str }

methode:
  | methode str=boucle {str}
  | methode str=contenuMethode {str}
  | str=boucle {str}
  | str=contenuMethode {str}

boucle:
  | IF LBRACKET stri=condition RBRACKET  LBRACE str=contenuMethode RBRACE { str}
   
 
contenuMethode:
  | contenuMethode  str=egalite {str}
  | contenuMethode  RETURN str=IDENT SC  { str}
  | str=egalite {str}
  | RETURN str=IDENT SC  { str}


condition:
  | str=IDENT { str } 
  | EPOINT  str=IDENT { str } 

(* Caracteres speciaux *)

egalite:
  | str=IDENT EQUAL stri=IDENT SC {stri}

modifier:
  | PUBLIC {}
  | PROTECTED {}
  | PRIVATE {}

legacy:
  | EXTENDS str=IDENT {}
  | IMPLEMENTS str=IDENT {}

primitive:
  | INT {} | FLOAT {}



(*attribut*)

