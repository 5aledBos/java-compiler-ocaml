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
%token EXTENDS IMPLEMENTS RETURN
%token INT FLOAT DOUBLE BOOLEAN VOID
%token IF WHILE FOR ELSE


%start filecontent

%type < AstClass.classAst > filecontent
%type < string > classe
%type < string > content

%%

filecontent: 

  | st=packageDeclaration importDeclaration str=classDeclaration { { classename=str } }
  | packageDeclaration str=classDeclaration { { classename=str } }
  | importDeclaration str=classDeclaration { { classename=str } }
  | str=classDeclaration   { { classename=str } }


(*=======*)
(*  | st=packageDeclaration imp=importDeclaration str=classDeclaration { st^ "\n" ^ imp^ "\n" ^ str }*)
(*  | pa=packageDeclaration str=classDeclaration { pa^ "\n" ^ str }*)
(*  | imp=importDeclaration str=classDeclaration { imp^ "\n" ^ str }*)
(*  | str=classDeclaration   { str }*)

packageDeclaration:
  | p=packageDeclaration PACKAGE str=IDENT SC { p^"\n"^"package " ^str }
  | PACKAGE str=IDENT SC { "package " ^str }

importDeclaration:
  | p=importDeclaration IMPORT str=IDENT SC {p^"\n"^"import " ^str }
  | IMPORT str=IDENT SC { "import " ^str }

classDeclaration:
  | str=classe { str }

classe:
  | CLASS id=IDENT LBRACE RBRACE EOF    { id }
  | CLASS id=IDENT legacy  LBRACE RBRACE EOF    { id }
  | modifier CLASS id=IDENT LBRACE str=content RBRACE EOF    { str }
  | modifier CLASS id=IDENT legacy  LBRACE str=content RBRACE EOF    { str }
  | CLASS id=IDENT LBRACE str=content RBRACE EOF    { id }
(*=======*)
(*  | CLASS id=IDENT LBRACE RBRACE EOF    { "classe " ^id }*)
(*  | CLASS id=IDENT legacy  LBRACE RBRACE EOF    {"classe " ^ id }*)
(*  | modifier CLASS id=IDENT LBRACE str=content RBRACE EOF    {"classe " ^ id ^ "\n" ^ str }*)
(*  | modifier CLASS id=IDENT legacy  LBRACE str=content RBRACE EOF    {"classe " ^ id ^ "\n" ^ str }*)
(*  | CLASS id=IDENT LBRACE str=content RBRACE EOF    {"classe " ^ id ^ "\n" ^ str }*)
(*>>>>>>> 7f18f222f894ac8b42a2e980a063769fe12b8a15*)

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
  | modifier primitive str=IDENT LBRACKET RBRACKET SC { str }
  | modifier primitive str=IDENT LBRACKET primitive stri=IDENT  RBRACKET SC { stri }
  | modifier primitive IDENT LBRACKET RBRACKET  LBRACE str=methode RBRACE  { str }
  | modifier primitive IDENT LBRACKET primitive stri=IDENT RBRACKET  LBRACE str=methode RBRACE  { str }

methode:
  | methode str=boucle {str}
  | methode str=contenuMethode {str}
  | modifier primitive str=IDENT LBRACKET RBRACKET SC {"methode " ^ str }
  | modifier primitive str=IDENT LBRACKET primitive stri=IDENT  RBRACKET SC {"methode " ^ stri ^ "\n" ^ str }
  | modifier primitive id=IDENT LBRACKET RBRACKET  LBRACE str=methode RBRACE  { "methode " ^ id ^ "\n" ^ str }
  | modifier primitive id=IDENT LBRACKET primitive stri=IDENT RBRACKET  LBRACE str=methode RBRACE  {"methode " ^id^ "\n" ^  str }

(*methode:*)
(*  | m=methode str=boucle {m^ "\n" ^ str}*)
(*  | m=methode str=contenuMethode {m ^ "\n" ^ str}*)
(*  | str=boucle {str}*)
(*  | str=contenuMethode {str}*)

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
  | c=contenuMethode  str=egalite {c^ "\n" ^ str}
  | c=contenuMethode  RETURN str=IDENT SC  { c^ "\n " ^"return " ^ str}
  | str=egalite {"egalite " ^ str}
  | RETURN str=IDENT SC  {"return " ^  str}

(*egalite:*)
(*  | str=IDENT EQUAL stri=IDENT SC {stri}*)


(*condition:*)
(*  | str=IDENT { str } *)
(*  | EPOINT  str=IDENT { str } *)

(* Caracteres speciaux *)

egalite:
  | str=IDENT EQUAL stri=IDENT SC {str^ " egale " ^ stri}

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

