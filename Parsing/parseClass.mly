%{

%}




/**************/
/* The tokens */
/**************/

/* Separators */
%token EOF EOL LBRACE RBRACE SC

/* Literal values */


/* Identifiers */
%token <string> IDENT
%token CLASS
%token PUBLIC PROTECTED PRIVATE
%token IMPORT PACKAGE	


%start filecontent

%type < string > filecontent
%type < string > classe
%type < string > content

%%

filecontent: 
  | st=packageDeclaration importDeclaration str=classDeclaration { st }
  | packageDeclaration str=classDeclaration { str }
  | importDeclaration str=classDeclaration { str }
  | str=classDeclaration   { str }

classe:
   | CLASS id=IDENT LBRACE RBRACE EOF    { id }
   | modifier CLASS id=IDENT LBRACE str=content RBRACE EOF    { str }

packageDeclaration:
  | packageDeclaration PACKAGE str=IDENT SC { str }
  | PACKAGE str=IDENT SC { str }

importDeclaration:
  | importDeclaration IMPORT str=IDENT SC { str }
  | IMPORT str=IDENT SC { str }

classDeclaration:
  | str=classe { str }

modifier:
  | PUBLIC {}
  | PROTECTED {}
  | PRIVATE {}

content:
  | content str=IDENT SC { str }
  | str=IDENT { str }	
