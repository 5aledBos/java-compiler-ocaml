%{

%}




/**************/
/* The tokens */
/**************/

/* Separators */
%token EOF EOL LBRACE RBRACE SEMICOLON

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
  | packageDeclaration importDeclaration str=classDeclaration { str }
  | packageDeclaration str=classDeclaration { str }
  | importDeclaration str=classDeclaration { str }
  | str=classDeclaration   { str }

classe:
   | CLASS id=IDENT LBRACE RBRACE EOF    { id }
   | modifier CLASS id=IDENT LBRACE str=content RBRACE EOF    { str }

packageDeclaration:
  | PACKAGE str=IDENT SEMICOLON { str }

importDeclaration:
  | IMPORT str=IDENT SEMICOLON {}

classDeclaration:
  | str=classe { str }

modifier:
  | PUBLIC {}
  | PROTECTED {}
  | PRIVATE {}

content:
  | content str=IDENT SEMICOLON { str }
  | str=IDENT { str }	
