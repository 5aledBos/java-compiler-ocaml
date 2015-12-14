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

%%

filecontent: 
  | packageDeclaration importDeclaration str=classDeclaration { str }

classe:
   | CLASS id=IDENT LBRACE RBRACE EOF    { id }

packageDeclaration:
  | PACKAGE str=IDENT SEMICOLON { str }

importDeclaration:
  | IMPORT str=IDENT SEMICOLON {}

classDeclaration:
  | str=classe { str }


