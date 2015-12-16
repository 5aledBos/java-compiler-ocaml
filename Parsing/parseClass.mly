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
%token EXTENDS IMPLEMENTS
%token INT FLOAT DOUBLE BOOLEAN VOID


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

classe:
  | CLASS id=IDENT LBRACE RBRACE EOF    { id }
  | CLASS id=IDENT legacy  LBRACE RBRACE EOF    { id }
  | modifier CLASS id=IDENT LBRACE str=content RBRACE EOF    { str }
  | modifier CLASS id=IDENT legacy  LBRACE str=content RBRACE EOF    { str }
  | CLASS id=IDENT LBRACE str=content RBRACE EOF    { id }

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

legacy:
  | EXTENDS str=IDENT {}
  | IMPLEMENTS str=IDENT {}


content:
  | str=declaration { str }

declaration:
  | str=attributDeclaration { str }

attributDeclaration:
  | attributDeclaration primitive str=IDENT SC { str }
  | primitive str=IDENT SC	{ str }

primitive:
  | INT {} | FLOAT {}

(*attribut*)

