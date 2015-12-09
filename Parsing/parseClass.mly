%{

%}




/**************/
/* The tokens */
/**************/

/* Separators */
%token EOF EOL LBRACE RBRACE

/* Literal values */


/* Identifiers */
%token <string> IDENT
%token CLASS


%start classe

%type < string > classe

%%

classe:
   | CLASS id=IDENT LBRACE RBRACE EOF    { id }

