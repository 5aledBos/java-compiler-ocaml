%{

%}

/******************************/
/* Entry points of the parser */
/******************************/

%start pathName
%type <AstExpr.expression> pathName

%%

/*********/
/* Rules */
/*********/

(* NAMES *)

pathName:
  | l = separated_nonempty_list(POINT, IDENT)       { Name(List.map var_of_string l) }

(*TODO: check in the Java spec*)
(*className:*)
(*  | id = IDENT                                { Var id }*)

%%
