%{

%}

/******************************/
/* Entry points of the parser */
/******************************/

%start pathName
%type <AstExpr.expression> pathName

%start typeName
%type <AstExpr.expression> typeName

%%

/*********/
/* Rules */
/*********/

(* NAMES *)

pathName:
  | l = separated_nonempty_list(POINT, IDENT)       { Name(List.map var_of_string l) }

typeName:
  | id = IDENT                                { Var id }
  | ptn = typeName POINT id = IDENT  { Name([ptn] @ [Var(id)]) }

%%
