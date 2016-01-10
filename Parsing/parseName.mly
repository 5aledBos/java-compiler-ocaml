%{

%}

/******************************/
/* Entry points of the parser */
/******************************/

%start packageName
%type <AstExpr.expression> packageName

(*%start typeName*)
(*%type <AstExpr.expression> typeName*)

%start expressionName
%type <AstExpr.expression> expressionName

%start methodName
%type <AstExpr.expression> methodName

%start packageOrTypeName
%type <AstExpr.expression> packageOrTypeName

%start ambiguousName
%type <AstExpr.expression> ambiguousName

%start className
%type <AstExpr.expression> className

%%

/*********/
/* Rules */
/*********/

(* NAMES *)

packageName:
  | id = IDENT                                { Var id }
  | pn = packageName POINT id = IDENT         { Name(pn, id) }

typeName:
  | id = IDENT                                { Var id }
  | ptn = packageOrTypeName POINT id = IDENT  { Name(ptn, id) }

(*expressionName:*)
(*  | id = IDENT                                { Var id }*)
(*  | an = ambiguousName POINT id = IDENT       { Name(an, id) }*)

(*methodName:*)
(*  | id = IDENT                                { Var id }*)
(*  | an = ambiguousName POINT id = IDENT       { Name(an, id) }*)

(*packageOrTypeName:*)
(*  | id = IDENT                                { Var id }*)
(*  | ptn = packageOrTypeName POINT id = IDENT  { Name(ptn, id) }*)

(*ambiguousName:*)
(*  | id = IDENT                                { Var id }*)
(*  | an = ambiguousName POINT id = IDENT       { Name(an, id) }*)

(*TODO: check in the Java spec*)
(*className:*)
(*  | id = IDENT                                { Var id }*)

%%
