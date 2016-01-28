%{
    open AstExpr
%}

/******************************/
/* Entry points of the parser */
/******************************/

%start typ
%type <AstExpr.typ> typ

%start primitiveType
%type <AstExpr.typ> primitiveType

%start referenceType
%type <AstExpr.typ> referenceType

(*%start classType*)
(*%type <AstExpr.> classType*)

%%

/*********/
/* Rules */
/*********/

(* TYPES *)

typ:
  | pt = primitiveType       { pt }
  | rt = referenceType       { rt }

primitiveType:
  | nt = numericType         { nt }
  | PBOOLEAN                 { Primitive(Boolean) }

numericType:
  | it = integralType        { it }
  | fpt = floatingPointType  { fpt }

integralType:
  | PBYTE       { Primitive(Byte) }
  | PSHORT      { Primitive(Short) }
  | PINT        { Primitive(Int) }
  | PLONG       { Primitive(Long) }
  | PCHAR       { Primitive(Char) }

floatingPointType:
  | PFLOAT      { Primitive(Float) }
  | PDOUBLE     { Primitive(Double) }

referenceType:
  (*| cit = classOrInterfaceType    { cit }*)
  | id = IDENT             { Type id }
  (*| at = arrayType                { at }*)

%public
classOrInterfaceType:
  | tds = typeDeclSpecifier ta = typeArguments?   { Types(tds, ta) }

typeDeclSpecifier:
  | tn = typeName                                 { tn }
  | cit = classOrInterfaceType POINT id = IDENT   { TypeDecl(cit, Var id) }

(*arrayType:
  | typ LBRACKET RBRACKET   { ArrayType(typ) }*)

%public
typeArguments:
  | LT ata = separated_nonempty_list(COMA, actualTypeArgument) GT  { TypeArgs(ata) }

actualTypeArgument:
  | rt = referenceType    { rt }
  | w = wildcard          { w }

wildcard:
  | QUESTMARK wb = wildcardBounds   { wb }

wildcardBounds:
  | EXTENDS rt = referenceType      { Wild(Extends, rt) }
  | SUPER rt = referenceType        { Wild(Super, rt) }

%%
