%{
    open AstUtil
%}

/******************************/
/* Entry points of the parser */
/******************************/

%start typ
%type <AstUtil.typ> typ

%start primitiveType
%type <AstUtil.typ> primitiveType

%start referenceType
%type <AstUtil.typ> referenceType

(*%start classType*)
(*%type <AstUtil.> classType*)

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
  (*| at = arrayType                { at }

classOrInterfaceType:
  | ct = classType                { ct }
  | it = interfaceType            { it }*)

(*classType:*)
(*  | tds = typeDeclSpecifier ta = typeArguments?   { Types(Class, tds, ta) }*)

(*interfaceType:*)
(*  | tds = typeDeclSpecifier ta = typeArguments?   { Types(Interface, tds, ta) }*)

(*typeDeclSpecifier:*)
(*  | tn = typeName                                 { tn }*)
(*  | cit = classOrInterfaceType POINT id = IDENT   { TypeDecl(cit, id) }*)

(*typeName:*)
(*  | id = IDENT                       { Var id }*)
(*  | tn = typeName POINT id = IDENT   { TypeName(tn, id) }*)

(*arrayType:
  | typ LBRACKET RBRACKET   { ArrayType(typ) }

typeArguments:
  | LT ata = actualTypeArgumentList GT  { TypeArgs(ata) }

actualTypeArgumentList:
  | at = actualTypeArgument                                     { [at] }
  | atl = actualTypeArgumentList COMA at = actualTypeArgument   { at::atl }

actualTypeArgument:
  | rt = referenceType    { rt }
  | w = wildcard          { w }

wildcard:
  | QUESTMARK wb = wildcardBounds?   {}

wildcardBounds:
  | EXTENDS rt = referenceType    {}
  | SUPER rt = referenceType      {}*)
%%
