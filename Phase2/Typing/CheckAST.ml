open AST

let stringOf_prim = function
  | Some(Type.Primitive(Type.Boolean)) -> "boolean"
  | Some(Type.Primitive(Type.Char)) -> "char"
  | Some(Type.Primitive(Type.Byte)) -> "byte"
  | Some(Type.Primitive(Type.Short)) -> "short"
  | Some(Type.Primitive(Type.Int)) -> "int"
  | Some(Type.Primitive(Type.Int)) -> "long"
  | Some(Type.Primitive(Type.Float)) -> "float"
  | Some(Type.Ref({tpath=[]; tid="String"})) -> "string"

exception Wrong_types_aop of Type.t option * assign_op * Type.t option

let check_aop_type x op y =
  if x <> y then raise(Wrong_types_aop(x, op, y))
