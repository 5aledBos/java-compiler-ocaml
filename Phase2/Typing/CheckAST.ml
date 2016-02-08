open AST

let string_type_of_infix_op = function
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod -> "int"

let stringOf_prim = function
  | Some(Type.Primitive(Type.Int)) -> "int"
  | Some(Type.Primitive(Type.Char)) -> "char"
  | Some(Type.Primitive(Type.Boolean)) -> "boolean"
  | Some(Type.Ref({tpath=[]; tid="String"})) -> "string"
  | Some(Type.Primitive(Type.Float)) -> "float"

exception Wrong_types_aop of Type.t option * assign_op * Type.t option

let check_aop_type x op y =
  if x <> y then raise(Wrong_types_aop(x, op, y))
