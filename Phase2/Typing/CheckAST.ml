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

(* ERRORS *)

exception Wrong_types_aop of Type.t option * assign_op * Type.t option
exception Wrong_types_op of Type.t option * infix_op * Type.t option
exception Wrong_type_tern of Type.t option
exception Wrong_type_if of Type.t option
exception Type_mismatch_tern of Type.t option * Type.t option

(* String of errors *)
let print_wrong_types_aop x op y =
  print_string ("L'operateur " ^ (AST.string_of_assign_op op));
  print_string (" attend deux arguments de meme type");
  print_string (" et il recoit " ^ (stringOf_prim x));
  print_endline (" et " ^ (stringOf_prim y))

(* TODO: Some of the ops only take bools *)
let print_wrong_types_op x op y =
    print_string ("L'operateur " ^ (AST.string_of_infix_op op));
    print_string (" attend deux arguments de meme type");
    print_string (" et il recoit " ^ (stringOf_prim x));
    print_endline (" et " ^ (stringOf_prim y))

let print_not_bool_exception expression test =
  print_string ("La condition d'une expression "^expression^" doit etre un booleen");
  print_endline (" et elle recoit un " ^ stringOf_prim test)

let print_wrong_type_tern test =
  print_not_bool_exception "ternaire" test

let print_wrong_type_if test =
  print_not_bool_exception "if" test

let print_type_mismatch_tern x y =
  print_string ("Les deux expressions d'une expression ternaire doivent etre du meme type");
  print_string (" et elle recoit " ^ (stringOf_prim x));
  print_endline (" et " ^ (stringOf_prim y))


(* CHECKS *)
let check_aop_type x op y =
  if x <> y then raise(Wrong_types_aop(x, op, y))

(* TODO: Some of the ops only take bools *)
let check_op_type x op y =
    if x <> y then raise(Wrong_types_op(x, op, y))

let check_tern_type test x y =
  if test <> Some(Type.Primitive(Type.Boolean)) then raise(Wrong_type_tern(test));
  if x <> y then raise(Type_mismatch_tern(x, y))

let check_if_test_type test =
  if test <> Some(Type.Primitive(Type.Boolean)) then raise(Wrong_type_if(test));
