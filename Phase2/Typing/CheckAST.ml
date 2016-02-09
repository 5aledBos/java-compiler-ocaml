open AST

(* String of helpers *)

let string_of_prefix_type = function
  | Op_not -> "boolean"
  | Op_bnot -> "int"
  | Op_neg | Op_incr | Op_decr | Op_plus -> "int ou float"


(* ERRORS *)

exception Wrong_types_aop of Type.t option * assign_op * Type.t option
exception Wrong_types_op of Type.t option * infix_op * Type.t option
exception Wrong_type_tern of Type.t option
exception Wrong_type_if of Type.t option
exception Type_mismatch_tern of Type.t option * Type.t option
exception Wrong_type_post of Type.t option
exception Wrong_type_unop of prefix_op * Type.t option
exception Type_mismatch_decl of Type.t option * Type.t option
exception Variable_name_exist of string
exception Unknown_variable of string

(* String of errors *)
let print_wrong_types_aop x op y =
  print_string ("L'operateur " ^ (AST.string_of_assign_op op));
  print_string (" attend deux arguments de meme type");
  print_string (" et il recoit " ^ (Type.stringOfOpt x));
  print_endline (" et " ^ (Type.stringOfOpt y))

(* TODO: Some of the ops only take bools *)
let print_wrong_types_op x op y =
    print_string ("L'operateur " ^ (AST.string_of_infix_op op));
    print_string (" attend deux arguments de meme type");
    print_string (" et il recoit " ^ (Type.stringOfOpt x));
    print_endline (" et " ^ (Type.stringOfOpt y))

let print_not_bool_exception expression test =
  print_string ("La condition d'une expression " ^ expression ^ " doit etre un booleen");
  print_endline (" et elle recoit un " ^ (Type.stringOfOpt test))

let print_wrong_type_tern test =
  print_not_bool_exception "ternaire" test

let print_wrong_type_if test =
  print_not_bool_exception "if" test

let print_wrong_type_post x =
  print_string ("Les operateurs ++ et -- attendent un int ou un float");
  print_endline (" et recoivent un " ^ (Type.stringOfOpt x))

let print_wrong_type_pre op x =
  print_string ("L'operateur " ^ (AST.string_of_prefix_op op));
  print_string (" attend un argument de type " ^ (string_of_prefix_type op));
  print_endline (" et il recoit " ^ (Type.stringOfOpt x))

let print_type_mismatch expression x y =
  print_string ("Les deux expressions d'une "^ expression ^" doivent etre du meme type");
  print_string (" et elle recoit " ^ (Type.stringOfOpt x));
  print_endline (" et " ^ (Type.stringOfOpt y))

let print_type_mismatch_tern x y =
  print_type_mismatch "expression ternaire" x y

let print_type_mismatch_decl x y =
  print_type_mismatch "declaration" x y

let print_variable_name_exist name =
  print_endline ("Le nom de variable " ^ name ^ "existe deja.")

let print_unkown_variable name =
  print_endline ("Pas de variable " ^ name ^ "dans le scope courant.")

(* CHECKS *)
let check_aop_type x op y =
  if x <> y then raise(Wrong_types_aop(x, op, y))

(* TODO: Some of the ops only take bools *)
let check_op_type x op y =
    if x <> y then raise(Wrong_types_op(x, op, y))

let check_tern_type test x y =
  if test <> Some(Type.Primitive(Type.Boolean)) then raise(Wrong_type_tern(test));
  match x, y with
  | Some(Type.Primitive(_)), None -> raise(Type_mismatch_tern(x, y))
  | None, Some(Type.Primitive(_)) -> raise(Type_mismatch_tern(x, y))
  | Some(_), None -> ()
  | None, Some(_) -> ()
  | Some(typ1), Some(typ2) ->  if typ1 <> typ2 then raise(Type_mismatch_tern(x, y))

let check_if_test_type test =
  if test <> Some(Type.Primitive(Type.Boolean)) then raise(Wrong_type_if(test))

let check_post_type x =
  if (x <> Some(Type.Primitive(Type.Int)) && x <> Some(Type.Primitive(Type.Float))) then raise(Wrong_type_post(x))

let check_pre_type op x =
  match op with
  | Op_not -> if x <> Some(Type.Primitive(Type.Boolean)) then raise(Wrong_type_unop(op, x))
  | Op_bnot -> if x <> Some(Type.Primitive(Type.Int)) then raise(Wrong_type_unop(op, x))
  | Op_neg | Op_incr | Op_decr | Op_plus -> if (x <> Some(Type.Primitive(Type.Int)) && x <> Some(Type.Primitive(Type.Float))) then raise(Wrong_type_unop(op, x))
