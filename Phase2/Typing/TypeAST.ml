open AST

let type_val v =
  match v with
  | String s -> Some(Type.Ref({tpath=[]; tid="String"}))
  | Boolean b -> Some(Type.Primitive(Type.Boolean))
  | Char c -> Some(Type.Primitive(Type.Char))
  | Int i -> Some(Type.Primitive(Type.Int))
  | Float f -> Some(Type.Primitive(Type.Float))
  | Null -> None

let rec type_expression e =
  match e.edesc with
  | AssignExp(e1,op,e2) -> type_expression e1; type_expression e2
  | Val v -> e.etype <- type_val v

let type_statement s =
  match s with
  | Expr e -> type_expression e

let type_method m = List.iter type_statement m.mbody

let type_class c = List.iter type_method c.cmethods

let type_type t =
  match t.info with
  | Class c -> type_class c
  | Inter -> ()

let type_program p = List.iter type_type p.type_list
