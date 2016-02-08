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
  (* | NewArray(t, [], None) -> ()
  | NewArray(t, [Some(e)], None) -> type_expression e
  | NewArray(t, [], Some(e)) -> type_expression e
  | NewArray(t, [Some(e1)], Some(e2)) -> type_expression e1; type_expression e2 *)
  | If(e1, e2, e3) -> type_expression e1; type_expression e2; type_expression e3
  | Val v -> e.etype <- type_val v
  | AssignExp(e1, op, e2) -> type_expression e1; type_expression e2; CheckAST.check_aop_type e1.etype op e2.etype
  | Post(e, op) -> type_expression e
  | Pre(op, e) -> type_expression e
  | Op(e1, op, e2) -> type_expression e1; type_expression e2
  | CondOp(e1, e2, e3) -> type_expression e1; type_expression e2; type_expression e3; CheckAST.check_tern_type e1.etype e2.etype e3.etype; e.etype <- e2.etype
  | Cast(t,ex) -> type_expression ex; e.etype <- Some(t)
  | Type t -> ()
  | ClassOf t -> ()
  | Instanceof(e, t) -> type_expression e
  | VoidClass -> ()

let rec type_statement s =
  match s with
  | Block b -> List.iter type_statement b
  | Nop -> ()
  | While(e, s) -> type_expression e; type_statement s
  | If(e, s, None) -> type_expression e; type_statement s
  | If(e, s1, Some(s2)) -> type_expression e; type_statement s1; type_statement s2
  | Return None -> ()
  | Return Some(e) -> type_expression e
  | Throw e -> type_expression e
  | Expr e -> type_expression e

let type_method m = List.iter type_statement m.mbody

let type_class c = List.iter type_method c.cmethods

let type_type t =
  match t.info with
  | Class c -> type_class c
  | Inter -> ()

let type_program p = List.iter type_type p.type_list
