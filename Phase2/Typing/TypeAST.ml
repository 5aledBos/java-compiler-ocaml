open AST

let type_val v =
  match v with
  | String s -> Some(Type.Ref({tpath=[]; tid="String"}))
  | Int i -> Some(Type.Primitive(Type.Int))
  | Float f -> Some(Type.Primitive(Type.Float))
  | Char c -> Some(Type.Primitive(Type.Char))
  | Null -> None
  | Boolean b -> Some(Type.Primitive(Type.Boolean))

let rec type_expression scope e =
  match e.edesc with
  | NewArray(t, l, None) -> e.etype <- Some(Type.Array(t, List.length l))
  | NewArray(t, l, Some(exp)) -> e.etype <- Some(Type.Array(t, List.length l))
  (* | Call(None, str, l) ->
  | Call(Some(exp), str, l) ->
  | Attr(exp, str) -> *)
  | If(e1, e2, e3) -> type_expression scope e1; type_expression scope e2; type_expression scope e3;
  | Val v -> e.etype <- type_val v
  | Name(name) -> e.etype <- if (Hashtbl.mem scope name) <> true then raise(CheckAST.Unknown_variable(name)) else Some(Hashtbl.find scope name)
  | ArrayInit(exp) -> List.iter (type_expression scope) exp;
    CheckAST.check_array_list_type exp;
    e.etype <- (match (List.hd exp).etype with
      | Some(t) -> Some(Type.Array(t, 1)))
  (* | Array(exp, []) ->
  | Array(exp, Some(l)) -> *)
  | AssignExp(e1, op, e2) -> type_expression scope e1; type_expression scope e2;
    CheckAST.check_aop_type e1.etype op e2.etype;
    e.etype <- e1.etype
  | Post(exp, op) -> type_expression scope exp;
    CheckAST.check_post_type exp.etype;
    e.etype <- exp.etype
  | Pre(op, exp) -> type_expression scope exp;
    CheckAST.check_pre_type op exp.etype;
    e.etype <- exp.etype
  | Op(e1, op, e2) -> type_expression scope e1; type_expression scope e2;
    CheckAST.check_op_type e1.etype op e2.etype;
    (match op with
    | Op_cor | Op_cand
    | Op_eq | Op_ne | Op_gt | Op_lt | Op_ge | Op_le -> e.etype <- Some(Type.Primitive(Type.Boolean))
    | Op_or | Op_and | Op_xor
    | Op_shl | Op_shr | Op_shrr
    | Op_add | Op_sub | Op_mul | Op_div | Op_mod -> e.etype <- e1.etype)
  | CondOp(e1, e2, e3) -> type_expression scope e1; type_expression scope e2; type_expression scope e3;
    CheckAST.check_tern_type e1.etype e2.etype e3.etype;
    if e2.etype <> None then e.etype <- e2.etype else e.etype <- e3.etype
  | Cast(t,ex) -> type_expression scope ex; e.etype <- Some(t)
  | Type t -> e.etype <- Some(t)
  | ClassOf t -> e.etype <- Some(t)
  | Instanceof(e, t) -> type_expression scope e
  | VoidClass -> ()

let add_variable scope name typ = if (Hashtbl.mem scope name) <> true then Hashtbl.add scope name typ else raise(CheckAST.Variable_name_exist(name))

let type_vardecl scope decl =
  match decl with
  | (t, name, None) -> add_variable scope name t
  | (t, name, Some e) -> type_expression scope e; if Some(t) <> e.etype then raise(CheckAST.Type_mismatch_decl(Some(t), e.etype)) else add_variable scope name t

let rec type_statement scope statement =
  match statement with
  | VarDecl(l) -> List.iter (type_vardecl scope) l
  | Block b -> let newscope = Hashtbl.copy scope in List.iter (type_statement newscope) b
  | Nop -> ()
  | While(e, s) -> type_expression scope e; type_statement scope s
  | If(e, s, None) -> type_expression scope e; type_statement scope s; CheckAST.check_if_test_type e.etype
  | If(e, s1, Some(s2)) -> type_expression scope e; type_statement scope s1; type_statement scope s2; CheckAST.check_if_test_type e.etype
  | Return None -> () (* Check with return type of the method *)
  | Return Some(e) -> type_expression scope e (* Check with return type of the method *)
  | Throw e -> type_expression scope e
  | Expr e -> type_expression scope e

let type_method m = let scope = Hashtbl.create 20 in List.iter (type_statement scope) m.mbody

let type_class c = List.iter type_method c.cmethods

let type_type t =
  match t.info with
  | Class c -> type_class c
  | Inter -> ()

let type_program p = List.iter type_type p.type_list
