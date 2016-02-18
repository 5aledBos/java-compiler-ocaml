open AST

type classscope = {
  methods : (string, Type.t) Hashtbl.t;
  attributes : (string, Type.t) Hashtbl.t
}

type gscope = {
  classes : (string, classscope) Hashtbl.t ;
  mutable current : string
}

let type_val v =
  match v with
  | String s -> Some(Type.Ref({tpath=[]; tid="String"}))
  | Int i -> Some(Type.Primitive(Type.Int))
  | Float f -> Some(Type.Primitive(Type.Float))
  | Char c -> Some(Type.Primitive(Type.Char))
  | Null -> None
  | Boolean b -> Some(Type.Primitive(Type.Boolean))

let rec type_expression globalScope scope e =
  match e.edesc with
  | New(None, l, []) ->   (*of string option * string list * expression list*)
    let (last, lst) = ListII.extract_last l in
      if (Hashtbl.mem globalScope.classes last) <> false
      then e.etype <- Some(Type.Ref({ tpath = lst ; tid = last }))
      else raise(CheckAST.Unknown_class(l))
  | NewArray(t, l, None) -> e.etype <- Some(Type.Array(t, List.length l))
  | NewArray(t, l, Some(exp)) -> e.etype <- Some(Type.Array(t, List.length l))
  | Call(None, str, l) -> List.iter (type_expression globalScope scope) l;
    e.etype <- if (Hashtbl.mem (Hashtbl.find globalScope.classes globalScope.current).methods str) <> true
    then raise(CheckAST.Unknown_method(str))
    else Some(Hashtbl.find (Hashtbl.find globalScope.classes globalScope.current).methods str)
  (* | Call(Some(exp), str, l) ->*)
  (*| Attr(exp, str) -> type_expression exp globalScope scope exp;*)
  | If(e1, e2, e3) -> type_expression globalScope scope e1; type_expression globalScope scope e2; type_expression globalScope scope e3
  | Val v -> e.etype <- type_val v
  | Name(name) -> e.etype <- if (Hashtbl.mem scope name) <> true
    then
      (if (Hashtbl.mem (Hashtbl.find globalScope.classes globalScope.current).attributes name) <> true
      then raise(CheckAST.Unknown_attribute(name))
      else Some(Hashtbl.find (Hashtbl.find globalScope.classes globalScope.current).attributes name))
    else Some(Hashtbl.find scope name)
  | ArrayInit(exp) -> List.iter (type_expression globalScope scope) exp;
    CheckAST.check_array_list_type exp;
    e.etype <- (match (List.hd exp).etype with
      | Some(t) -> Some(Type.Array(t, 1)))
  (* | Array(exp, []) ->
  | Array(exp, Some(l)) -> *)
  | AssignExp(e1, op, e2) -> type_expression globalScope scope e1; type_expression globalScope scope e2;
    CheckAST.check_aop_type e1.etype op e2.etype;
    e.etype <- e1.etype
  | Post(exp, op) -> type_expression globalScope scope exp;
    CheckAST.check_post_type exp.etype;
    e.etype <- exp.etype
  | Pre(op, exp) -> type_expression globalScope scope exp;
    CheckAST.check_pre_type op exp.etype;
    e.etype <- exp.etype
  | Op(e1, op, e2) -> type_expression globalScope scope e1; type_expression globalScope scope e2;
    CheckAST.check_op_type e1.etype op e2.etype;
    (match op with
    | Op_cor | Op_cand
    | Op_eq | Op_ne | Op_gt | Op_lt | Op_ge | Op_le -> e.etype <- Some(Type.Primitive(Type.Boolean))
    | Op_or | Op_and | Op_xor
    | Op_shl | Op_shr | Op_shrr
    | Op_add | Op_sub | Op_mul | Op_div | Op_mod -> e.etype <- e1.etype)
  | CondOp(e1, e2, e3) -> type_expression globalScope scope e1; type_expression globalScope scope e2; type_expression globalScope scope e3;
    CheckAST.check_tern_type e1.etype e2.etype e3.etype;
    if e2.etype <> None then e.etype <- e2.etype else e.etype <- e3.etype
  | Cast(t,ex) -> type_expression globalScope scope ex; e.etype <- Some(t)
  | Type t -> e.etype <- Some(t)
  | ClassOf t -> e.etype <- Some(t)
  | Instanceof(e, t) -> type_expression globalScope scope e
  | VoidClass -> ()

let add_variable scope name typ = if (Hashtbl.mem scope name) <> true then Hashtbl.add scope name typ else raise(CheckAST.Variable_name_exist(name))

let type_vardecl globalScope scope decl =
  match decl with
  | (t, name, None) -> add_variable scope name t
  | (t, name, Some e) -> type_expression globalScope scope e;
    if Some(t) <> e.etype then raise(CheckAST.Type_mismatch_decl(Some(t), e.etype)) else add_variable scope name t

let rec type_statement globalScope scope statement =
  match statement with
  | VarDecl(l) -> List.iter (type_vardecl globalScope scope) l
  | Block b -> let newscope = Hashtbl.copy scope in List.iter (type_statement globalScope newscope) b
  | Nop -> ()
  | While(e, s) -> type_expression globalScope scope e; type_statement globalScope scope s
  | If(e, s, None) -> type_expression globalScope scope e; type_statement globalScope scope s;
    CheckAST.check_if_test_type e.etype
  | If(e, s1, Some(s2)) -> type_expression globalScope scope e; type_statement globalScope scope s1; type_statement globalScope scope s2;
    CheckAST.check_if_test_type e.etype
  | Return None -> () (* Check with return type of the method *)
  | Return Some(e) -> type_expression globalScope scope e (* Check with return type of the method *)
  | Throw e -> type_expression globalScope scope e
  | Expr e -> type_expression globalScope scope e

let type_method globalScope m = let scope = Hashtbl.create 20 in List.iter (type_statement globalScope scope) m.mbody

let type_class globalScope c = List.iter (type_method globalScope) c.cmethods

let type_type globalScope t =
  match t.info with
  | Class c -> type_class globalScope c
  | Inter -> ()

(* First add all the attibutes and methods from the classes so they can be then used in the methods body *)
let add_method globalScope m = if (Hashtbl.mem (Hashtbl.find globalScope.classes globalScope.current).methods m.mname) <> true
  then Hashtbl.add (Hashtbl.find globalScope.classes globalScope.current).methods m.mname m.mreturntype else raise(CheckAST.Method_name_exist(m.mname))

let add_attribute globalScope a = if (Hashtbl.mem (Hashtbl.find globalScope.classes globalScope.current).attributes a.aname) <> true
  then Hashtbl.add (Hashtbl.find globalScope.classes globalScope.current).attributes a.aname a.atype else raise(CheckAST.Attribute_name_exist(a.aname))

let add_class globalScope c = List.iter (add_method globalScope) c.cmethods;
  List.iter (add_attribute globalScope) c.cattributes

let add_type globalScope t =
  match t.info with
  | Class c -> if (Hashtbl.mem globalScope.classes t.id) <> true
    then (globalScope.current <- t.id; Hashtbl.add globalScope.classes globalScope.current {attributes = (Hashtbl.create 20); methods = (Hashtbl.create 20)})
    else raise(CheckAST.Class_name_exist(t.id)); add_class globalScope c
  | Inter -> ()

let type_program p = let globalScope = { classes = Hashtbl.create 20; current = "" }
  in List.iter (add_type globalScope) p.type_list;
  List.iter (type_type globalScope) p.type_list
