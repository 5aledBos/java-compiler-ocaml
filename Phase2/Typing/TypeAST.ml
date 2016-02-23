open AST

type funcinfo = {
  ftype: Type.t;
  fargs: argument list
}

type classscope = {
  methods : (string, funcinfo) Hashtbl.t;
  constructors : (string, funcinfo) Hashtbl.t;
  attributes : (string, Type.t) Hashtbl.t;
  parent : Type.ref_type;
}

type gscope = {
  classes : (string, classscope) Hashtbl.t ;
  mutable current : string
}

type scope = {
  returntype: Type.t;
  vars: (string, Type.t) Hashtbl.t
}

let type_val v =
  match v with
  | String s -> Some(Type.Ref({ tpath = []; tid = "String" }))
  | Int i -> Some(Type.Primitive(Type.Int))
  | Float f -> Some(Type.Primitive(Type.Float))
  | Char c -> Some(Type.Primitive(Type.Char))
  | Null -> None
  | Boolean b -> Some(Type.Primitive(Type.Boolean))

(* Local exceptions *)
exception Diff_arg
exception Not_typed_arg

let compare_call_args a b =
  match a.etype with
  | None -> raise(Not_typed_arg)
  | Some(t) -> if t <> b.ptype then raise(Diff_arg)

(* Compare the list of arguments of a method or constructor (given by its name) with a given list of arguments
and raise an exception if the method or constructor exists *)
let compare_args_function_args name args info = if List.length args <> List.length info.fargs then ()
  else try (* If all the arguments are the same, raise an exception *)
      List.iter2 compare_call_args args info.fargs;
      raise(CheckAST.Function_exist(name, info.ftype, info.fargs))
    with | Diff_arg -> ()

(* Type the result of a call expression given a class name, a method name, a list of arguments and the global scope *)
let type_call_expr methodName args globalScope className = let meths = (Hashtbl.find globalScope.classes className).methods in
  (* Check if the method name exists in the methods of the class *)
  if (Hashtbl.mem meths methodName) <> true
  then raise(CheckAST.Unknown_method(methodName, args, None))
  else ((* Check if there is a method with the proper name and the right arguments list *)
    try
      List.iter (compare_args_function_args methodName args) (Hashtbl.find_all meths methodName);
      raise(CheckAST.Unknown_method(methodName, args, None))
    with
      | Not_typed_arg -> raise(CheckAST.Not_typed_arg(methodName))
      | CheckAST.Function_exist(_, t, _) -> Some(t))

let rec check_ref_type (globalScope : gscope) (apparent_typ : Type.ref_type) (real_typ : Type.ref_type) =
  if apparent_typ.tid <> real_typ.tid then
    if apparent_typ.tid = "Object" then () else
      let parent = (Hashtbl.find globalScope.classes real_typ.tid).parent in
        if parent.tid = "Object" then raise(CheckAST.Wrong_ref_type(apparent_typ, real_typ)) else if apparent_typ <> parent then check_ref_type globalScope apparent_typ parent

let check_aop_type globalScope x op y =
  if x <> y then
    (match x with
     | Some(Type.Ref(apparent_typ)) -> if y <> None then
       (match y with
        | Some(Type.Ref(real_typ)) -> check_ref_type globalScope apparent_typ real_typ
        | _ -> raise(raise(CheckAST.Wrong_types_aop(x, op, y))))
     | _ -> raise(CheckAST.Wrong_types_aop(x, op, y)))

(* Type the expressions given the global and the current scopes *)
let rec type_expression globalScope scope e =
  match e.edesc with
  | New(None, l, exps) -> List.iter (type_expression globalScope scope) exps;
    let (last, lst) = ListII.extract_last l in
    (* Check if the class exists *)
    if (Hashtbl.mem globalScope.classes last) <> true
    then raise(CheckAST.Unknown_class(l))
    else
      (e.etype <- let constructors = (Hashtbl.find globalScope.classes last).constructors in
      (* Check if there are constructors. If not and the call is without arguments, no error. Otherwise, error *)
      if (Hashtbl.length constructors == 0 && List.length exps == 0) then Some(Type.Ref({ tpath = []; tid = last }))
      else (* Check if there is a constructor with the right arguments list *)
        try
          List.iter (compare_args_function_args last exps) (Hashtbl.find_all constructors last);
          raise(CheckAST.Unknown_constructor(l, exps))
        with
          | Not_typed_arg -> raise(CheckAST.Not_typed_arg(last))
          | CheckAST.Function_exist(_, t, _) -> Some(t))
  (* | New(Some(str), l, exps) -> *)
  | NewArray(t, l, None) -> e.etype <- Some(Type.Array(t, List.length l))
  | NewArray(t, l, Some(exp)) -> e.etype <- Some(Type.Array(t, List.length l))
  | Call(None, str, l) -> List.iter (type_expression globalScope scope) l;
    e.etype <- type_call_expr str l globalScope globalScope.current
  | Call(Some(exp), str, l) -> List.iter (type_expression globalScope scope) l; type_expression globalScope scope exp;
    (match exp with
     | { edesc = Name(id) } -> let cname = Type.stringOfOpt exp.etype in
     e.etype <- type_call_expr str l globalScope cname)
  | Attr(exp, str) -> type_expression globalScope scope exp;
    (match exp with
     | { edesc = Name(id) } -> let cname = Type.stringOfOpt exp.etype in
     let attrs = (Hashtbl.find globalScope.classes cname).attributes in
     e.etype <- (if Hashtbl.mem attrs str <> false
       then Some(Hashtbl.find attrs str)
       else raise(CheckAST.Unknown_attribute(str, cname))))
  | If(e1, e2, e3) -> type_expression globalScope scope e1; type_expression globalScope scope e2; type_expression globalScope scope e3
  | Val v -> e.etype <- type_val v
  | Name(name) -> e.etype <- if (Hashtbl.mem scope.vars name) <> true
    then (if (Hashtbl.mem (Hashtbl.find globalScope.classes globalScope.current).attributes name) <> true
      then raise(CheckAST.Unknown_variable(name))
      else Some(Hashtbl.find (Hashtbl.find globalScope.classes globalScope.current).attributes name))
    else Some(Hashtbl.find scope.vars name)
  | ArrayInit(exp) -> List.iter (type_expression globalScope scope) exp;
    CheckAST.check_array_list_type exp;
    e.etype <- (match (List.hd exp).etype with
      | Some(t) -> Some(Type.Array(t, 1)))
  (* | Array(exp, []) ->
  | Array(exp, l) -> *)
  | AssignExp(e1, op, e2) -> type_expression globalScope scope e1; type_expression globalScope scope e2;
    check_aop_type globalScope e1.etype op e2.etype;
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
  | Cast(t, exp) -> type_expression globalScope scope exp; e.etype <- Some(t)
  | Type t -> e.etype <- Some(t)
  | ClassOf t -> e.etype <- Some(t)
  | Instanceof(e, t) -> type_expression globalScope scope e
  | VoidClass -> ()

(* Add a variable, given its name and type, to the current scope *)
let add_variable scope name typ = if (Hashtbl.mem scope.vars name) <> true
  then Hashtbl.add scope.vars name typ
  else raise(CheckAST.Variable_name_exist(name))

(* Type a variable declaration and add it to the current scope *)
let type_vardecl globalScope scope decl =
  match decl with
  | (t, name, None) -> add_variable scope name t
  | (t, name, Some e) -> type_expression globalScope scope e;
    if Some(t) <> e.etype then
      (match t with
       | Type.Ref(apparent_typ) ->
        if e.etype <> None then
          (match e.etype with
           | Some(Type.Ref(real_typ)) -> check_ref_type globalScope apparent_typ real_typ;add_variable scope name t
           | _ -> raise(CheckAST.Type_mismatch_decl(Some(t), e.etype)))
        else add_variable scope name t
       | _ -> raise(CheckAST.Type_mismatch_decl(Some(t), e.etype)))
    else add_variable scope name t

(* Type the variable declaration of a for statement and add the variables to the current scope *)
let type_for_vardecl globalScope scope decl =
  match decl with
  | (Some(t), name, None) -> add_variable scope name t
  | (Some(t), name, Some e) -> type_expression globalScope scope e;
    if Some(t) <> e.etype then raise(CheckAST.Type_mismatch_decl(Some(t), e.etype)) else add_variable scope name t

(* Type the statements given the global and the current scopes *)
let rec type_statement globalScope scope statement =
  match statement with
  | VarDecl(l) -> List.iter (type_vardecl globalScope scope) l
  | Block b -> let newscope = { returntype = scope.returntype; vars = Hashtbl.copy scope.vars } in
    List.iter (type_statement globalScope newscope) b
  | Nop -> ()
  | While(e, s) -> type_expression globalScope scope e; type_statement globalScope scope s
  | For(l, None, exps, s) -> let forScope = { returntype = scope.returntype; vars = Hashtbl.copy scope.vars } in
    List.iter (type_for_vardecl globalScope forScope) l;
    List.iter (type_expression globalScope forScope) exps; type_statement globalScope forScope s
  | For(l, Some(exp), exps, s) -> let forScope = { returntype = scope.returntype; vars = Hashtbl.copy scope.vars } in
    List.iter (type_for_vardecl globalScope forScope) l;
    type_expression globalScope forScope exp; CheckAST.check_for_expr exp.etype;
    List.iter (type_expression globalScope forScope) exps; type_statement globalScope forScope s
  | If(e, s, None) -> type_expression globalScope scope e; type_statement globalScope scope s;
    CheckAST.check_if_test_type e.etype
  | If(e, s1, Some(s2)) -> type_expression globalScope scope e; type_statement globalScope scope s1; type_statement globalScope scope s2;
    CheckAST.check_if_test_type e.etype
  | Return None -> if scope.returntype <> Type.Void then raise(CheckAST.Wrong_return_type(scope.returntype, Type.Void))
  | Return Some(e) -> type_expression globalScope scope e; CheckAST.check_return_type scope.returntype e.etype
  | Throw e -> type_expression globalScope scope e
  | Try(s1, l, s2) -> List.iter (type_statement globalScope scope) s1; List.iter (type_statement globalScope scope) s2;
    List.iter (type_catches globalScope scope) l
  | Expr e -> type_expression globalScope scope e

(* Type the catch clauses and add the variable to the current scope *)
and type_catches globalScope scope catch = let catchScope = { returntype = scope.returntype; vars = Hashtbl.copy scope.vars } in
  match catch with
  | (arg, l) -> add_variable catchScope arg.pident arg.ptype; List.iter (type_statement globalScope catchScope) l

(* Add a given method or constructor name to the current scope *)
let add_function_args scope a = if (Hashtbl.mem scope.vars a.pident) <> true
  then Hashtbl.add scope.vars a.pident a.ptype
  else raise(CheckAST.Variable_name_exist(a.pident))

(* Type a given method and create a new scope for it *)
let type_method globalScope m = let scope = { returntype = m.mreturntype; vars = Hashtbl.create 20 } in
  List.iter (add_function_args scope) m.margstype;
  List.iter (type_statement globalScope scope) m.mbody

(* Type a given constructor and create a new scope for it *)
let type_constructor globalScope c = let constScope = { returntype = Type.Ref({ tpath = []; tid = c.cname }); vars = Hashtbl.create 20 } in
  List.iter (add_function_args constScope) c.cargstype;
  List.iter (type_statement globalScope constScope) c.cbody

(* Type a given class: add all its methods and constructors to the global scope *)
let type_class globalScope c = List.iter (type_method globalScope) c.cmethods; List.iter (type_constructor globalScope) c.cconsts

(* Compare the type of 2 given arguments and raise an exception if they are different *)
let compare_arg a b = if a.ptype <> b.ptype then raise(Diff_arg)

(* Compare the list of arguments of a constructor with a given list of arguments and raise an exception if the constructor exists *)
let compare_function_args name args info = if List.length args <> List.length info.fargs then ()
  else try (* If all the arguments are the same, raise an exception *)
      List.iter2 compare_arg args info.fargs;
      raise(CheckAST.Function_exist(name, info.ftype, args))
    with
      | Diff_arg -> ()

(* Add a given method to the global scope, checking if it does not exist already *)
let add_method globalScope m = let meths = (Hashtbl.find globalScope.classes globalScope.current).methods in
  (if (Hashtbl.mem meths m.mname) <> true
  then Hashtbl.add meths m.mname { ftype = m.mreturntype; fargs = m.margstype }
  else
    (List.iter (compare_function_args m.mname m.margstype) (Hashtbl.find_all meths m.mname);
    (* If no exception has been raise, we add the method into the hash table *)
    Hashtbl.add meths m.mname { ftype = m.mreturntype; fargs = m.margstype }))

(* Add a given attribute to the global scope, checking if it does not exist already *)
let add_attribute globalScope a = if (Hashtbl.mem (Hashtbl.find globalScope.classes globalScope.current).attributes a.aname) <> true
  then Hashtbl.add (Hashtbl.find globalScope.classes globalScope.current).attributes a.aname a.atype
  else raise(CheckAST.Attribute_name_exist(a.aname))

(* Add a given constructor to the global scope, checking if it does not exist already *)
let add_constructor globalScope c = let constructors = (Hashtbl.find globalScope.classes globalScope.current).constructors in
  (if (Hashtbl.mem constructors c.cname) <> true
  then Hashtbl.add constructors c.cname { ftype = Type.Ref({ tpath = []; tid = c.cname }); fargs = c.cargstype }
  else
    (List.iter (compare_function_args c.cname c.cargstype) (Hashtbl.find_all constructors c.cname);
    Hashtbl.add constructors c.cname { ftype = Type.Ref({ tpath = []; tid = c.cname }); fargs = c.cargstype }))


(* Add the methods, attributes and constructors of a given class to the global scope *)
let add_class globalScope c = List.iter (add_method globalScope) c.cmethods;
  List.iter (add_attribute globalScope) c.cattributes;
  List.iter (add_constructor globalScope) c.cconsts

(* Create the structure of the scope of each class (attributes, methods and constructors) *)
let add_type globalScope t =
  match t.info with
  | Class c -> if (Hashtbl.mem globalScope.classes t.id) <> true
    then (globalScope.current <- t.id; Hashtbl.add globalScope.classes globalScope.current
      { attributes = (Hashtbl.create 20); methods = (Hashtbl.create 20); constructors = (Hashtbl.create 20); parent = c.cparent })
    else raise(CheckAST.Class_name_exist(t.id)); add_class globalScope c
  | Inter -> ()

(* Type the classes and interfaces of a program *)
let type_type globalScope t =
  match t.info with
  | Class c -> globalScope.current <- t.id; type_class globalScope c
  | Inter -> ()

(* Type a program: create a global scope and type the content of the program *)
let type_program p = let globalScope = { classes = Hashtbl.create 20; current = "" }
  in List.iter (add_type globalScope) p.type_list;
  List.iter (type_type globalScope) p.type_list
