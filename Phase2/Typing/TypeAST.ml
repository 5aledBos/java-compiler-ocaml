open AST

let type_val v =
  match v with
  | Boolean b -> Some(Type.Primitive(Type.Boolean))
  | Char c -> Some(Type.Primitive(Type.Char))
  | Int i -> Some(Type.Primitive(Type.Int))

let rec type_expression e =
  match e.edesc with
  | AssignExp(e1,op,e2) -> {edesc = AssignExp(type_expression e1, op, type_expression e2); etype = None}
  | Val v -> {edesc = Val(v); etype = type_val v}

let type_statement s =
  match s with
  | Expr e -> Expr(type_expression e)

let type_method m = {
  mmodifiers = m.mmodifiers;
  mname = m.mname;
  mreturntype = m.mreturntype;
  margstype = m.margstype;
  mthrows = m.mthrows;
  mbody = List.map type_statement m.mbody
}

let type_class c = {
  cparent = c.cparent;
  cattributes = c.cattributes;
  cinits = c.cinits;
  cconsts = c.cconsts;
  cmethods = List.map type_method c.cmethods;
  ctypes = c.ctypes;
  cloc = c.cloc
}

let type_type t = {
    modifiers = t.modifiers;
    id = t.id;
    info = match t.info with
           | Class c -> Class(type_class c)
           | Inter -> Inter
}

let type_program p = {
    package = p.package;
    type_list = List.map type_type p.type_list
}
