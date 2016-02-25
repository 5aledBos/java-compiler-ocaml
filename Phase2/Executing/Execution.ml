open Compilation
open AST
open Type

type globalScope =
{
  data : globalData;
  mutable currentClassScoped : string;
  heap : (int, globalObjectDescriptor) Hashtbl.t;
  mutable free_adress : int;
(*  scope : exec_scope list*)
}

exception NullPointerException
exception InvalideOperationException
exception ArithmeticException

let printHeap heap =
   Hashtbl.iter (fun key value -> (Printf.printf "%i " key; printObjectDescriptor(value); print_endline(""))) heap


type exec_scope =
{
  mutable currentscope : int;
  mutable currentobject : globalObjectDescriptor;
  mutable scopelist : (string, exec_Value) Hashtbl.t list
}

let printScope scope =
	Hashtbl.iter (fun key value -> print_string("object: " ^ key); print_string(", value "^ string_execvalue(value)); print_endline("")) (List.nth scope.scopelist  scope.currentscope)

(* constructor calls *)
(*let compare_args_function_args args constargs = *)
(*  let isEqual = true in*)
(*      List.iter2 (compare_args isEqual) args constargs; isEqual*)

(*let compare_args isEqual a.etype b.ptype = if (a.pident == b.pidend) <> true then isEqual = true else isEqual = false*)

(*let rec findConstructor constructors exps = match constructors with*)
(*  | (head::liste) -> if (compare_args_function_args exps head.cargstype) then head else findConstructor liste exps*)
(*let (last, lst) = ListII.extract_last l =  in globalScope.currentClassScoped <- last; let cd = Hashtbl.find globalScope.classDescriptorTable l in let constr = findConstructor cd exps;*) 


let rec evaluate_expression globalScope scope expr = match expr.edesc with
  | New(None, l, exps) -> (match expr.etype with Some(ref_type) -> addObject ref_type globalScope scope; VRef(globalScope.free_adress -1) )
(*  | New(Some(str), l, exps) -> print_endline("ici"); VInt (int_of_string "55")*)
  | Val v -> (match v with
				| Int(i) -> VInt (int_of_string i)
				| Boolean(b) -> VBool(b)
				| String s -> VString(s)
				| Null -> VNull )
  | AssignExp(e1, op, e2) -> let ref1 = evaluate_expression  globalScope scope e1 and ref2 = evaluate_expression globalScope scope e2 in  eval_assign_op op ref1 ref2 globalScope scope
  | Op(e1, op, e2) -> eval_normal_op op (evaluate_expression globalScope scope e1) (evaluate_expression globalScope scope e2) globalScope scope
  | Post(exp, op) -> eval_post_op op (evaluate_expression globalScope scope exp) globalScope scope
  | Pre(op, exp) -> eval_pre_op op (evaluate_expression globalScope scope exp) globalScope scope
  | Name(name) -> (*let execvalue = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name in print_endline("found ref: "); execvalue;*) VName(name)

  | Call(Some(exp), str, l) -> (match exp.edesc with Name(id) -> let o = find_object_in_heap globalScope.heap (Hashtbl.find (List.nth scope.scopelist  scope.currentscope) (id)) in (match o with ObjectDescriptor(od) -> let m = Hashtbl.find globalScope.data.methodTable (od.otype ^ "_" ^ str) in  globalScope.currentClassScoped <- od.otype; scope.currentscope <- scope.currentscope +1;scope.scopelist <- scope.scopelist @ [Hashtbl.create 20]; scope.currentobject <- o;let returnvalue = execute_method m globalScope scope l in  globalScope.currentClassScoped <- od.otype; scope.scopelist <- remove_at scope.currentscope scope.scopelist; scope.currentscope <- scope.currentscope-1;  returnvalue ))

  | Call(None, str, l) -> let m = Hashtbl.find globalScope.data.methodTable (globalScope.currentClassScoped ^ "_" ^ str) in execute_method m globalScope scope; VNull
(*  | If(e1, e2, e3) -> let vbool = evaluate_expression globalScope scope e1 in (match vbool with VBool(b) -> if b then VNull else VNull)*)


and eval_normal_op op v1 v2 globalScope scope = match op, v1, v2 with
  | Op_cand, VBool(b1), VBool(b2) -> VBool(b1 && b2)
  | Op_or, VBool(b1), VBool(b2) -> VBool(b1 || b2)
  | Op_and, VBool(b1), VBool(b2) -> VBool(b1 & b2)	
  | Op_xor, VBool(b1), VBool(b2) -> VBool((b1&&(not b2))||((not b1)&&b2) )

  | Op_eq, VInt(i1), VInt(i2) -> if i1 == i2 then VBool(true) else VBool(false)
  | Op_ne, VInt(i1), VInt(i2) -> if i1 != i2 then VBool(true) else VBool(false)
  | Op_gt, VInt(i1), VInt(i2) -> if i1 > i2 then VBool(true) else VBool(false)
  | Op_lt, VInt(i1), VInt(i2) -> if i1 < i2 then VBool(true) else VBool(false)
  | Op_ge, VInt(i1), VInt(i2) -> if i1 >= i2 then VBool(true) else VBool(false)
  | Op_le, VInt(i1), VInt(i2) -> if i1 <= i2 then VBool(true) else VBool(false)
  | Op_add, VInt(i1), VInt(i2) -> VInt(i1+i2)
  | Op_sub, VInt(i1), VInt(i2) -> VInt(i1-i2)
  | Op_mul, VInt(i1), VInt(i2) -> VInt(i1*i2)
  | Op_div, VInt(i1), VInt(i2) -> if i2 == 0 then raise(ArithmeticException) else VInt(i1/i2)
  | Op_mod, VInt(i1), VInt(i2) -> VInt(i1 mod i2)

  | Op_cand, VName(name1), VBool(b2) ->let vbool1 = find_execvalue_in_scope scope name1 in (match vbool1 with VBool(b1) -> VBool(b1 && b2))
  | Op_or, VName(name1), VBool(b2) ->let vbool1 = find_execvalue_in_scope scope name1 in (match vbool1 with VBool(b1) -> VBool(b1 || b2) )
  | Op_and, VName(name1), VBool(b2) ->let vbool1 = find_execvalue_in_scope scope name1 in (match vbool1 with VBool(b1) -> VBool(b1 & b2) )
  | Op_xor, VName(name1), VBool(b2) ->let vbool1 = find_execvalue_in_scope scope name1 in (match vbool1 with VBool(b1) -> VBool((b1&&(not b2))||((not b1)&&b2)) )

  | Op_eq, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> if i1 == i2 then VBool(true) else VBool(false))
  | Op_ne, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> if i1 != i2 then VBool(true) else VBool(false))
  | Op_gt, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> if i1 > i2 then VBool(true) else VBool(false))
  | Op_lt, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> if i1 < i2 then VBool(true) else VBool(false))
  | Op_ge, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> if i1 >= i2 then VBool(true) else VBool(false))
  | Op_le, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> if i1 <= i2 then VBool(true) else VBool(false))
  | Op_add, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> VInt(i1+i2) )
  | Op_sub, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> VInt(i1-i2) )
  | Op_mul, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> VInt(i1*i2) )
  | Op_div, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> if i2 == 0 then raise(ArithmeticException) else VInt(i1/i2) )
  | Op_mod, VName(name1), VInt(i2) -> let vint1 = find_execvalue_in_scope scope name1 in (match vint1 with VInt(i1) -> VInt(i1 mod i2) )

  | Op_cor, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with VInt(i1), VInt(i2) -> VInt(i1+i2) )

  | Op_cand, VName(name1), VName(name2) ->let vbool1 = find_execvalue_in_scope scope name1 and vbool2 = find_execvalue_in_scope scope name2 in (match vbool1, vbool2 with VBool(b1), VBool(b2) -> VBool(b1 && b2))
  | Op_or, VName(name1), VName(name2) ->let vbool1 = find_execvalue_in_scope scope name1 and vbool2 = find_execvalue_in_scope scope name2 in (match vbool1, vbool2 with VBool(b1), VBool(b2) -> VBool(b1 || b2))
  | Op_and, VName(name1), VName(name2) ->let vbool1 = find_execvalue_in_scope scope name1 and vbool2 = find_execvalue_in_scope scope name2 in (match vbool1, vbool2 with VBool(b1), VBool(b2) -> VBool(b1 & b2))
  | Op_xor, VName(name1), VName(name2) ->let vbool1 = find_execvalue_in_scope scope name1 and vbool2 = find_execvalue_in_scope scope name2 in (match vbool1, vbool2 with VBool(b1), VBool(b2) -> VBool((b1&&(not b2))||((not b1)&&b2)))

  | Op_eq, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with
						| VInt(i1), VInt(i2) -> if i1 == i2 then VBool(true) else VBool(false)
						| VRef(i1), VRef(i2) -> if i1 == i2 then VBool(true) else VBool(false)  )
  | Op_ne, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with
						| VInt(i1), VInt(i2) -> if i1 != i2 then VBool(true) else VBool(false)
						| VRef(i1), VRef(i2) -> if i1 != i2 then VBool(true) else VBool(false)  )
  | Op_gt, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with
						| VInt(i1), VInt(i2) -> if i1 > i2 then VBool(true) else VBool(false) )
  | Op_lt, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with
						| VInt(i1), VInt(i2) -> if i1 < i2 then VBool(true) else VBool(false) )
  | Op_ge, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with
						| VInt(i1), VInt(i2) -> if i1 >= i2 then VBool(true) else VBool(false) )
  | Op_le, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with
						| VInt(i1), VInt(i2) -> if i1 <= i2 then VBool(true) else VBool(false) )
  | Op_add, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with VInt(i1), VInt(i2) -> VInt(i1 + i2) )
  | Op_sub, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with VInt(i1), VInt(i2) -> VInt(i1 - i2) )
  | Op_mul, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with VInt(i1), VInt(i2) -> VInt(i1 * i2) )
  | Op_div, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with VInt(i1), VInt(i2) -> if i2 == 0 then raise(InvalideOperationException) else VInt(i1+i2) )
  | Op_mod, VName(name1), VName(name2) -> let vint1 = find_execvalue_in_scope scope name1 and vint2 = find_execvalue_in_scope scope name2 in (match vint1, vint2 with VInt(i1), VInt(i2) -> VInt(i1 mod i2) )


and eval_assign_op op v1 v2 globalScope scope = match op with
  | Assign -> (match v1, v2 with
					| VName(name1), VName(name2) -> Hashtbl.remove (List.nth scope.scopelist  scope.currentscope) name1; let ref_nb2 = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name2 in (match ref_nb2 with 
						| VRef(i) -> (*Hashtbl.remove globalScope.heap ref1;*) Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name1 (VRef(i)); VRef(i) )
					| VName(name1), VString(s) -> let execvalue = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name1 in (match execvalue with
						| VRef(i) ->  Hashtbl.remove globalScope.heap i; Hashtbl.add globalScope.heap i (StringDescriptor(s)); replace_execvalue_in_scope scope name1 (VRef(i)); VRef(i)
						| VNull -> Hashtbl.add globalScope.heap globalScope.free_adress (StringDescriptor(s)); Hashtbl.remove (List.nth scope.scopelist  scope.currentscope) name1; Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name1 (VRef(globalScope.free_adress)); globalScope.free_adress <- globalScope.free_adress + 1; VNull )
					| VName(name1), VNull -> replace_execvalue_in_scope scope name1 VNull; VNull
					| VName(name1), VInt(i) -> replace_execvalue_in_scope scope name1 (VInt(i)); VInt(i)
					| VName(name1), VRef(i) -> replace_execvalue_in_scope scope name1 (VRef(i)); VRef(i)
  )
  | Ass_add -> (match v1, v2 with
					| VName(name1), VInt(i) -> let value1 = find_execvalue_in_scope scope name1 in ( match value1 with VInt(i1) -> replace_execvalue_in_scope scope name1 (VInt(i1+i)); VInt(i1+i) )
					| VName(name1), VString(s) -> let execvalue = find_execvalue_in_scope scope name1 in (match execvalue with
						| VRef(i) ->  let s1 = Hashtbl.find globalScope.heap i in (match s1 with StringDescriptor(str1) -> Hashtbl.remove globalScope.heap i; Hashtbl.add globalScope.heap i (StringDescriptor(str1 ^ s));replace_execvalue_in_scope scope name1 (VRef(i)); VRef(i) )
						| VNull -> raise(NullPointerException)
						| _ -> VNull )
   )
  | Ass_sub -> (match v1, v2 with
					| VName(name1), VInt(i) -> let value1 = find_execvalue_in_scope scope name1 in ( match value1 with VInt(i1) -> replace_execvalue_in_scope scope name1 (VInt(i1-i)); VInt(i1-i)) 
					| _, _ -> raise(InvalideOperationException)
  )
  | Ass_mul -> (match v1, v2 with
					| VName(name1), VInt(i) -> let value1 = find_execvalue_in_scope scope name1 in ( match value1 with VInt(i1) -> replace_execvalue_in_scope scope name1 (VInt(i1*i)); VInt(i1*i)) 
					| _, _ -> raise(InvalideOperationException)
  )
  | Ass_div -> (match v1, v2 with
					| VName(name1), VInt(i) -> let value1 = find_execvalue_in_scope scope name1 in ( match value1 with VInt(i1) -> replace_execvalue_in_scope scope name1 (VInt(i1*i)); VInt(i1*i)) 
					| _, _ -> raise(InvalideOperationException)
  )
  | Ass_mod -> (match v1, v2 with
					| VName(name1), VInt(i) -> let value1 = find_execvalue_in_scope scope name1 in ( match value1 with VInt(i1) -> replace_execvalue_in_scope scope name1 (VInt(i1 mod i)); VInt(i1 mod i)) 
					| _, _ -> raise(InvalideOperationException)
  )
(*  | Ass_shl*)
(*  | Ass_shr*)
(*  | Ass_shrr*)
(*  | Ass_and*)
(*  | Ass_xor*)
(*  | Ass_or*)


and eval_post_op op v1 globalScope scope = match op, v1 with
  | Incr, VName(name) -> let execvalue = find_execvalue_in_scope scope name in (match execvalue with VInt(i) -> replace_execvalue_in_scope scope name (VInt(i+1)); VInt(i+1))
  | Decr, VName(name) -> let execvalue = find_execvalue_in_scope scope name in (match execvalue with VInt(i) -> replace_execvalue_in_scope scope name (VInt(i-1)); VInt(i-1))

and eval_pre_op op v1 globalScope scope = match op, v1 with
  | Op_not, VName(name) ->  let execvalue = find_execvalue_in_scope scope name in (match execvalue with VBool(b) -> VBool(not b))
(*  | Op_neg*)
  | Op_incr, VName(name) -> let execvalue = find_execvalue_in_scope scope name in (match execvalue with VInt(i) -> replace_execvalue_in_scope scope name (VInt(i+1)); VInt(i)) 
  | Op_decr, VName(name) -> let execvalue = find_execvalue_in_scope scope name in (match execvalue with VInt(i) -> replace_execvalue_in_scope scope name (VInt(i-1)); VInt(i))
(*  | Op_bnot -> VNull*)
(*  | Op_plus*)

and execute_method m globalScope scope params =
	  print_endline("*********executing method" ^ m.mname ^"**************");
	 List.iter2 (addParameterToScope globalScope scope) m.margstype params;
	 List.iter (evaluate_statement globalScope scope) m.mbody;
	 print_endline("*********End of method**************");
	if Hashtbl.mem (List.nth scope.scopelist  scope.currentscope) "return" <> true then VNull else
	 let a = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) "return" in print_endline("return fonction: " ^ string_execvalue a); a


and addParameterToScope globalScope scope marg param  =
  add_variable_to_scope globalScope scope marg.pident (evaluate_expression globalScope scope param)


and evaluate_statement globalScope scope stmt = match stmt with
  | VarDecl(l) -> List.iter (exec_vardecl globalScope scope) l
  | Expr e -> evaluate_expression globalScope scope e; ()
  | Return Some(e) -> add_variable_to_scope globalScope scope "return" (evaluate_expression globalScope scope e)
  | Return None -> add_variable_to_scope globalScope scope "return" VNull
  | If(e, s, None) -> let execvalue = evaluate_expression globalScope scope e in print_endline(string_execvalue(execvalue));(match execvalue with
		| VBool(b) -> if b then evaluate_statement globalScope scope s
		| VName(name) -> let vbool = find_execvalue_in_scope scope name in (match vbool with
			| VBool(b) -> if b then evaluate_statement globalScope scope s ))
  | If(e, s1, Some(s2)) -> let execvalue = evaluate_expression globalScope scope e in (match execvalue with
		| VBool(b) -> if b then evaluate_statement globalScope scope s1 else evaluate_statement globalScope scope s2
		| VName(name) -> let vbool = find_execvalue_in_scope scope name in (match vbool with
			| VBool(b) -> if b then evaluate_statement globalScope scope s1 else evaluate_statement globalScope scope s2 ))
  | Block b -> List.iter (evaluate_statement globalScope scope) b
  | While(e, s) -> let execvalue = evaluate_expression globalScope scope e in (match execvalue with
		| VBool(b) -> if b then begin evaluate_statement globalScope scope s; evaluate_statement globalScope scope (While(e, s)) end
		| VName(name) -> let vbool = find_execvalue_in_scope scope name in (match vbool with
			| VBool(b) -> if b then begin evaluate_statement globalScope scope s; evaluate_statement globalScope scope (While(e, s)) end ))
(*  | For(l, None, exps, s) ->  List.iter (evaluate_for_expression globalScope scope) exps;List.iter (exec_for_vardecl globalScope scope) l;  evaluate_statement globalScope scope s*)
  | For(l, None, exps, s) -> List.iter (exec_for_vardecl globalScope scope) l;  evaluate_statement globalScope scope s;evaluate_expressions globalScope scope exps; evaluate_statement globalScope scope (For([], None, exps, s))
  | For(l, Some(exp), exps, s) -> List.iter (exec_for_vardecl globalScope scope) l; let execvalue = evaluate_expression globalScope scope exp in (match execvalue with
		| VBool(b) -> if b then begin evaluate_statement globalScope scope s; evaluate_expressions globalScope scope exps; evaluate_statement globalScope scope (For([], Some(exp), exps, s)) end
		| VName(name) -> let vbool = find_execvalue_in_scope scope name in (match vbool with
			| VBool(b) -> if b then begin evaluate_statement globalScope scope s; evaluate_expressions globalScope scope exps; evaluate_statement globalScope scope (For([], Some(exp), exps, s)) end ))


and exec_vardecl globalScope scope decl = match decl with
  | (typ, name, None) -> add_variable_to_scope globalScope scope name VNull
  | (typ, name, Some e) -> add_variable_to_scope globalScope scope name (evaluate_expression globalScope scope e)


and exec_for_vardecl globalScope scope decl =
  match decl with
  | (Some(t), name, None) -> ()
  | (Some(t), name, Some e) -> ()
  | (None, name, None) -> ()
  | (None, name, Some e) -> ()

and evaluate_expressions globalScope scope exps = match exps with
  | [] -> ()
  | (head::liste) -> evaluate_expression globalScope scope head; evaluate_expressions globalScope scope liste;



and addObject typ globalScope scope =
  let addAttributeToObject objectattributes globalScope scope classattribute = match classattribute.adefault with
   			| Some(expr) -> Hashtbl.add objectattributes classattribute.aname (evaluate_expression globalScope scope expr)
			| None -> Hashtbl.add objectattributes classattribute.aname (VNull)
  in

	let createObjectFromDescriptor cd = match cd with
		| ClassDescriptor(classDescriptor) -> let objectcreated = { otype  = classDescriptor.cdname; oattributes = Hashtbl.create 20 } in List.iter (addAttributeToObject objectcreated.oattributes globalScope scope) classDescriptor.cdattributes; ObjectDescriptor(objectcreated)
		| StringClass -> StringDescriptor("")

	in match typ with
  		| Ref(ref_type) -> let cd = Hashtbl.find globalScope.data.classDescriptorTable ref_type.tid in
						let object_created = createObjectFromDescriptor cd in
							Hashtbl.add globalScope.heap globalScope.free_adress object_created; globalScope.free_adress <- globalScope.free_adress+1

and add_variable_to_scope globalScope scope name execvalue = match execvalue with
  | VName(name1) -> Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name (find_execvalue_in_scope scope name1)
  | VString(s) -> add_object_to_heap globalScope (StringDescriptor(s)); Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name (VRef(globalScope.free_adress-1))
  | _ -> Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name execvalue
	

and add_object_to_heap globalScope globalObjectDescriptor =
  Hashtbl.add globalScope.heap globalScope.free_adress globalObjectDescriptor; globalScope.free_adress <- globalScope.free_adress +1


and find_object_in_heap heap execvalue = match execvalue with
  | VRef(i) -> print_endline(string_execvalue(execvalue)); Hashtbl.find heap i


and find_execvalue_in_scope scope name =
  if Hashtbl.mem (List.nth scope.scopelist  scope.currentscope) name <> true
then match scope.currentobject with
					| ObjectDescriptor(od) -> Hashtbl.find od.oattributes name
(*  					| IntegerDescriptor(i) ->*)
(*					| StringDescriptor(s) ->*)
(*  					| NullObject ->*)
else Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name


and replace_execvalue_in_scope scope name execvalue =
  if Hashtbl.mem (List.nth scope.scopelist  scope.currentscope) name <> true
then match scope.currentobject with
					| ObjectDescriptor(od) -> Hashtbl.remove od.oattributes name; Hashtbl.add od.oattributes name execvalue
(*  					| IntegerDescriptor(i) ->*)
(*					| StringDescriptor(s) ->*)
(*  					| NullObject ->*)
else Hashtbl.remove (List.nth scope.scopelist  scope.currentscope) name; Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name execvalue

let execute_program ast compilationData =
  let mainMethod = Hashtbl.find compilationData.methodTable "B_main" in
  print_method "" mainMethod;
  let globalScope = { data = compilationData; currentClassScoped = "B"; heap = Hashtbl.create 20; free_adress = 1 } in
  Hashtbl.add globalScope.heap 0 NullObject;
  let scope = { currentscope = 0; currentobject=NullObject; scopelist = [Hashtbl.create 20] } in
  List.iter (evaluate_statement globalScope scope) mainMethod.mbody;
  printHeap globalScope.heap;
  printScope scope
  
  

  


  
