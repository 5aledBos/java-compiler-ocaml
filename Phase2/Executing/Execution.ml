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
				| String s -> VString(s)
				| Null -> VNull )
  | AssignExp(e1, op, e2) -> let ref1 = evaluate_expression  globalScope scope e1 and ref2 = evaluate_expression globalScope scope e2 in  eval_op op ref1 ref2 globalScope scope
  | Name(name) -> (*let execvalue = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name in print_endline("found ref: "); execvalue;*) VName(name)

  | Call(Some(exp), str, l) -> (match exp.edesc with Name(id) -> let o = find_object_in_heap globalScope.heap (Hashtbl.find (List.nth scope.scopelist  scope.currentscope) (id)) in (match o with ObjectDescriptor(od) -> let m = Hashtbl.find globalScope.data.methodTable (od.otype ^ "_" ^ str) in  globalScope.currentClassScoped <- od.otype; scope.currentscope <- scope.currentscope +1;scope.scopelist <- scope.scopelist @ [Hashtbl.create 20]; scope.currentobject <- o;let returnvalue = execute_method m globalScope scope l in  globalScope.currentClassScoped <- od.otype; scope.scopelist <- remove_at scope.currentscope scope.scopelist; scope.currentscope <- scope.currentscope-1;  returnvalue ))

  | Call(None, str, l) -> let m = Hashtbl.find globalScope.data.methodTable (globalScope.currentClassScoped ^ "_" ^ str) in execute_method m globalScope scope; VNull

and eval_op op v1 v2 globalScope scope = match op with
  | Assign -> (match v1, v2 with
					| VName(name1), VName(name2) -> Hashtbl.remove (List.nth scope.scopelist  scope.currentscope) name1; let ref_nb2 = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name2 in (match ref_nb2 with 
						| VRef(i) -> (*Hashtbl.remove globalScope.heap ref1;*) Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name1 (VRef(i)); VRef(i) )
					| VName(name1), VString(s) -> let execvalue = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name1 in (match execvalue with
						| VRef(i) ->  Hashtbl.remove globalScope.heap i; Hashtbl.add globalScope.heap i (StringDescriptor(s)); VRef(i)
						| VNull -> Hashtbl.add globalScope.heap globalScope.free_adress (StringDescriptor(s)); Hashtbl.remove (List.nth scope.scopelist  scope.currentscope) name1; Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name1 (VRef(globalScope.free_adress)); globalScope.free_adress <- globalScope.free_adress + 1; VNull )
					| VName(name1), VNull -> VNull
					| VName(name1), VInt(i) -> replace_execvalue_in_scope scope name1 (VInt(i)); VInt(i)
)
  | Ass_add -> VNull

and execute_method m globalScope scope params =
	  print_endline("*********executing method**************");
	 List.iter2 (addParameterToScope globalScope scope) m.margstype params;
	 List.iter (evaluate_statement globalScope scope) m.mbody;
	if Hashtbl.mem (List.nth scope.scopelist  scope.currentscope) "return" <> true then VNull else
	 Hashtbl.find (List.nth scope.scopelist  scope.currentscope) "return"

and addParameterToScope globalScope scope marg param  =
  add_variable_to_scope scope marg.pident (evaluate_expression globalScope scope param)

and evaluate_statement globalScope scope stmt = match stmt with
  | VarDecl(l) -> List.iter (exec_vardecl globalScope scope) l
  | Expr e -> evaluate_expression globalScope scope e; ()
  | Return Some(e) -> add_variable_to_scope scope "return" (evaluate_expression globalScope scope e)
  | Return None -> add_variable_to_scope scope "return" VNull

and exec_vardecl globalScope scope decl = match decl with
  | (typ, name, None) -> add_variable_to_scope scope name VNull
  | (typ, name, Some e) -> add_variable_to_scope scope name (evaluate_expression globalScope scope e)

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

and add_variable_to_scope scope name execvalue = 
	print_endline(string_of_int scope.currentscope);
	Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name execvalue

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
  
  

  


  
