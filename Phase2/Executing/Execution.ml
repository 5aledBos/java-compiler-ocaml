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
  | New(None, l, exps) -> (match expr.etype with Some(ref_type) -> let ref_nb = globalScope.free_adress in addObject ref_type "" globalScope scope; VRef(ref_nb) )
(*  | New(Some(str), l, exps) -> print_endline("ici"); VInt (int_of_string "55")*)
  | Val v -> (match v with
				| Int(i) -> VInt (int_of_string i)
				| String s -> VString s
				| Null -> VNull )
  | AssignExp(e1, op, e2) -> let ref1 = evaluate_expression  globalScope scope e1 and ref2 = evaluate_expression globalScope scope e2 in  eval_op op ref1 ref2 globalScope scope
  | Name(name) -> (*let execvalue = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name in print_endline("found ref: "); execvalue;*) VString(name)
(*  | Call(Some(exp), str, l) -> (match exp.edesc with Name(id) -> let o = Hashtbl.find globalScope.heap (Hashtbl.find (List.nth scope.scopelist  scope.currentscope) (id)) in (match o with ObjectDescriptor(od) -> let m = Hashtbl.find globalScope.data.methodTable (od.otype ^ "_" ^ str) in execute_method m globalScope scope l ; (*List.iter (evaluate_expression globalScope) l)*); VNull ))*)
  | Call(None, str, l) -> let m = Hashtbl.find globalScope.data.methodTable (globalScope.currentClassScoped ^ "_" ^ str) in execute_method m globalScope scope; VNull

and eval_op op v1 v2 globalScope scope = match op with
  | Assign -> (match v1, v2 with
					| VString(name1), VString(name2) -> Hashtbl.remove (List.nth scope.scopelist  scope.currentscope) name1; let ref_nb2 = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name2 in (match ref_nb2 with VRef(i) -> (*Hashtbl.remove globalScope.heap ref1;*) Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name1 (VRef(i)); VRef(i) ) 
					| VRef(ref1), VString(s) -> Hashtbl.remove globalScope.heap ref1; Hashtbl.add globalScope.heap ref1 (StringDescriptor(s)); VRef(ref1)  )
  | Ass_add -> VNull

and execute_method m globalScope scope params =
	  print_endline("*********executing method**************");
(*	 List.iter2 (addParameterToScope globalScope scope) params m.margstype;*)
	 List.iter (evaluate_statement globalScope scope) m.mbody

(*and addParameterToScope globalScope scope param marg = match param.edesc with *)
(*  | Name(id) -> Hashtbl.add (List.nth scope.scopelist  scope.currentscope) marg.pident (Hashtbl.find (List.nth scope.scopelist  scope.currentscope) id); print_endline(marg.pident ^ id)*)
(*  | Val v -> (match v with | Int(i) -> Hashtbl.add (List.nth scope.scopelist  scope.currentscope) marg.pident globalScope.free_adress; Hashtbl.add globalScope.heap globalScope.free_adress (IntegerDescriptor(int_of_string(i)))); globalScope.free_adress <- globalScope.free_adress + 1*)

and evaluate_statement globalScope scope stmt = match stmt with
  | VarDecl(l) -> List.iter (exec_vardecl globalScope scope) l
  | Expr e -> evaluate_expression globalScope scope e; ()
  | Return Some(e) -> ()
  | Return None -> ()

and exec_vardecl globalScope scope decl = match decl with
  | (typ, name, None) -> add_variable_to_scope scope name VNull
  | (typ, name, Some e) -> add_variable_to_scope scope name (evaluate_expression globalScope scope e)
(*	addObject typ name globalScope scope (Some(e))*)


and addObject typ oname globalScope scope =
  let addAttributeToObject objectattributes globalScope scope classattribute = match classattribute.adefault with
   			| Some(expr) -> Hashtbl.add objectattributes classattribute.aname (evaluate_expression globalScope scope expr); globalScope.free_adress <- globalScope.free_adress+1
			| None -> Hashtbl.add objectattributes classattribute.aname (VNull); globalScope.free_adress <- globalScope.free_adress+1
  in

	let createObjectFromDescriptor cd oname = match cd with
		| ClassDescriptor(classDescriptor) -> let objectcreated = { otype  = classDescriptor.cdname; oname = oname; oattributes = Hashtbl.create 20 } in List.iter (addAttributeToObject objectcreated.oattributes globalScope scope) classDescriptor.cdattributes; ObjectDescriptor(objectcreated)
(*		| StringClass -> (match evalue with Some(value) -> let stringvalue = evaluate_expression globalScope scope value in (match stringvalue with VString(s) -> StringDescriptor(s)))*)

	in (match typ with
  		| Ref(ref_type) -> let cd = Hashtbl.find globalScope.data.classDescriptorTable ref_type.tid in
						let object_created = createObjectFromDescriptor cd oname in
							Hashtbl.add globalScope.heap globalScope.free_adress object_created; (*Hashtbl.add (List.nth scope.scopelist  scope.currentscope) oname (VRef(globalScope.free_adress))*); globalScope.free_adress <- globalScope.free_adress+1; )

and add_variable_to_scope scope name execvalue = 
	Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name execvalue


let execute_program ast compilationData =
  let mainMethod = Hashtbl.find compilationData.methodTable "B_main" in
  print_method "" mainMethod;
  let globalScope = { data = compilationData; currentClassScoped = "B"; heap = Hashtbl.create 20; free_adress = 2 } in
  Hashtbl.add globalScope.heap 0 NullObject;
  let scope = { currentscope = 0; scopelist = [Hashtbl.create 20] } in
  List.iter (evaluate_statement globalScope scope) mainMethod.mbody;
  printHeap globalScope.heap;
  printScope scope
  
  

  


  
