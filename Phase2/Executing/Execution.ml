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
  mutable scopelist : (string, int) Hashtbl.t list
}

let printScope scope =
	Hashtbl.iter (fun key value -> print_string("object: " ^ key); Printf.printf ", adresse in heap:  %i " value; print_endline("")) (List.nth scope.scopelist  scope.currentscope)

(* constructor calls *)
(*let compare_args_function_args args constargs = *)
(*  let isEqual = true in*)
(*      List.iter2 (compare_args isEqual) args constargs; isEqual*)

(*let compare_args isEqual a.etype b.ptype = if (a.pident == b.pidend) <> true then isEqual = true else isEqual = false*)

(*let rec findConstructor constructors exps = match constructors with*)
(*  | (head::liste) -> if (compare_args_function_args exps head.cargstype) then head else findConstructor liste exps*)



let rec evaluate_expression globalScope scope expr = match expr.edesc with
  | New(None, l, exps) -> (*let (last, lst) = ListII.extract_last l =  in globalScope.currentClassScoped <- last; let cd = Hashtbl.find globalScope.classDescriptorTable l in let constr = findConstructor cd exps;*)  VNull
(*  | New(Some(str), l, exps) -> print_endline("ici"); VInt (int_of_string "55")*)
  | Val v -> (match v with
				| Int(i) -> VInt (int_of_string i)
				| String s -> VString s )
  | AssignExp(e1, op, e2) -> let ref1 = evaluate_expression  globalScope scope e1 and ref2 = evaluate_expression globalScope scope e2 in  eval_op op ref1 ref2 globalScope scope
  | Name(name) -> let ref_nb = Hashtbl.find (List.nth scope.scopelist  scope.currentscope) name in print_endline("found ref: " ^ string_of_int ref_nb); VRef(name, ref_nb)
  | Call(Some(exp), str, l) -> (match exp.edesc with Name(id) -> let o = Hashtbl.find globalScope.heap (Hashtbl.find (List.nth scope.scopelist  scope.currentscope) (id)) in (match o with ObjectDescriptor(od) -> let m = Hashtbl.find globalScope.data.methodTable (od.otype ^ "_" ^ str) in execute_method m globalScope scope l ; (*List.iter (evaluate_expression globalScope) l)*); VNull ))
  | Call(None, str, l) -> let m = Hashtbl.find globalScope.data.methodTable (globalScope.currentClassScoped ^ "_" ^ str) in execute_method m globalScope scope; VNull

and eval_op op v1 v2 globalScope scope = match op with
  | Assign -> (match v1, v2 with
					| VRef(name1, ref1), VRef(name2, ref2) -> Hashtbl.remove (List.nth scope.scopelist  scope.currentscope) name1; Hashtbl.remove globalScope.heap ref1; Hashtbl.add (List.nth scope.scopelist  scope.currentscope) name1 ref2; VRef(name1, ref2) 
					| VRef(name1, ref1), VString(s) -> Hashtbl.remove globalScope.heap ref1; Hashtbl.add globalScope.heap ref1 (StringDescriptor(s)); VRef(name1, ref1)  )
  | Ass_add -> VNull

and execute_method m globalScope scope params =
	  print_endline("*********executing method**************");
	 List.iter2 (addParameterToScope globalScope scope) params m.margstype;
	 List.iter (evaluate_statement globalScope scope) m.mbody

and addParameterToScope globalScope scope param marg = match param.edesc with 
  | Name(id) -> Hashtbl.add (List.nth scope.scopelist  scope.currentscope) marg.pident (Hashtbl.find (List.nth scope.scopelist  scope.currentscope) id); print_endline(marg.pident ^ id)
  | Val v -> (match v with | Int(i) -> Hashtbl.add (List.nth scope.scopelist  scope.currentscope) marg.pident globalScope.free_adress; Hashtbl.add globalScope.heap globalScope.free_adress (IntegerDescriptor(int_of_string(i)))); globalScope.free_adress <- globalScope.free_adress + 1

and evaluate_statement globalScope scope stmt = match stmt with
  | VarDecl(l) -> List.iter (exec_vardecl globalScope scope) l
  | Expr e -> evaluate_expression globalScope scope e; ()
  | Return Some(e) -> ()
  | Return None -> ()

and exec_vardecl globalScope scope decl = match decl with
  | (typ, name, None) -> addObject typ name globalScope scope None
  | (typ, name, Some e) -> addObject typ name globalScope scope (Some(e))



and addObject typ oname globalScope scope evalue =
  let addAttributeToObject objectattributes globalScope scope classattribute = addObject classattribute.atype classattribute.aname globalScope scope classattribute.adefault; Hashtbl.add objectattributes classattribute.aname (globalScope.free_adress-1)
  in

	let createObjectFromDescriptor cd oname = match cd with
		| ClassDescriptor(classDescriptor) -> let objectcreated = { otype  = classDescriptor.cdname; oname = oname; oattributes = Hashtbl.create 20 } in List.iter (addAttributeToObject objectcreated.oattributes globalScope scope) classDescriptor.cdattributes; ObjectDescriptor(objectcreated)
		| StringClass -> (match evalue with Some(value) -> let stringvalue = evaluate_expression globalScope scope value in (match stringvalue with VString(s) -> StringDescriptor(s)))

	in (match typ, evalue with
  		| Primitive(Int), Some(e) -> let i = evaluate_expression globalScope scope e in (match i with VInt(value_of_int) -> Hashtbl.add globalScope.heap globalScope.free_adress (IntegerDescriptor(value_of_int)); Hashtbl.add (List.nth scope.scopelist  scope.currentscope) oname globalScope.free_adress; globalScope.free_adress <- globalScope.free_adress+1)
 		| Primitive(Int), None -> Hashtbl.add globalScope.heap globalScope.free_adress (IntegerDescriptor(0)); Hashtbl.add (List.nth scope.scopelist  scope.currentscope) oname globalScope.free_adress; globalScope.free_adress <- globalScope.free_adress+1;
  		| Ref(ref_type), Some(e) -> let cd = Hashtbl.find globalScope.data.classDescriptorTable ref_type.tid in
						let object_created = createObjectFromDescriptor cd oname in
							Hashtbl.add globalScope.heap globalScope.free_adress object_created; Hashtbl.add (List.nth scope.scopelist  scope.currentscope) oname globalScope.free_adress; globalScope.free_adress <- globalScope.free_adress+1; evaluate_expression globalScope scope e; ()
  		| Ref(ref_type), None -> Hashtbl.add globalScope.heap globalScope.free_adress NullObject; Hashtbl.add (List.nth scope.scopelist  scope.currentscope) oname globalScope.free_adress; globalScope.free_adress <- globalScope.free_adress+1)




let execute_program ast compilationData =
  let mainMethod = Hashtbl.find compilationData.methodTable "B_main" in
  print_method "" mainMethod;
  let globalScope = { data = compilationData; currentClassScoped = "B"; heap = Hashtbl.create 20; free_adress = 2 } in
  Hashtbl.add globalScope.heap 0 NullObject;
  let scope = { currentscope = 0; scopelist = [Hashtbl.create 20] } in
  List.iter (evaluate_statement globalScope scope) mainMethod.mbody;
  printHeap globalScope.heap;
  printScope scope
  
  

  


  
