open Compilation
open AST
open Type

type globalScope =
{
  data : globalData;
  mutable current : string;
  heap : (int, globalObjectDescriptor) Hashtbl.t;
  mutable free_adress : int;
(*  stack : exec_scope list*)
}

let printHeap heap =
   Hashtbl.iter (fun key value -> (Printf.printf "%i " key; printObjectDescriptor(value); print_endline(""))) heap

type exec_scope =
{
  scopedObjects : (string, int) Hashtbl.t;
}


(*  *)

let rec evaluate_expression expr = match expr.edesc with
  | New(None, l, exps) -> print_endline("ici"); VInt (int_of_string "5")
(*  | New(Some(str), l, exps) -> print_endline("ici"); VInt (int_of_string "55")*)
  | Val v -> match v with
				| Int(i) -> VInt (int_of_string i)


let rec addObject typ oname globalScope evalue =
  let addAttributeToObject objectattributes globalScope classattribute = addObject classattribute.atype classattribute.aname globalScope classattribute.adefault; Hashtbl.add objectattributes classattribute.aname (globalScope.free_adress-1)
  in

	let createObjectFromDescriptor cd oname = match cd with
		| ClassDescriptor(classDescriptor) -> let objectcreated = { otype  = classDescriptor.cdname; oname = oname; oattributes = Hashtbl.create 20 } in List.iter (addAttributeToObject objectcreated.oattributes globalScope) classDescriptor.cdattributes; ObjectDescriptor(objectcreated)
		| StringClass -> StringDescriptor("")

	in (match typ, evalue with
  		| Primitive(Int), Some(e) -> let i = evaluate_expression e in (match i with VInt(value_of_int) -> Hashtbl.add globalScope.heap 	globalScope.free_adress (IntegerDescriptor(value_of_int)); globalScope.free_adress <- globalScope.free_adress+1)
 		| Primitive(Int), None -> Hashtbl.add globalScope.heap globalScope.free_adress (IntegerDescriptor(0)); globalScope.free_adress <- globalScope.free_adress+1;
  		| Ref(ref_type), Some(e) -> let cd = Hashtbl.find globalScope.data.classDescriptorTable ref_type.tid in
						let object_created = createObjectFromDescriptor cd oname in
							Hashtbl.add globalScope.heap globalScope.free_adress object_created; globalScope.free_adress <- globalScope.free_adress+1
  		| Ref(ref_type), None -> Hashtbl.add globalScope.heap globalScope.free_adress NullObject; globalScope.free_adress <- globalScope.free_adress+1)



let exec_vardecl globalScope decl =
  match decl with
  | (typ, name, None) -> addObject typ name globalScope None
  | (typ, name, Some e) -> addObject typ name globalScope (Some(e))
(*    if Some(t) <> e.etype then raise(CheckAST.Type_mismatch_decl(Some(t), e.etype)) else add_variable scope name t*)

let evaluate_statement globalScope stmt = match stmt with
  | VarDecl(l) -> List.iter (exec_vardecl globalScope) l

(*  print_endline("evaluate statement...")*)




let execute_program ast compilationData =
  let mainMethod = Hashtbl.find compilationData.methodTable "B_main" in
  print_method "" mainMethod;
  let globalScope = { data = compilationData; current = "main"; heap = Hashtbl.create 20; free_adress = 1 } in
  Hashtbl.add globalScope.heap 0 NullObject; 
  List.iter (evaluate_statement globalScope) mainMethod.mbody; (*{ scopedObjects = Hashtbl.create 20 }*)
  printHeap globalScope.heap
  
  

  


  
