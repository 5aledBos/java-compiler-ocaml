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


(*let addAttributesToObject objectattributes classattribute = match classattribute.atype with*)
(*(*  | {amodifiers : modifiers; aname = attrname; atype = attrtype; adefault = defaultvalue;(*      aloc : Location.t;*)} ->  *)*)
(*  | Primitive(primitive) -> match primitive with*)
(*  								| Boolean -> match classattribute.adefault with*)
(*									| None -> Hashtbl.add objectattributes classattribute.aname Bool(true)*)
(*									| Some(Bool(b)) -> Hashtbl.add objectattributes classattribute.aname Bool(b)*)
(*								| Int -> match classattribute.adefault with*)
(*									| None -> Hashtbl.add objectattributes classattribute.aname Int(0)*)
(*  									| Some(Int(i)) -> Hashtbl.add objectattributes classattribute.aname Int(i)*)



let createObjectFromDescriptor cd oname = match cd with
(*  | ClassDescriptor(classDescriptor) -> let objectattributes = addAttributeToObject classDescriptor.cdattributes in*)
  | ClassDescriptor(classDescriptor) -> IntegerDescriptor(30)
(*  | ObjectClass of classDescriptor*)
(*  | StringClass*)
  | IntegerClass -> IntegerDescriptor(0)
(*  | BooleanClass*)
  

(*let addObjectToHeap heap adress globalClassDescriptor =*)
(*  Hashtbl.add heap adress globalClassDescriptor;*)
(*  adress = adress + 1;*)
(*  Printf/printf " "*)
(*  print_endline("test")*)

let addObject typ oname globalScope = match typ with
  | Primitive(Int) -> Hashtbl.add globalScope.heap globalScope.free_adress (IntegerDescriptor(0)); globalScope.free_adress <- globalScope.free_adress+1;
  | Ref(ref_type) -> let cd = Hashtbl.find globalScope.data.classDescriptorTable ref_type.tid in
						let object_created = createObjectFromDescriptor cd oname in
							Hashtbl.add globalScope.heap globalScope.free_adress object_created; globalScope.free_adress <- globalScope.free_adress+1
(*  | Primitive(Boolean) ->*)
(*  | *)
(*  let cd = Hashtbl.find globalScope.data.classDescriptorTable cname in*)
(*  let objectdescr =  { otype = cname; oname = oname; oattributes : Hashtbl.create 20 } in*)
(*  match cd with*)
(*    | ClassDescriptor(classe) -> List.iter (addAttributeToObject objectdescr.oattributes) classe.cdattributes*)
(*    | IntegerClass ->  addObjectToHeap *)
  

let addObjectWithValue typ oname globalScope value =  match typ, value with
  | Primitive(Int), VInt(i) -> Hashtbl.add globalScope.heap globalScope.free_adress (IntegerDescriptor(i)); globalScope.free_adress <- globalScope.free_adress+1
  | Ref(ref_type), VInt(i) -> let cd = Hashtbl.find globalScope.data.classDescriptorTable ref_type.tid in
						let object_created = createObjectFromDescriptor cd oname in
							Hashtbl.add globalScope.heap globalScope.free_adress object_created; globalScope.free_adress <- globalScope.free_adress+1
(*  *)

let rec evaluate_expression expr = match expr.edesc with
  | New(None, l, exps) -> print_endline("ici"); VInt (int_of_string "5")
  | Val v -> match v with
				| Int(i) -> VInt (int_of_string i)

let exec_vardecl globalScope decl =
  match decl with
  | (typ, name, None) -> addObject typ name globalScope
  | (typ, name, Some e) -> let value = evaluate_expression e in addObjectWithValue typ name globalScope value
(*    if Some(t) <> e.etype then raise(CheckAST.Type_mismatch_decl(Some(t), e.etype)) else add_variable scope name t*)

let evaluate_statement globalScope stmt = match stmt with
  | VarDecl(l) -> List.iter (exec_vardecl globalScope) l

(*  print_endline("evaluate statement...")*)

let execute_program ast compilationData =
  let mainMethod = Hashtbl.find compilationData.methodTable "B_main" in
  print_method "" mainMethod;
  let globalScope = { data = compilationData; current = "main"; heap = Hashtbl.create 20; free_adress = 0 } in
  List.iter (evaluate_statement globalScope) mainMethod.mbody; (*{ scopedObjects = Hashtbl.create 20 }*)
  printHeap globalScope.heap
  
  

  


  
