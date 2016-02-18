open AST
open Hashtbl

type objectDescriptor =
{
	name : string;
	attributes : astattribute list
}

type classDescriptor =
{
	cdname : string;
	cdmethods : (string, string) Hashtbl.t
}

type globalClassDescriptor =
  | ClassDescriptor of classDescriptor
  | ObjectClass
  | StringClass
  | IntegerClass
  | BooleanClass

type globalData = 
{
  methodTable : (string, astmethod) Hashtbl.t;
  classDescriptorTable : (string, globalClassDescriptor) Hashtbl.t
}

(*type methodTable = (string, astmethod) Hashtbl*)

(*type descriptorClassTable = (string, classDescriptor) Hashtbl*)

let addPredifinedClassesToDescriptorTable descriptorTable =
  Hashtbl.add descriptorTable "Object" ObjectClass;
  Hashtbl.add descriptorTable"String" StringClass;
  Hashtbl.add descriptorTable "Int" IntegerClass;
  Hashtbl.add descriptorTable "Boolean" BooleanClass

let printClassDescriptorTable cdtable =
  print_endline("list of the classes");
  Hashtbl.iter (fun key value -> print_endline(key)) cdtable

let printMethodTable mtable =
  print_endline("list of the methods of all classes");
  Hashtbl.iter (fun key value -> print_endline(key) ) mtable

(*compile/fonction read AST
add method, addattribute

*)

let addMethodsToClassDesciptor cd methodTable m =
  let methodName = cd.cdname ^ "_" ^ m.mname in
  Hashtbl.add cd.cdmethods m.mname (methodName);
  Hashtbl.add methodTable methodName m

let addMethodsAndAttributesToDescriptors classDescriptorTable methodTable ttype = match ttype.info with
 | Class c -> let cd = { cdname = ttype.id; cdmethods = Hashtbl.create 20  } in
      List.iter (addMethodsToClassDesciptor cd methodTable) c.cmethods; Hashtbl.add classDescriptorTable cd.cdname (ClassDescriptor(cd))
 | Inter -> ()


let compile ast = 
  let compilationData = { methodTable = Hashtbl.create 20; classDescriptorTable = Hashtbl.create 20 }
  in
  addPredifinedClassesToDescriptorTable compilationData.classDescriptorTable;
  List.iter (addMethodsAndAttributesToDescriptors compilationData.classDescriptorTable compilationData.methodTable) ast.type_list;
(*  match fileAst with*)
(*  | { package = p ; type_list = cl } -> *)
  compilationData

let printCompilationData data = match data with
  | {methodTable = mtable; classDescriptorTable = cdtable } -> printMethodTable(mtable); printClassDescriptorTable(cdtable)

  
