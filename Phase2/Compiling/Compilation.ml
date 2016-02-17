open AST
open Hashtbl

type objectDescriptor =
{
	name : string;
	attributes : astattribute list
}

type classDescriptor =
{
	name : string;
	methodList : string list
}

type globalClassDescriptor =
  | ClassDescriptor of classDescriptor
  | ObjectClass
  | StringClass
  | IntegerClass
  | BooleanClass

type globalData = 
{
  methodTable : (string, string) Hashtbl.t;
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
  Hashtbl.iter (fun key value -> print_string(key)) cdtable

let printMethodTable mtable =
  print_endline("list of the methods of all classes");
  Hashtbl.iter (fun key value -> print_endline(key) ) mtable

(*compile/fonction read AST
add method, addattribute

*)

let compile fileAst = 
  let compilationData = { methodTable = Hashtbl.create 20; classDescriptorTable = Hashtbl.create 20 }
  in
  addPredifinedClassesToDescriptorTable compilationData.classDescriptorTable;
  compilationData

let printCompilationData data = match data with
  | {methodTable = mtable; classDescriptorTable = cdtable } -> printMethodTable(mtable); printClassDescriptorTable(cdtable)


  
