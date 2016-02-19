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
	cdmethods : (string, string) Hashtbl.t;
    attributes : astattribute list
}

type globalClassDescriptor =
  | ClassDescriptor of classDescriptor
  | ObjectClass of classDescriptor
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
  let cd = { cdname = "Object"; cdmethods = Hashtbl.create 20; attributes = [] } in
  Hashtbl.add descriptorTable "Object" (ObjectClass(cd));
  Hashtbl.add descriptorTable"String" StringClass;
  Hashtbl.add descriptorTable "Int" IntegerClass;
  Hashtbl.add descriptorTable "Boolean" BooleanClass

let printClassDescriptor cd = match cd with
  | ClassDescriptor(cd) -> print_endline("*****Method of the class*****");  Hashtbl.iter (fun key value -> print_endline(key)) cd.cdmethods;
							print_endline("*****attributes of the class*****"); List.iter (print_attribute ("  ")) cd.attributes 
  | ObjectClass(cd) -> ()
  | StringClass -> ()
  | IntegerClass -> ()
  | BooleanClass -> ()
  

let printClassDescriptorTable cdtable =
  print_endline("list of the classes");
  Hashtbl.iter (fun key value -> print_endline(key); printClassDescriptor(value)) cdtable

let printMethodTable mtable =
  print_endline("list of the methods of all classes");
  Hashtbl.iter (fun key value -> print_endline(key)) mtable


let addMethodsToClassDesciptor cd methodTable m =
  let methodName = cd.cdname ^ "_" ^ m.mname in
  Hashtbl.add cd.cdmethods m.mname (methodName);
  Hashtbl.add methodTable methodName m

let addMethodsFromParent cdmethods parentcd = match parentcd with
  | ClassDescriptor(cd) -> Hashtbl.iter (fun key value -> Hashtbl.add cdmethods key value) cd.cdmethods
  | ObjectClass(cd) -> Hashtbl.iter (fun key value -> Hashtbl.add cdmethods key value) cd.cdmethods
  

let addMethodsAndAttributesToDescriptors classDescriptorTable methodTable c cname =
 print_endline("------------compilation of the class " ^ cname ^ " -----------------");
 let cd = { cdname = cname; cdmethods = Hashtbl.create 20; attributes = c.cattributes  } in
	  print_endline(c.cparent.tid);
      addMethodsFromParent cd.cdmethods (Hashtbl.find classDescriptorTable c.cparent.tid);
      List.iter (addMethodsToClassDesciptor cd methodTable) c.cmethods;
	  Hashtbl.add classDescriptorTable cd.cdname (ClassDescriptor(cd))

let isCompiled cname classDescriptorTable = match cname with
  | "Object" -> true
  | _ -> Hashtbl.mem classDescriptorTable cname

let rec findParentClass cname typelist = match typelist with
  | head::liste -> if head.id = cname then head else findParentClass cname liste

let rec compileClass classDescriptorTable methodTable ast ttype = match ttype.info with
 | Class c -> if (isCompiled ttype.id classDescriptorTable) = false then begin
				if isCompiled c.cparent.tid classDescriptorTable then
					begin addMethodsAndAttributesToDescriptors classDescriptorTable methodTable c ttype.id end
				else begin
				let parenttype = findParentClass c.cparent.tid ast.type_list in
				  compileClass classDescriptorTable methodTable ast parenttype; addMethodsAndAttributesToDescriptors classDescriptorTable methodTable c ttype.id end
				end
 | Inter -> ()
  

let compile ast = 
  let compilationData = { methodTable = Hashtbl.create 20; classDescriptorTable = Hashtbl.create 20 }
  in
  addPredifinedClassesToDescriptorTable compilationData.classDescriptorTable;
  List.iter (compileClass compilationData.classDescriptorTable compilationData.methodTable ast) ast.type_list;
  compilationData

let printCompilationData data = match data with
  | {methodTable = mtable; classDescriptorTable = cdtable } -> printMethodTable(mtable); printClassDescriptorTable(cdtable)

  
