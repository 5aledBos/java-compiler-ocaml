open AST
open Hashtbl


type execValue =
  | VInt of int
  | VBool of bool
  | VString of string
  | VRef of int
  | VNull 

type objectDescriptor =
{
    otype : string;
	oname : string;
	oattributes : (string, execValue) Hashtbl.t;
}

type globalObjectDescriptor =
  | ObjectDescriptor of objectDescriptor
  | IntegerDescriptor of int

let printObjectDescriptor od = match od with
  |IntegerDescriptor(i) -> Printf.printf "Integer object descriptor: %i" i

type classDescriptor =
{
	cdname : string;
	cdmethods : (string, string) Hashtbl.t;
    cdattributes : astattribute list
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


let addPredifinedClassesToDescriptorTable descriptorTable =
  let cd = { cdname = "Object"; cdmethods = Hashtbl.create 20; cdattributes = [] } in
  Hashtbl.add descriptorTable "Object" (ObjectClass(cd));
  Hashtbl.add descriptorTable"String" StringClass;
  Hashtbl.add descriptorTable "Int" IntegerClass;
  Hashtbl.add descriptorTable "Boolean" BooleanClass

let printClassDescriptor cd = match cd with
  | ClassDescriptor(cd) -> print_endline("*****Method of the class*****");  Hashtbl.iter (fun key value -> print_endline(key ^":   " ^ value)) cd.cdmethods; print_endline("*****End of Method of the class*****");
							print_endline("*****attributes of the class*****"); List.iter (print_attribute ("  ")) cd.cdattributes;print_endline("*****end of attributes of the class*****") 
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


let addMethodsToClassDesciptor cname methods methodTable m =
  let methodName = cname ^ "_" ^ m.mname in
  Hashtbl.add methods m.mname (methodName);
  Hashtbl.add methodTable methodName m

let addMethodsFromParent cdmethods parentcd = match parentcd with
  | ClassDescriptor(cd) -> Hashtbl.iter (fun key value -> if(Hashtbl.mem cdmethods key) <> true then Hashtbl.add cdmethods key value) cd.cdmethods
  | ObjectClass(cd) -> Hashtbl.iter (fun key value -> Hashtbl.add cdmethods key value) cd.cdmethods
  

let addAtributesFromParent cattributes parentcd = match parentcd with
  | ClassDescriptor(cd) -> cd.cdattributes @ cattributes
  | ObjectClass(cd) -> cd.cdattributes @ cattributes

let printParentAttributes parentcd = match parentcd with
  | ClassDescriptor(cd) -> List.iter (print_attribute (" parentattribute ")) cd.cdattributes
  | ObjectClass(cd) -> List.iter (print_attribute (" parentattribute ")) cd.cdattributes

let addMethodsAndAttributesToDescriptors classDescriptorTable methodTable c cname =
 print_endline("------------compilation of the class " ^ cname ^ " -----------------");
	  let methods = Hashtbl.create 20 in
(*	  print_endline(c.cparent.tid);*)
      let parentClassDescriptor = Hashtbl.find classDescriptorTable c.cparent.tid in
      List.iter (addMethodsToClassDesciptor cname methods methodTable) c.cmethods;
	  addMethodsFromParent methods (parentClassDescriptor);
	  let attributes = addAtributesFromParent c.cattributes parentClassDescriptor in
	  Hashtbl.add classDescriptorTable cname (ClassDescriptor({ cdname = cname; cdmethods = methods; cdattributes = attributes }))

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

  
