open AST
open Hashtbl


(* Pour l'exécution, on ne travaille que sur les types primitifs int et bool*)
(* ce type est fait pour les valeurs d'exécution*)
type exec_Value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VRef of int				(* valeur qui correspond à l'adresse des objets contenus dans le tas*)
  | VNull 
  | VName of string 	(* valeur particulière qui correspond à l'utilisation de nom variable dans un programme*)
  | VAttr of string * string  		(* valeur particulière qui correspond à l'utilisation de nom d'attributs d'objet dans un programme'*)


let string_execvalue value = match value with
  | VInt(i) -> "int: " ^ string_of_int i
  | VBool(b) -> "bool: " ^ string_of_bool b
  | VName(name) -> "name: " ^ name
  | VString(s) -> "string: " ^ s
  | VRef(i) -> "object reference, adresse in heap: " ^ string_of_int i
  | VNull -> "null reference"
  | VAttr(str1, str2) -> "Atrtribute " ^ str2 ^ "from the object " ^ str1
  

(* *)
type objectDescriptor =
{
    otype : string;
(*	oname : string;*)
	oattributes : (string, exec_Value) Hashtbl.t;
}

type globalObjectDescriptor =
  | ObjectDescriptor of objectDescriptor
  | IntegerDescriptor of int
  | StringDescriptor of string
  | NullObject
  | ThisObject		(* non utilisé on ne type pas et n'exécute pas les expressions avec le mot clé "this"*)

let printObjectDescriptor od = match od with
  | IntegerDescriptor(i) -> Printf.printf "Integer object descriptor: %i" i; print_endline("")
  | StringDescriptor(str) -> print_endline("String object descriptor: " ^ str)
  | ObjectDescriptor(od) -> print_endline("object: "^ " from type " ^ od.otype); Hashtbl.iter (fun key value -> print_string(key);print_endline("\t:    attribute: " ^string_execvalue(value)); print_endline("")) od.oattributes
  | NullObject -> print_endline("Null object")
	

type classDescriptor =
{
	cdname : string;
	cdmethods : (string, string) Hashtbl.t;		(* type de la table est (nom de la méthode, nom de la méthode telle qu'elle est dans la 												table des méthodes par exemple "classe_method" *)
	cdconstructors : (string, astconst) Hashtbl.t;
    cdattributes : astattribute list
}

type globalClassDescriptor =
  | ClassDescriptor of classDescriptor
  | ObjectClass of classDescriptor
  | StringClass
  | IntegerClass
  | BooleanClass


(* type de données qui sort avec la compilation du fichier*)
type globalData = 
{
  methodTable : (string, astmethod) Hashtbl.t;
  classDescriptorTable : (string, globalClassDescriptor) Hashtbl.t
}


(* On ajoute toutes les classes pré-définies à la table des méthodes, on a défini un type exception générique plus, quelques types d'exécution en plus pour gérer les exceptions dans le fichier java*)
let addPredifinedClassesToDescriptorTable descriptorTable =
  let cd = { cdname = "Object"; cdmethods = Hashtbl.create 20; cdconstructors = Hashtbl.create 10; cdattributes = [] } in
  Hashtbl.add descriptorTable "Object" (ObjectClass(cd));
  let cdexception = { cdname = "Exception"; cdmethods = Hashtbl.create 20; cdconstructors = Hashtbl.create 10; cdattributes = [] } in 
  Hashtbl.add descriptorTable "NullPointerException" (ClassDescriptor(cdexception));
  Hashtbl.add descriptorTable "RuntimeException" (ClassDescriptor(cdexception));
  Hashtbl.add descriptorTable "InvalideOperationException" (ClassDescriptor(cdexception));
  Hashtbl.add descriptorTable "ArithmeticException" (ClassDescriptor(cdexception));
  Hashtbl.add descriptorTable "ClassCastException" (ClassDescriptor(cdexception));
  Hashtbl.add descriptorTable "Exception" (ClassDescriptor(cdexception));
  Hashtbl.add descriptorTable"String" StringClass;
  Hashtbl.add descriptorTable "Int" IntegerClass;
  Hashtbl.add descriptorTable "Boolean" BooleanClass



let printClassDescriptor cd = match cd with
  | ClassDescriptor(cd) -> print_endline("*****Constructor of the class*****");Hashtbl.iter (fun key value -> print_endline(key)) cd.cdconstructors; (*print_const (" ") value)) cd.cdconstructors;*)print_endline("*****End of Constructors of the class*****"); print_endline("*****Method of the class*****");  Hashtbl.iter (fun key value -> print_endline(key ^":   " ^ value)) cd.cdmethods; print_endline("*****End of Method of the class*****");
							print_endline("*****attributes of the class*****"); List.iter (print_attribute ("  ")) cd.cdattributes;print_endline("*****end of attributes of the class*****") 
  | ObjectClass(cd) -> ()
  | StringClass -> ()
  | IntegerClass -> ()
  | BooleanClass -> ()


let type_from_object o = match o with 
  | ObjectDescriptor(objectDescriptor) -> objectDescriptor.otype
  | IntegerDescriptor(i) -> "Int"
  | StringDescriptor(s) -> "String"
  | NullObject -> "Null"
  | ThisObject -> "This"
  

let printClassDescriptorTable cdtable =
  print_endline("list of the classes");
  Hashtbl.iter (fun key value -> print_endline(key); printClassDescriptor(value)) cdtable

let printMethodTable mtable =
  print_endline("list of the methods of all classes");
  Hashtbl.iter (fun key value -> print_endline(key)) mtable



(* *)
let addMethodsToClassDesciptor cname methods methodTable m =
  let methodName = cname ^ "_" ^ m.mname in
  Hashtbl.add methods m.mname (methodName);
  Hashtbl.add methodTable methodName m

(*ajout des méthodes de la classe héritée, on ajout que les méthodes qui n'existe pas déjà dans la classe fille*)
let addMethodsFromParent cdmethods parentcd = match parentcd with
  | ClassDescriptor(cd) -> Hashtbl.iter (fun key value -> if(Hashtbl.mem cdmethods key) <> true then Hashtbl.add cdmethods key value) cd.cdmethods
  | ObjectClass(cd) -> Hashtbl.iter (fun key value -> Hashtbl.add cdmethods key value) cd.cdmethods
  

let rec get_list_of_arg args str = match args with
  | [] -> str
  | (arg::liste) -> match arg.ptype with
(*						| Array of t * int*)
  						| Primitive(Int) -> get_list_of_arg liste (str ^ "int")
						| Primitive(Boolean) -> get_list_of_arg liste (str ^ "boolean")
  						| Ref(ref_type) -> get_list_of_arg liste (str ^ ref_type.tid)

let rec get_list_arg_from_type exps str = match exps with
  | [] -> str 
  | (e::liste) -> match e.etype with
(*						| Array of t * int*)
  						| Some(Primitive(Int)) -> get_list_arg_from_type liste (str ^ "int")
						| Some(Primitive(Boolean)) -> get_list_arg_from_type liste (str ^ "boolean")
  						| Some(Ref(ref_type)) -> get_list_arg_from_type liste (str ^ ref_type.tid)


let add_constructors_to_class constructors const = 
	let listarg = get_list_of_arg const.cargstype "" in
	Hashtbl.add constructors (const.cname ^ "_" ^ listarg) const


  

let addAtributesFromParent cattributes parentcd = match parentcd with
  | ClassDescriptor(cd) -> cd.cdattributes @ cattributes
  | ObjectClass(cd) -> cd.cdattributes @ cattributes

let printParentAttributes parentcd = match parentcd with
  | ClassDescriptor(cd) -> List.iter (print_attribute (" parentattribute ")) cd.cdattributes
  | ObjectClass(cd) -> List.iter (print_attribute (" parentattribute ")) cd.cdattributes


let addMethodsAndAttributesToDescriptors classDescriptorTable methodTable c cname =
 print_endline("------------compilation of the class " ^ cname ^ " -----------------");
	  let methods = Hashtbl.create 20 in
      let parentClassDescriptor = Hashtbl.find classDescriptorTable c.cparent.tid in
      List.iter (addMethodsToClassDesciptor cname methods methodTable) c.cmethods;
	  addMethodsFromParent methods (parentClassDescriptor);
	  let cconsts = Hashtbl.create 10 in
		List.iter (add_constructors_to_class cconsts) c.cconsts; 
		if Hashtbl.mem cconsts (cname ^ "_") <> true then Hashtbl.add cconsts (cname ^ "_") ( {cmodifiers =[Public]; cname = cname; cargstype = []; cthrows =[]; cbody =[Nop] });
	  let attributes = addAtributesFromParent c.cattributes parentClassDescriptor in
	  Hashtbl.add classDescriptorTable cname (ClassDescriptor({ cdname = cname; cdmethods = methods; cdconstructors = cconsts; cdattributes = attributes }))


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


