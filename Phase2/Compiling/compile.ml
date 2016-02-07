open AST

type objectDescriptor =
{
	name : string;
	attributes : astattribute list
}

type classDescriptor =
{
	name : string;
	methodDescriptor : astmethod list
}

let addMethod classDesc methode = match classDesc with
  | { name=str; methodDescriptor = liste} -> methode :: liste
  
(*compile/fonction read AST
add method, addattribute

*)

