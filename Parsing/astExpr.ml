open AstUtil

(* AST *)

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Band | Bor | Bpipe | Bcirc | Bamp
  | Beq | Bneq | Bgt | Bge | Blt | Ble
  | Blshift | Bsrshift | Burshift

type unopleft =
  | Uminus | Uplus
  | Ulincr | Uldecr
  | Unot
  | Ubitwise

type unopright =
  | Urincr | Urdecr

type assign =
  | Ass
  | Assmul
  | Assdiv
  | Assmod
  | Assplus
  | Assminus
  | Asslshift
  | Asssrshift
  | Assurshift
  | Assamp
  | Asscirc
  | Asspipe

type expression =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of string
  | String of string
  | Null
  | CVoid
  | This of expression option
  | Var of string
  | Name of expression list
  | Binop of expression * binop * expression
  | Unopleft of unopleft * expression
  | Unopright of expression * unopright
  | Assign of expression * assign * expression
  | Fieldaccess of expression * string
  | Fieldaccesssuper of string
  | Fieldaccessclass of expression * string
  | Case of expression
  | Default
  | ArrayAccess of expression * expression
  | ArrayCreation of typ * expression option list
  | ArrayCreationInit of typ * (unit * unit) list * expression
  | ArrayInit of expression list
  | Ternary of expression * expression * expression
  | Instanceof of expression * typ
  | Cast of typ * expression
  | CastP of typ * int * expression
  | Method of expression * expression list
  | MethodP of expression * typ list option * expression * expression list
  | MethodS of typ list option * expression * expression list
  | MethodCS of expression * typ list option * expression * expression list
  | MethodT of expression * typ list * expression * expression list
  | VarDecl of expression * expression option

type statement =
  | Expression of expression
  | Expressions of expression list
  | Statements of statement list
  | EmptyStatement
  | Assert of expression
  | BAssert of expression * expression
  | Switch of expression * statement
  | SwitchBlock of statement list * expression list
  | SwitchGroup of expression list * statement list
  | Break of string option
  | Continue of string option
  | Return of expression option
  | Throw of expression
  | Synchro of expression * statement list
  | Try of statement list * statement list
  | Tryfin of statement list * statement list * statement list
  | CatchClause of expression * statement list
  | Label of string * statement
  | If of expression * statement * statement option
  | While of expression * statement
  | DoWhile of statement * expression
  | For of expression list option * expression option * expression list * statement
  | EFor of expression * expression * statement
  | LocalVarDecl of modifier list option * typ * expression list
  (*| EFor of modifier list option * type * expression * expression * statement *)


(* ERRORS *)

type error =
  | Illegal_bracket of char

exception Err of error

let report_err = function
      | Illegal_bracket c ->
    print_string "'";
	  print_char c;
	  print_string "' expected"


let var_of_string s = Var(s)

(* STRING_OF *)

let string_of_binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Band -> "&&"
  | Bor  -> "||"
  | Bpipe -> "|"
  | Bcirc -> "^"
  | Bamp -> "&"
  | Beq  -> "="
  | Bneq -> "!="
  | Bgt  -> ">"
  | Bge  -> ">="
  | Blt  -> "<"
  | Ble  -> "<="
  | Blshift -> "<<"
  | Bsrshift -> ">>"
  | Burshift -> ">>>"

let string_of_unopleft = function
  | Unot -> "not"
  | Uminus -> "-"
  | Uplus -> "+"
  | Ulincr -> "++"
  | Uldecr -> "--"
  | Ubitwise -> "~"

let string_of_unopright = function
  | Urincr -> "++"
  | Urdecr -> "--"

let string_of_assign = function
  | Ass -> "="
  | Assmul -> "*="
  | Assdiv -> "/="
  | Assmod -> "%="
  | Assplus -> "+="
  | Assminus -> "-="
  | Asslshift -> "<<="
  | Asssrshift -> ">>="
  | Assurshift -> ">>>="
  | Assamp -> "&="
  | Asscirc -> "^="
  | Asspipe -> "|="

let string_of_list f l =
  match l with
  | [] -> ""
  | li -> String.concat ", " (List.map f li)

let string_of_opt f o =
  match o with
  | None -> ""
  | Some(li) -> f li

let rec string_of_expr expr =
  match expr with
  (* Literals *)
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> "'" ^ c ^ "'"
  | String s -> "\"" ^ s ^ "\""
  | Null -> "null"
  | Var v -> v
  | CVoid -> "void.class"
  | This None -> "this"
  | This Some(e) -> (string_of_expr e) ^ ".this"

  (* Names *)
  | Name(l) -> (string_of_list string_of_expr l)

  (* Operations *)
  | Binop(e1, op, e2) -> (string_of_expr e1) ^ (string_of_binop op) ^ (string_of_expr e2)
  | Bool true -> "true"
  | Bool false -> "false"
  | Unopleft(op, e) -> (string_of_unopleft op) ^ (string_of_expr e)
  | Unopright(e, op) -> (string_of_expr e) ^ (string_of_unopright op)
  | Assign(e1, ass, e2) -> (string_of_expr e1) ^ (string_of_assign ass) ^ (string_of_expr e2)
  | Fieldaccess(e, str) -> (string_of_expr e) ^ "." ^ str
  | Fieldaccesssuper(str) -> "(super." ^ str ^ ")"
  | Fieldaccessclass(e, str) -> "(" ^ (string_of_expr e) ^ ".super." ^ str ^ ")"
  | Case(e) -> "(case: " ^ (string_of_expr e) ^ ")"
  | Default -> "default: "
  | ArrayAccess(e1, e2) -> "(" ^ (string_of_expr e1) ^ "[" ^ (string_of_expr e2) ^ "]" ^ ")"
  | ArrayCreation(t, e) -> "(new " ^ (string_of_type t) ^ (string_of_list (string_of_opt string_of_expr) e) ^ ")"
  | ArrayInit(l) -> (string_of_list string_of_expr l)
  | Ternary(c, e1, e2) -> (string_of_expr c) ^ " ? " ^ (string_of_expr e1) ^ " : " ^ (string_of_expr e2)
  | Instanceof(e, t) -> (string_of_expr e) ^ " instance of " ^ (string_of_type t)
  | Cast(t, e) -> "(" ^ (string_of_type t) ^ ") " ^ (string_of_expr e)
  | CastP(t, d, e) -> "(" ^ (string_of_type t) ^ " " ^ (string_of_int d) ^ ") " ^ (string_of_expr e)
  | Method(e1, e2) -> (string_of_expr e1) ^ "(" ^ (string_of_list string_of_expr e2) ^ ")"
  (* TODO: Remove the "METHODP at the beginning. Just here to know when it works" *)
  | MethodP(e1, t, e2, l) -> "METHODP" ^ (string_of_expr e1) ^ "." ^ (string_of_opt (string_of_list string_of_type) t) ^ (string_of_expr e2) ^ "(" ^ (string_of_list string_of_expr l) ^ ")"
  | MethodS(t, e2, l) -> "super" ^ "." ^ "<" ^ (string_of_opt (string_of_list string_of_type) t) ^ ">" ^ (string_of_expr e2) ^ "(" ^ (string_of_list string_of_expr l) ^ ")"
  | VarDecl(e1, None) -> "(" ^ (string_of_expr e1) ^ ")"
  | VarDecl(e1, Some(e2)) -> "(" ^ (string_of_expr e1) ^ "=" ^ (string_of_expr e2) ^ ")"

let rec string_of_statement stat =
  match stat with
  | Expression e -> "(" ^ string_of_expr e ^ ")"
  | Expressions e -> string_of_list string_of_expr e
  | Statements s -> string_of_list string_of_statement s
  | EmptyStatement -> "Empty statement"
  | Assert(e) -> "(assert " ^ (string_of_expr e) ^ ")"
  | BAssert(e1, e2) -> "(assert " ^ (string_of_expr e1) ^ ":" ^ (string_of_expr e2) ^ ")"
  | Switch(e, s) -> "(switch (" ^ (string_of_expr e) ^ ") " ^ (string_of_statement s) ^ ")"
  | SwitchBlock(s, e) -> "{" ^ (string_of_list string_of_statement s) ^ (string_of_list string_of_expr e) ^ "}"
  | SwitchGroup(e, s) -> (string_of_list string_of_expr e) ^ (string_of_list string_of_statement s)
  | Try(b, c) -> "try {" ^ (string_of_list string_of_statement b) ^ "}" ^ (string_of_list string_of_statement c)
  | Tryfin(b, c, f) -> "try {" ^ (string_of_list string_of_statement b) ^ "}" ^ (string_of_list string_of_statement c) ^ "finally {" ^ (string_of_list string_of_statement f) ^ "}"
  | CatchClause(e, s) -> "catch ("^ (string_of_expr e) ^") {" ^ (string_of_list string_of_statement s) ^ "}"
  | Break(Some(v)) -> "(break " ^ v ^ ")"
  | Break(None) -> "(break)"
  | Continue(Some(v)) -> "(continue " ^ v ^ ")"
  | Continue(None) -> "(continue)"
  | Return(Some(e)) -> "(return " ^ (string_of_expr e) ^ ")"
  | Return(None) -> "(return)"
  | Throw(e) -> "(throw " ^ (string_of_expr e) ^ ")"
  | Synchro(e, s) -> "(synchronized (" ^ (string_of_expr e) ^ ") {" ^ (string_of_list string_of_statement s) ^ "}"
  | Label(v, s) -> v ^ " : " ^ (string_of_statement s)
  | If(e, s, es) -> "if(" ^ (string_of_expr e) ^ ") {" ^ (string_of_statement s) ^ " else {" ^ (string_of_opt string_of_statement es) ^ "}"
  | While(e, s) -> "while(" ^ (string_of_expr e) ^ ") {" ^ (string_of_statement s) ^ "}"
  | DoWhile(s, e) -> "do {" ^ (string_of_statement s) ^ "} while(" ^ (string_of_expr e) ^ ")"
  | For(f, e, es, s) -> "for(" ^ (string_of_opt (string_of_list string_of_expr) f) ^ ";" ^ (string_of_opt string_of_expr e) ^ ";" ^ (string_of_list string_of_expr es) ^ ") {" ^ (string_of_statement s) ^ "}"
  | EFor(id, e, s) -> "for(" ^ (string_of_expr id) ^ " : " ^ (string_of_expr e) ^ ") { " ^ (string_of_statement s) ^ " }"
  (*| EFOR(vm, t, id, e, s) -> "for("(* Print variable modifiers and type *) ^ (string_of_expr id) ^ " : " ^ (string_of_expr e) ^ ") { " ^ (string_of_statement s) ^ " }"*)
  | LocalVarDecl(ml, t, el) -> "(" ^ (string_of_opt (string_of_list string_of_modifier) ml) ^ " " ^ (string_of_type t) ^ " " ^ (string_of_list string_of_expr el) ^ ")"

