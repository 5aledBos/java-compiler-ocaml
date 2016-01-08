%{
    open AstExpr
%}

/**********/
/* Tokens */
/**********/

/* Operators */
%token PLUS MINUS TIMES DIV MOD
%token LSHIFT SRSHIFT URSHIFT
%token AND OR NOT
%token GT GE LT LE
%token NULL
%token INCR DECR BITWISE
%token ASS MULASS DIVASS MODASS PLUSASS MINUSASS LSHIFTASS SRSHIFTASS URSHIFTASS AMPASS CIRCASS PIPEASS

/* Statements */
%token IF ELSE WHILE FOR SWITCH CASE
%token BREAK CONTINUE THROW SYNCHRONIZED TRY FINALLY

%token QUESTMARK COLON PIPE CIRCUMFLEX AMP COMA

/* Literal values */
%token <float> FLOAT
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token <string> CHAR

/********************************/
/* Priorities and associativity */
/********************************/

%left OR
%left AND
(*%left EQUAL NEQUAL
%left GT GE LT LE*)
%left PLUS MINUS
%left TIMES DIV MOD
(*%right UMINUS UPLUS NOT INCR DECR BITWISE*)

/******************************/
/* Entry points of the parser */
/******************************/

%start statements
%type <AstExpr.statement list> statements

%%

/*********/
/* Rules */
/*********/

statements:
  | s = statement                     { [s] }
  | s = statement rest = statements   { s::rest }

primary:
  | pna = primarynoarray              { pna }
  (*| ac = arraycreation               { ac }*)

primarynoarray:
  | l = literal                       { l }
  (*| t = typ POINT c = clas            {}
  | VOID POINT c = clas               {}
  | THIS                              {}
  | cn = classname POINT THIS         {}*)
  | LPAR e = expression RPAR          { e }
  | LPAR e = expression               { raise (Err(Illegal_bracket ')')) }
  (* TODO: | e = expression RPAR               { raise (Err(Illegal_bracket '(')) }*)
  (*| cie = classinstexpr               { cie }
  | fa = fieldaccess                  { fa }
  | mi = methodinvoc                  { mi }
  | aa = arrayaccess                  { aa }*)

(*fieldaccess:
  | p = primary POINT id = IDENT                  {}
  | SUPER POINT id = IDENT                        {}
  | cn = classname POINT SUPER POINT id = IDENT   {}

methodinvoc:
  | mn = methodname LPAR al = arglist? RPAR                                                 {}
  | p = primary POINT nwa = nonwildargs? id = IDENT LPAR al = arglist? RPAR                 {}
  | SUPER POINT nwa = nonwildargs? id = IDENT LPAR al = arglist? RPAR                       {}
  | cn = classname POINT SUPER POINT nwa = nonwildargs? id = IDENT LPAR al = arglist? RPAR  {}
  | tn = typename POINT nwa = nonwildargs id = IDENT LPAR al = arglist? RPAR                {}

(* methodname in parseClass *)

arglist:
  | e = expression                    { [e] }
  | al = arglist COMA e = expression  { al::e }*)

literal:
  | i = INT                           { Int i }
  | f = FLOAT                         { Float f }
  | b = BOOL                          { Bool b }
  | c = CHAR                          { Char c }
  | str = STRING                      { String str }
  | NULL                              { Null }

expression:
  | c = conditional     { c }
  | ass = assignment    { ass }

conditional:
  | co = condor         { co }
  (*| co = condor QUESTMARK e = expression COLON c = conditional*)

condor:
  | ca = condand                              { ca }
  | co = condor OR ca = condand               { Binop(co, Bor, ca) }

condand:
  | io = inclusiveor                          { io }
  | ca = condand AND io = inclusiveor         { Binop(ca, Band, io) }

inclusiveor:
  | eo = exclusiveor                          { eo }
  | io = inclusiveor PIPE eo = exclusiveor    { Binop(io, Bpipe, eo) }

exclusiveor:
  | ae = andexpr                              { ae }
  | eo = exclusiveor CIRCUMFLEX ae = andexpr  { Binop(eo, Bcirc, ae) }

andexpr:
  | ee = equalexpr                            { ee }
  | ae = andexpr AMP ee = equalexpr           { Binop(ae, Bamp, ee) }

equalexpr:
  | re = relationalexpr                         { re }
  | ee = equalexpr EQUAL re = relationalexpr    { Binop(ee, Beq, re) }
  | ee = equalexpr NEQUAL re = relationalexpr   { Binop(ee, Bneq, re) }

relationalexpr:
  | se = shiftexpr                                     { se }
  | re = relationalexpr op = binoprel se = shiftexpr   { Binop(re, op, se) }
  (*| re = relationalexpr INSTANCEOF rt = referencetype*)

shiftexpr:
  | ae = addexpr                                 { ae }
  | se = shiftexpr op = binopshift ae = addexpr  { Binop(se, op, ae) }

addexpr:
  | me = multexpr                      { me }
  | ae = addexpr PLUS me = multexpr    { Binop(ae, Badd, me) }
  | ae = addexpr MINUS me = multexpr   { Binop(ae, Bsub, me) }

multexpr:
  | ue = unary                               { ue }
  | me = multexpr op = binopmul ue = unary   { Binop(me, op, ue) }

unary:
  | op = unop u = unary   { Unopleft(op, u) }
  | u = unarynot          { u }

unarynot:
  | pe = postfix       { pe }
  | BITWISE u = unary  { Unopleft(Ubitwise, u) }
  | NOT u = unary      { Unopleft(Unot, u) }
  (*| ca = castexpr      { ca }*)

postfix:
  | p = primary        { p }
  | id = IDENT         { Var id }
  | p = postfix INCR   { Unopright(p, Urincr) }
  | p = postfix DECR   { Unopright(p, Urdecr) }

(*castexpr:
  | LPAR pt = primtype RPAR ue = unary
  | LPAR rt = reftype RPAR u = unarynot
  | LPAR PrimitiveType d = dims? RPAR ue = unary
  | LPAR rt = reftype RPAR u = unarynot*)

assignment:
  | l = leftside ass = assign e = expression  { Assign(l, ass, e) }

leftside:
  | id = IDENT                        { Var id }
  (*| fa = fieldaccess  { fa }
  | aa = arrayaccess  { aa }*)


(* BLOCKS AND STATEMENTS *)

block:
  | LBRACE bs = blockstatements RBRACE    { bs }
  | LBRACE bs = blockstatements           { raise (Err(Illegal_bracket '}')) }
  (* TODO: | bs = blockstatements RBRACE               { raise (Err(Illegal_bracket '{')) }*)

blockstatements:
  | bs = blockstatement                         { [bs] }
  | bs = blockstatement rest = blockstatements  { bs::rest }

blockstatement:
  (*| lv = localvariabledeclstat
  | cd = classdeclar*)
  | s = statement       { s }

(*localvariabledeclstat:
  | lv = localvariabledecl SC     { lv }

localvariabledecl:
  | vm = variablemodifiers t = typ vd = variabledecls

variablemodifiers:
  | vm = variablemodifier                         { [vm] }
  | vs = variablemodifiers vm = variablemodifier  { vs::vm }

variablemodifier:
  | (* TODO: Use the class parser? *)

variabledecls:
  | vd = variabledecl                             { [vd] }
  | vs = variabledecls COMA vd = variabledecl     { vs::vd }*)

statement:
  | s = statwithoutsubstat    { s }
  | ls = labeledstatement     { ls }
  | i = ifstatement           { i }
  | ie = ifelsestatement      { ie }
  | ws = whilestatement       { ws }
  (*| fs = forstatement         { fs }*)

statwithoutsubstat:
  | b = block                                         { Statements(b) }
  (*| es = emptystatement         { es }*)
  | es = exprstatement                                { Expression(es) }
  (*| ast = assertstatement       { ast }
  | ss = switchstatement        { ss }
  | ds = dostatement            { ds }*)
  | BREAK id = IDENT SC                               { Break(id) }
  (* TODO | BREAK SC *)
  | CONTINUE id = IDENT SC                            { Continue(id) }
  (* TODO | CONTINUE SC *)
  | RETURN e = expression SC                          { Return(e) }
  (* TODO: | RETURN SC*)
  | SYNCHRONIZED LPAR e = expression RPAR b = block   { Synchro(e, b) }
  | THROW e = expression SC                           { Throw(e) }
  (*| ts = trystatement                                 { ts }*)

exprstatements:
  | es = exprstatement                         { [es] }
  | es = exprstatement rest = exprstatements   { es::rest }

exprstatement:
  | ass = assignment SC       { ass }
  | INCR u = unary SC         { Unopleft(Ulincr, u) }
  | DECR u = unary SC         { Unopleft(Uldecr, u) }
  | p = postfix INCR SC       { Unopright(p, Urincr) }
  | p = postfix DECR SC       { Unopright(p, Urdecr) }
  (*| mi = methodinvoc SC       { mi }
  | cie = classinstexpr SC    { cie }*)

(*trystatement:
  | TRY b = block c = catches                      { Try(b, c) }
  | TRY b = block c = catches? FINALLY b = block   { Tryfin(b, c, b) }

catches:
  | cc = catchclause                 { [cc] }
  | c = catches cc = catchclause     { c::cc }

catchclause:
  | CATCH LPAR fp = formalparam RPAR b = block    {}

formalparam:
  | vm = variablemodifiers t = typ vdi = variabledeclid     {}

variabledeclid:
  | id = IDENT                        { Var id }
  | variabledeclid LBRACKET RBRACKET  {}*)

labeledstatement:
  | id = IDENT COLON s = statement                                    { Label(id, s) }

ifstatement:
  | IF LPAR e = expression RPAR s = statement                         { If(e, s) }

ifelsestatement:
  | IF LPAR e = expression RPAR s1 = statement ELSE s2 = statement    { Ifelse(e, s1, s2) }

whilestatement:
  | WHILE LPAR e = expression RPAR s = statement                      { While(e, s) }

(*forstatement:
  | bf = basicfor       { bf }
  | ef = enhancedfor    { ef }*)

(*basicfor:
  | FOR LPAR fi = forinit SC e = expression SC es = exprstatements RPAR s = statement   { For() }

forinit:
  | es = exprstatements         { es }
  | lv = localvariabledecl      { lv }*)

%inline binopmul:
  | TIMES     { Bmul }
  | DIV       { Bdiv }
  | MOD       { Bmod }

%inline binoprel:
  | GT        { Bgt }
  | GE        { Bge }
  | LT        { Blt }
  | LE        { Ble }

%inline binopshift:
  | LSHIFT    { Blshift }
  | SRSHIFT   { Bsrshift }
  | URSHIFT   { Burshift }

%inline unop:
  | INCR      { Ulincr }
  | DECR      { Uldecr }
  | PLUS      { Uplus }
  | MINUS     { Uminus }
  | BITWISE   { Ubitwise }
  | NOT       { Unot }

%inline assign:
  | ASS         { Ass }
  | MULASS      { Assmul }
  | DIVASS      { Assdiv }
  | MODASS      { Assmod }
  | PLUSASS     { Assplus }
  | MINUSASS    { Assminus }
  | LSHIFTASS   { Asslshift }
  | SRSHIFTASS  { Asssrshift }
  | URSHIFTASS  { Assurshift }
  | AMPASS      { Assamp }
  | CIRCASS     { Asscirc }
  | PIPEASS     { Asspipe }

%%

