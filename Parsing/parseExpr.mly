%{
    open Expr
%}

/**********/
/* Tokens */
/**********/

/* Operators */
%token PLUS MINUS TIMES DIV MOD
%token AND OR NOT
%token GT GE LT LE EQ NEQ
%token NULL
%token INCR DECR BITWISE
%token ASS MULASS DIVASS MODASS PLUSASS MINUSASS

/* Statements */
%token IF ELSE WHILE FOR SWITCH CASE

%token QUESTMARK COLUMN PIPE CIRCUMFLEX AMP

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
%left EQ NEQ
%left GT GE LT LE
%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS UPLUS NOT INCR DECR BITWISE

/******************************/
/* Entry points of the parser */
/******************************/

%start expressions
%type <Expr.expression list> expressions

%%

/*********/
/* Rules */
/*********/

expressions:
  | p = primary SC EOF                 { [p] }
  | p = primary SC rest = expressions  { p::rest }
  (*| s = statement EOF                 { [s] }
  | s = statement rest = expressions  { s::rest }*)

primary:
  | l = literal                       { l }
  | LPAR e = expression RPAR          { e }

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
  (*| co = condor QUESTMARK e = expression COLUMN c = conditional*)

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
  | ae = addexpr    { ae }
  (*| se = shiftexpr "<<" ae = addexpr
  | se = shiftexpr ">>" ae = addexpr
  | se = shiftexpr ">>>" ae = addexpr*)

addexpr:
  | me = multexpr                      { me }
  | ae = addexpr PLUS me = multexpr    { Binop(ae, Badd, me) }
  | ae = addexpr MINUS me = multexpr   { Binop(ae, Bsub, me) }

multexpr:
  | ue = unary                               { ue }
  | me = multexpr op = binopmul ue = unary   { Binop(me, op, ue) }

unary:
  | op = unop u = unary   { Unop(op, u) }
  | u = unarynot      { u }

unarynot:
  | pe = postfix      { pe }
  (*| BITWISE u = unary
  | NOT u = unary
  | ca = castexpr*)

postfix:
  | p = primary      { p }
  | id = IDENT       { Var id }
  (*| p = postfix INCR 
  | p = postfix DECR*)

assignment:
  | l = leftside ass = assign e = expression  { Assign(l, ass, e) }

leftside:
  | id = IDENT                        { Var id }
  (*| fa = fieldaccess  { fa }
  | aa = arrayaccess  { aa }*)

(*statement:
  | IF LPAR e = expr RPAR b = block                   { If(e, b) }
  (* TODO: Add else if *)
  | IF LPAR e = expr RPAR b1 = block ELSE b2 = block  { Ifelse(e, b1, b2) }
  | WHILE LPAR e = expr RPAR b = block                { While(e, b) }
  | FOR f = forstat b = block                         { For(f, b) }

block:
  | LBRACE e = expressions RBRACE   { e }

forstat:
  | LPAR ass = assignment SC e1 = expr SC e2 = expr RPAR  { [ass; e1; e2] }*)

%inline binopmul:
  | PLUS      { Badd }
  | DIV       { Bdiv }
  | MOD       { Bmod }

%inline binoprel:
  | GT        { Bgt }
  | GE        { Bge }
  | LT        { Blt }
  | LE        { Ble }

%inline unop:
  | INCR      { Uincr }
  | DECR      { Udecr }
  | PLUS      { Uplus }
  | MINUS     { Uminus }

%inline assign:
  | ASS       { Ass }
  | MULASS    { Assmul }
  | DIVASS    { Assdiv }
  | MODASS    { Assmod }
  | PLUSASS   { Assplus }
  | MINUSASS  { Assminus }
  (* TODO: Add <<= >>= >>>= &= ^= and|= *)

%%

