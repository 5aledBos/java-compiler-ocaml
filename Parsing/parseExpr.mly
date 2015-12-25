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
  | e = expr SC                       { [e] }
  | e = expr SC rest = expressions    { e::rest }
(* For now, and expression file must end with a statement *)
  | s = statement EOF                 { [s] }
  | s = statement rest = expressions  { s::rest }

expr:
  | LPAR e = expr RPAR                { e }
  | e1 = expr op = binop e2 = expr    { Binop(e1, op, e2) }
  | l = literal                       { l }
  | u = unary                         { u }
  | ass = assignment                  { ass }

literal:
  | i = INT                           { Int i }
  | f = FLOAT                         { Float f }
  | b = BOOL                          { Bool b }
  | c = CHAR                          { Char c }
  | str = STRING                      { String str }
  | NULL                              { Null }
  | id = IDENT                        { Var id }

unary:
  | op = unop e = expr                { Unop(op, e) }
  | MINUS e = expr %prec UMINUS       { Unop(Uminus, e) }
  | PLUS e = expr %prec UPLUS         { Unop(Uplus, e) }

assignment:
  | e1 = expr ass = assign e2 = expr  { Assign(e1, ass, e2) }

statement:
  | IF LPAR e = expr RPAR b = block                   { If(e, b) }
  (* TODO: Add else if *)
  | IF LPAR e = expr RPAR b1 = block ELSE b2 = block  { Ifelse(e, b1, b2) }
  | WHILE LPAR e = expr RPAR b = block                { While(e, b) }
  | FOR f = forstat b = block                         { For(f, b) }

block:
  | LBRACE e = expressions RBRACE   { e }

forstat:
  | LPAR ass = assignment SC e1 = expr SC e2 = expr RPAR  { [ass; e1; e2] }

%inline binop:
  | PLUS      { Badd }
  | MINUS     { Bsub }
  | TIMES     { Bmul }
  | DIV       { Bdiv }
  | MOD       { Bmod }
  | AND       { Band }
  | OR        { Bor }
  | EQ        { Beq }
  | NEQ       { Bneq }
  | GT        { Bgt }
  | GE        { Bge }
  | LT        { Blt }
  | LE        { Ble }

%inline unop:
  | NOT       { Unot }
  | INCR      { Uincr }
  | DECR      { Udecr }
  | BITWISE   { Ubit }

%inline assign:
  | ASS       { Ass }
  | MULASS    { Assmul }
  | DIVASS    { Assdiv }
  | MODASS    { Assmod }
  | PLUSASS   { Assplus }
  | MINUSASS  { Assminus }

%%

