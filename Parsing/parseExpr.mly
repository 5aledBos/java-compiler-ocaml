%{
    open Expr
%}

/**********/
/* Tokens */
/**********/

/* Separators */
%token EOF

/* Operators */
%token PLUS MINUS TIMES DIV MOD

/* Literal values */
%token <float> FLOAT
%token <string> IDENT

/********************************/
/* Priorities and associativity */
/********************************/

%left PLUS MINUS
%left TIMES DIV

/******************************/
/* Entry points of the parser */
/******************************/

%start expression
%type <Expr.expression> expression

%%

/*********/
/* Rules */
/*********/

expression:
    | e = expr EOF                      { e }

expr:
    | e1 = expr op = binop e2 = expr    { Binop(op, e1, e2) }
    | f = FLOAT                         { Const f }
    | id = IDENT                        { Var id }

%inline binop:
    | PLUS      { Badd }
    | MINUS     { Bsub }
    | TIMES     { Bmul }
    | DIV       { Bdiv }
    | MOD       { Bmod }

%%

