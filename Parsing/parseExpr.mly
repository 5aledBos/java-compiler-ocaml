%{
    open Expr
%}

%token EOF PLUS MINUS TIMES DIV
%token <float> FLOAT
%token <string> IDENT

%start expression
%type <Expr.expression> expression

%left PLUS MINUS
%left TIMES DIV

%%

expression:
    | e = expr EOF                    { e }

expr:
    | e1 = expr o = PLUS e2 = expr    { Sum(e1, e2) }
    | e1 = expr o = MINUS e2 = expr   { Sub(e1, e2) }
    | e1 = expr o = TIMES e2 = expr   { Mul(e1, e2) }
    | e1 = expr o = DIV e2 = expr     { Div(e1, e2) }
    | f = FLOAT                       { Const f }
    | id = IDENT                      { Var id }

%%

