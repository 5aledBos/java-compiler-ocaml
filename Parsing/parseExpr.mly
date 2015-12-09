%{

%}

%token EOF PLUS
%token <float> FLOAT

%start expression
%type <string> expression

%%

expression:
    | c = operation EOF { c }
    | c = nombre        { c }
operation:
    | d = nombre PLUS e = nombre { d^" + "^e }
    | d = operation PLUS e = nombre { d^" + "^e }
nombre:
    | a = FLOAT { string_of_float a }

%%

