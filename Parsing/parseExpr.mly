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
%token IF ELSE WHILE DO FOR SWITCH CASE DEFAULT ASSERT
%token BREAK CONTINUE THROW SYNCHRONIZED TRY CATCH FINALLY

%token NEW INSTANCEOF
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

(*%left OR
%left AND
%left EQUAL NEQUAL
%left GT GE LT LE
%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS UPLUS NOT INCR DECR BITWISE*)

/******************************/
/* Entry points of the parser */
/******************************/

%start statements
%type <AstExpr.statement list> statements

%start expression
%type <AstExpr.expression> expression

%start block
%type <AstExpr.statement list> block

%%

/*********/
/* Rules */
/*********/

statements:
  | s = nonempty_list(statement)    { s }


(* EXPRESSIONS *)

expression:
  | c = conditionalExpression  { c }
  | ass = assignment           { ass }
  | p = primary                { p }
  | error { raise Illegal_expression }

primary:
  | pna = primaryNoNewArray          { pna }
  | ac = arrayCreationExpression     { ac }

primaryNoNewArray:
  | l = literal                              { l }
  (*| t = typ POINT CLASS                      { TClass(t) }*)
  | VOID POINT CLASS                         { CVoid }
  | THIS                                     { This(None) }
  | id = IDENT POINT THIS                    { This(Some(Var id)) }
  | LPAR e = expression RPAR                 { e }
  | cie = classInstanceCreationExpression    { cie }
  | fa = fieldAccess                         { fa }
  | mi = methodInvocation                    { mi }
  | aa = arrayAccess                         { aa }

literal:
  | i = INT                           { Int i }
  | f = FLOAT                         { Float f }
  | b = BOOL                          { Bool b }
  | c = CHAR                          { Char c }
  | str = STRING                      { String str }
  | NULL                              { Null }

classInstanceCreationExpression:
  | NEW ta = typeArguments? cit = classOrInterfaceType LPAR al = argumentList RPAR                                           { ClassInstCrea(ta, cit, al) }
  (*| p = primary POINT NEW tp = typeArguments? id = IDENT ta = typeArguments? LPAR al = argumentList RPAR cb = classBody?   { ClassInstCreaP(p, tp, Var id, ta, al, cb) }*)

fieldAccess:
  | p = primary POINT id = IDENT                  { Fieldaccess(p, id) }
  | SUPER POINT id = IDENT                        { Fieldaccesssuper(id) }
  (*| id = IDENT POINT SUPER POINT id = IDENT       { Fieldaccessclass(id, id) }*)

methodInvocation:
  | mn = pathName LPAR al = argumentList RPAR        { Method(mn, al) }
  (*| p = primary POINT nwa = nonWildTypeArguments? id = IDENT LPAR al = argumentList RPAR                 { MethodP(p, nwa, Var id, al) }
  | SUPER POINT nwa = nonWildTypeArguments? id = IDENT LPAR al = argumentList RPAR                       { MethodS(nwa, Var id, al) }
  | id = IDENT POINT SUPER POINT nwa = nonWildTypeArguments? id = IDENT LPAR al = argumentList RPAR  { MethodCS(cn, nwa, Var id, al) }
  | tn = typeName POINT nwa = nonWildTypeArguments id = IDENT LPAR al = argumentList RPAR               { MethodT(tn, nwa, Var id, al) }*)

nonWildTypeArguments:
  | LT l = nonempty_list(referenceType) GT   { l }

%public
argumentList:
  | l = separated_list(COMA, expression)   { l }

arrayAccess:
  | en = pathName LBRACKET e = expression RBRACKET            { ArrayAccess(en, e) }
  | pna = primaryNoNewArray LBRACKET e = expression RBRACKET  { ArrayAccess(pna, e) }

arrayCreationExpression:
  | NEW pt = primitiveType de = nonempty_list(delimited(LBRACKET, expression?, RBRACKET))                    { ArrayCreation(pt, de) }
  (*| NEW coi = classOrInterfaceType de = dimExprs d = dims?         { ArrayCreation(coi, de, d) }*)
  | NEW pt = primitiveType d = nonempty_list(pair(LBRACKET, RBRACKET)) ai = arrayInitializer          { ArrayCreationInit(pt, List.length(d), ai) }
  (*| NEW coi = classOrInterfaceType d = dims ai = arrayInitializer  { ArrayCreation(coi, de, ai) }*)

arrayInitializer:
  | LBRACE vi = separated_list(COMA, variableInitializer) (*COMA?*) RBRACE    { ArrayInit(vi) }

%public
variableInitializer:
  | e = expression         { e }
  | ai = arrayInitializer  { ai }


conditionalExpression:
  | co = conditionalOrExpression         { co }
  | co = conditionalOrExpression QUESTMARK e = expression COLON c = conditionalExpression  { Ternary(co, e, c) }

conditionalOrExpression:
  | ca = conditionalAndExpression                                  { ca }
  | co = conditionalOrExpression OR ca = conditionalAndExpression  { Binop(co, Bor, ca) }

conditionalAndExpression:
  | io = inclusiveOrExpression                                     { io }
  | ca = conditionalAndExpression AND io = inclusiveOrExpression   { Binop(ca, Band, io) }

inclusiveOrExpression:
  | eo = exclusiveOrExpression                                    { eo }
  | io = inclusiveOrExpression PIPE eo = exclusiveOrExpression    { Binop(io, Bpipe, eo) }

exclusiveOrExpression:
  | ae = andExpression                                        { ae }
  | eo = exclusiveOrExpression CIRCUMFLEX ae = andExpression  { Binop(eo, Bcirc, ae) }

andExpression:
  | ee = equalityExpression                            { ee }
  | ae = andExpression AMP ee = equalityExpression     { Binop(ae, Bamp, ee) }

equalityExpression:
  | re = relationalExpression                                  { re }
  | ee = equalityExpression EQUAL re = relationalExpression    { Binop(ee, Beq, re) }
  | ee = equalityExpression NEQUAL re = relationalExpression   { Binop(ee, Bneq, re) }

relationalExpression:
  | se = shiftExpression                                           { se }
  | re = relationalExpression op = binoprel se = shiftExpression   { Binop(re, op, se) }
  | re = relationalExpression INSTANCEOF rt = referenceType        { Instanceof(re, rt) }

shiftExpression:
  | ae = additiveExpression                                       { ae }
  | se = shiftExpression op = binopshift ae = additiveExpression  { Binop(se, op, ae) }

additiveExpression:
  | me = multiplicativeExpression                                 { me }
  | ae = additiveExpression PLUS me = multiplicativeExpression    { Binop(ae, Badd, me) }
  | ae = additiveExpression MINUS me = multiplicativeExpression   { Binop(ae, Bsub, me) }

multiplicativeExpression:
  | ue = unaryExpression                                               { ue }
  | me = multiplicativeExpression op = binopmul ue = unaryExpression   { Binop(me, op, ue) }

unaryExpression:
  | op = unop u = unaryExpression      { Unopleft(op, u) }
  | u = unaryExpressionNotPlusMinus    { u }

unaryExpressionNotPlusMinus:
  | pe = postfixExpression       { pe }
  | BITWISE u = unaryExpression  { Unopleft(Ubitwise, u) }
  | NOT u = unaryExpression      { Unopleft(Unot, u) }
  | ca = castExpression          { ca }

postfixExpression:
  | p = primary                  { p }
  | en = pathName                { en }
  | p = postfixExpression INCR   { Unopright(p, Urincr) }
  | p = postfixExpression DECR   { Unopright(p, Urdecr) }

castExpression:
  | LPAR pt = primitiveType l = list(pair(LBRACKET, RBRACKET)) RPAR ue = unaryExpression    { CastP(pt, List.length(l), ue) }
  (*| LPAR rt = referenceType RPAR u = unaryExpressionNotPlusMinus                            { Cast(rt, u) }*)

assignment:
  | l = leftHandSide ass = assign e = expression  { Assign(l, ass, e) }

leftHandSide:
  | en = pathName                { en }
  | fa = fieldAccess             { fa }
  | aa = arrayAccess             { aa }


(* BLOCKS AND STATEMENTS *)

block:
  | LBRACE bs = list(blockStatement) RBRACE    { bs }

blockStatement:
  | vm = variableModifiers? t = typ vd = variableDeclarators SC     { LocalVarDeclS(vm, t, vd) }
  (*| cd = classDeclaration                                           { cd }*)
  | s = statement                                                   { s }

%public
variableDeclarators:
  | vd = separated_nonempty_list(COMA, variableDeclarator)      { vd }

variableDeclarator:
  | vdi = variableDeclaratorId vc = variableDeclaratorCompl?    { VarDecl(vdi, vc) }

variableDeclaratorCompl:
  | ASS vi = variableInitializer     { vi }

%public
variableDeclaratorId:
  | id = IDENT                                     { Var id }
  (*| vdi = variableDeclaratorId LBRACKET RBRACKET   {}*)

statement:
  | s = statementWithoutTrailingSubstatement                          { s }
  | id = IDENT COLON s = statement                                    { Label(id, s) }
  | IF LPAR e = expression RPAR s = statement es = elseStatement?     { If(e, s, es) }
  | WHILE LPAR e = expression RPAR s = statement                      { While(e, s) }
  | fs = forStatement                                                 { fs }

statementWithoutTrailingSubstatement:
  | b = block                                             { Statements(b) }
  | SC                                                    { EmptyStatement }
  | se = statementExpression SC                           { Expression(se) }
  | ast = assertStatement                                 { ast }
  (*| SWITCH LPAR id = IDENT RPAR sb = switchBlock          { Switch(Var id, sb) }*)
  | DO s = statement WHILE LPAR e = expression RPAR SC    { DoWhile(s, e) }
  | BREAK id = IDENT? SC                                  { Break(id) }
  | CONTINUE id = IDENT? SC                               { Continue(id) }
  | RETURN e = expression? SC                             { Return(e) }
  | SYNCHRONIZED LPAR e = expression RPAR b = block       { Synchro(e, b) }
  | THROW e = expression SC                               { Throw(e) }
  | ts = tryStatement                                     { ts }

statementExpression:
  | ass = assignment                       { ass }
  | INCR u = unaryExpression               { Unopleft(Ulincr, u) }
  | DECR u = unaryExpression               { Unopleft(Uldecr, u) }
  | p = postfixExpression INCR             { Unopright(p, Urincr) }
  | p = postfixExpression DECR             { Unopright(p, Urdecr) }
  | mi = methodInvocation                  { mi }
  | cie = classInstanceCreationExpression  { cie }

assertStatement:
  | ASSERT es = statementExpression SC                                 { Assert(es) }
  | ASSERT e1 = statementExpression COLON e2 = statementExpression SC  { BAssert(e1, e2) }

(*switchBlock:
  | LBRACE sbg = list(switchBlockStatementGroup) sl = list(switchLabel) RBRACE   { SwitchBlock(sbg, sl) }

switchBlockStatementGroup:
  | l = nonempty_list(switchLabel) bs = list(blockStatement)           { SwitchGroup(l, bs) }

switchLabel:
  | CASE c = expression COLON                                  { Case(c) }
  | CASE e = enumConstantName COLON                            { Case(e) }
  | DEFAULT COLON                                              { Default }

enumConstantName:
  | id = IDENT                                                 { Var id }*)

tryStatement:
  | TRY b = block c = nonempty_list(catchClause)                { Try(b, c) }
  | TRY b = block c = list(catchClause) FINALLY f = block       { Tryfin(b, c, f) }

(*TODO: use formalParameter from parseClass*)
catchClause:
  | CATCH LPAR fp = formalParam RPAR b = block                  { CatchClause(fp, b) }

formalParam:
  | vdi = IDENT                                                  { Var vdi }
  (*| vm = variableModifiers t = typ vdi = variableDeclaratorId     {}*)

elseStatement:
  | ELSE s = statement                                                { s }

forStatement:
  | FOR LPAR fi = forInit? SC e = expression? SC es = separated_list(COMA, statementExpression) RPAR s = statement   { For(fi, e, es, s) }
  | FOR LPAR vm = variableModifiers? t = typ id = IDENT COLON e = expression RPAR s = statement  { EFor(vm, t, Var id, e, s) }

forInit:
  | es = separated_nonempty_list(COMA, statementExpression)    { es }
  | vm = variableModifiers? t = typ vd = variableDeclarators   { [LocalVarDecl(vm, t, vd)] }


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
