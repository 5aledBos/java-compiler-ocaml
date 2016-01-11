%{
    open AstExpr
    open AstUtil
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

%start expression
%type <AstExpr.expression> expression

%start block
%type <AstExpr.statement list> block

%%

/*********/
/* Rules */
/*********/

statements:
  | s = statement                     { [s] }
  | s = statement rest = statements   { s::rest }


(* NAMES *)

(*TODO: check in the Java spec*)
className:
  | id = IDENT                                { Var id }


(* EXPRESSIONS *)

primary:
  | pna = primaryNoNewArray          { pna }
  | ac = arrayCreationExpression     { ac }

primaryNoNewArray:
  | l = literal                              { l }
  (*| t = typ POINT c = clas                   {}*)
  | VOID POINT CLASS                         { CVoid }
  | THIS                                     { This(None) }
  | id = IDENT POINT THIS                { This(Some(Var id)) }
  | LPAR e = expression RPAR                 { e }
  (*| cie = classInstanceCreationExpression    { cie }*)
  | fa = fieldAccess                         { fa }
  (*| mi = methodInvocation                   { mi }*)
  | aa = arrayAccess                         { aa }

literal:
  | i = INT                           { Int i }
  | f = FLOAT                         { Float f }
  | b = BOOL                          { Bool b }
  | c = CHAR                          { Char c }
  | str = STRING                      { String str }
  | NULL                              { Null }

assignmentExpression:
  (*| c = conditionalExpression  { c }*)
  | ass = assignment           { ass }
  | p = primary                { p }

(*classInstanceCreationExpression:
  | NEW ta = typeArguments? cit = classOrInterfaceType LPAR al = argumentList? RPAR   {}
  | p = primary POINT NEW tp = typeArguments? id = IDENT ta = typeArguments? LPAR al = argumentList? RPAR cb = classBody?   {}*)

fieldAccess:
  | p = primary POINT id = IDENT                  { Fieldaccess(p, id) }
  | SUPER POINT id = IDENT                        { Fieldaccesssuper(id) }
  (*| cn = className POINT SUPER POINT id = IDENT   { Fieldaccessclass(cn, id) }*)

methodInvocation:
  | mn = pathName LPAR al = argumentList? RPAR                                                          { Method(mn, al) }
  (*| p = primary POINT nwa = nonWildTypeArguments? id = IDENT LPAR al = argumentList? RPAR                 { MethodP(p, nwa, Var id, al) }*)
  | SUPER POINT nwa = nonWildTypeArguments? id = IDENT LPAR al = argumentList? RPAR                       { MethodS(nwa, Var id, al) }
  (*| cn = className POINT SUPER POINT nwa = nonWildTypeArguments? id = IDENT LPAR al = argumentList? RPAR  { MethodCS(cn, nwa, Var id, al) }
  | tn = typeName POINT nwa = nonWildTypeArguments id = IDENT LPAR al = argumentList? RPAR               { MethodT(tn, nwa, Var id, al) }*)

nonWildTypeArguments:
  | LT l = referenceTypeList GT             { l }

referenceTypeList:
  | t = referenceType                       { [t] }
  | t = referenceType l = referenceTypeList { t::l }

%public
argumentList:
  | e = expression                         { [e] }
  | e = expression COMA al = argumentList  { e::al }

arrayAccess:
  | en = pathName LBRACKET e = expression RBRACKET      { ArrayAccess(en, e) }
  | pna = primaryNoNewArray LBRACKET e = expression RBRACKET  { ArrayAccess(pna, e) }

arrayCreationExpression:
  | NEW pt = primitiveType de = nonempty_list(delimited(LBRACKET,expression?,RBRACKET))                    { ArrayCreation(pt, de) }
  (*| NEW coi = classOrInterfaceType de = dimExprs d = dims?         { ArrayCreation(coi, de, d) }
  | NEW pt = primitiveType d = dims ai = arrayInitializer          { ArrayCreationInit(pt, d, ai) }
  | NEW coi = classOrInterfaceType d = dims ai = arrayInitializer  { ArrayCreation(coi, de, ai) }*)

arrayInitializer:
  | LBRACE vi = variableInitializers? c = COMA? RBRACE    { ArrayInit(vi, c) }

variableInitializers:
  | vi = variableInitializer                                  { [vi] }
  | vs = variableInitializers COMA vi = variableInitializer   { vi::vs }

%public
variableInitializer:
  | e = expression         { e }
  | ai = arrayInitializer  { ai }

(*conditionalExpression:
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
  (*| ca = castExpression          { ca }*)

postfixExpression:
  | p = primary                  { p }
  | en = pathName          { en }
  | p = postfixExpression INCR   { Unopright(p, Urincr) }
  | p = postfixExpression DECR   { Unopright(p, Urdecr) }*)

(*castExpression:
  | LPAR pt = primitiveType RPAR ue = unaryExpression              { Cast(pt, ue) }
  | LPAR rt = referenceType RPAR u = unaryExpressionNotPlusMinus   { Cast(rt, u) }
  | LPAR pt = primitiveType d = dims? RPAR ue = unaryExpression    { Cast(pt, ue) }
  | LPAR rt = referenceType RPAR u = unaryExpressionNotPlusMinus   { Cast(rt, u) }*)

assignment:
  | l = leftHandSide ass = assign e = assignmentExpression  { Assign(l, ass, e) }

leftHandSide:
  | en = pathName          { en }
  (*| fa = fieldAccess             { fa }
  | aa = arrayAccess             { aa }*)

expression:
  | ae = assignmentExpression    { ae }

constantExpression:
  | e = expression               { e }


(* BLOCKS AND STATEMENTS *)

(* Was forced to use EmptyBlock instead of just blockStatements? because for some reason I couldn't make it work. *)
block:
  | LBRACE RBRACE                         { [EmptyBlock] }
  | LBRACE bs = blockStatements RBRACE    { bs }

blockStatements:
  | bs = blockStatement                         { [bs] }
  | bs = blockStatement rest = blockStatements  { bs::rest }

blockStatement:
  (*| lv = localvariabledeclstat
  | cd = classdeclar*)
  | s = statement       { s }

(*localVariableDeclarationStatement:
  | lv = localVariableDeclaration SC     { lv }

localVariableDeclaration:
  | vm = variableModifiers t = typ vd = variableDeclarators

variableModifiers:
  | vm = variableModifier                         { [vm] }
  | vs = variablemMdifiers vm = variableModifier  { vs::vm }

variableModifier:
  | (* TODO: Use the class parser? *)

variableDeclarators:
  | vd = variableDeclarator                                 { [vd] }
  | vs = variableDeclarators COMA vd = variableDeclarator   { vs::vd }

variableDeclarator:
  | vdi = variableDeclaratorId
  | vdi = variableDeclaratorId ASS vi = variableInitializer

variableDeclaratorId:
  | id = IDENT
  | vdi = variableDeclaratorId LBRACKET RBRACKET

variableInitializer:
  | e = expression
  | ai = arrayInitializer*)

statement:
  | s = statementWithoutTrailingSubstatement { s }
  (*| ls = labeledStatement                    { ls }*)
  | i = ifThenStatement                      { i }
  (*| ie = ifThenElseStatement                 { ie }
  | ws = whileStatement                      { ws }
  | fs = forStatement                        { fs }*)

statementWithoutTrailingSubstatement:
  (*| b = block                                         { Statements(b) }
  | SC                                                { EmptyStatement }*)
  | es = expressionStatement                          { Expression(es) }
  (*| ast = assertStatement                             { ast }
  | ss = switchStatement                              { ss }
  | ds = doStatement                                  { ds }
  | BREAK id = IDENT? SC                              { Break(id) }
  | CONTINUE id = IDENT? SC                           { Continue(id) }
  | RETURN e = expression? SC                          { Return(e) }
  | SYNCHRONIZED LPAR e = expression RPAR b = block   { Synchro(e, b) }
  | THROW e = expression SC                           { Throw(e) }
  | ts = tryStatement                                 { ts }

expressionStatements:
  | es = expressionStatement                         { [es] }
  | es = expressionStatement rest = expressionStatements   { es::rest }*)

expressionStatement:
  | se = statementExpression SC    { se }

statementExpression:
  | ass = assignment                       { ass }
  (*| INCR u = unaryExpression               { Unopleft(Ulincr, u) }
  | DECR u = unaryExpression               { Unopleft(Uldecr, u) }
  | p = postfixExpression INCR             { Unopright(p, Urincr) }
  | p = postfixExpression DECR             { Unopright(p, Urdecr) }
  | mi = methodInvocation                  { mi }
  | cie = classInstanceCreationExpression  { cie }*)

(*assertStatement:
  | ASSERT es = statementExpression SC                                 { Assert(es) }
  | ASSERT e1 = statementExpression COLON e2 = statementExpression SC  { BAssert(e1, e2) }

switchStatement:
  | SWITCH LPAR id = IDENT RPAR sb = switchBlock                       { Switch(Var id, sb) }

switchBlock:
  | LBRACE sbg = switchBlockStatementGroups? sl = switchLabels? RBRACE   { SwitchBlock(sbg, sl) }

switchBlockStatementGroups:
  | sbsg = switchBlockStatementGroup                                   { [sbsg] }
  | s = switchBlockStatementGroup g = switchBlockStatementGroups       { s::g }

switchBlockStatementGroup:
  | l = switchLabels b = blockStatements                               { SwitchGroup(l, b) }

switchLabels:
  | s = switchLabel                                                    { [s] }
  | s = switchLabel  sls = switchLabels                                { s::sls }

switchLabel:
  | CASE c = constantExpression COLON                                  { Case(c) }
  | CASE e = enumConstantName COLON                                    { Case(e) }
  | DEFAULT COLON                                                      { Default }

enumConstantName:
  | id = IDENT                                                         { Var id }

tryStatement:
  | TRY b = block c = catches                                          { Try(b, c) }
  | TRY b = block c = catches FINALLY f = block                        { Tryfin(b, Some(c), f) }
  | TRY b = block FINALLY f = block                                    { Tryfin(b, None, f) }
  (* Could be the following, but give the error: Error: do not know how to resolve a reduce/reduce conflict
  | TRY b = block c = catches? FINALLY f = block                        { Tryfin(b, c, f) }*)

catches:
  | cc = catchClause                                                   { [cc] }
  | cc = catchClause c = catches                                       { cc::c }

(*TODO: use formalParameter from parseClass*)
catchClause:
  | CATCH LPAR fp = formalParam RPAR b = block                 { CatchClause(fp, b) }

formalParam:
  | vdi = IDENT                                                        { Var vdi }
  (*| vm = variableModifiers t = typ vdi = variableDeclaratorId     {}*)

labeledStatement:
  | id = IDENT COLON s = statement                                     { Label(id, s) }*)

ifThenStatement:
  | IF LPAR e = expression RPAR s = statement                          { If(e, s) }

(*ifThenElseStatement:
  | IF LPAR e = expression RPAR s1 = statement ELSE s2 = statement     { Ifelse(e, s1, s2) }
  (* TODO: replace previous by
  | IF LPAR e = expression RPAR s1 = statementNoShortIf ELSE s2 = statement    { Ifelse(e, s1, s2) }*)

(*statementNoShortIf:
  | StatementWithoutTrailingSubstatement
  | LabeledStatementNoShortIf
  | IfThenElseStatementNoShortIf
  | WhileStatementNoShortIf
  | ForStatementNoShortIf*)

whileStatement:
  | WHILE LPAR e = expression RPAR s = statement                      { While(e, s) }

doStatement:
  | DO s = statement WHILE LPAR e = expression RPAR SC                { DoWhile(s, e) }

forStatement:
  | bf = basicForStatement       { bf }
  | ef = enhancedForStatement    { ef }

basicForStatement:
  | FOR LPAR fi = forInit? SC e = expression? SC es = statementExpressionList? RPAR s = statement   { For(fi, e, es, s) }

(* TODO: Change this to accept coma seperated declaration *)
forInit:
  | es = statementExpressionList                             { es }
  (*| lv = localvariabledecl                                   { lv }*)

statementExpressionList:
  | e = statementExpression                                  { [e] }
  | e = statementExpression COMA l = statementExpressionList { e::l }

(* TODO: add type *)
enhancedForStatement:
  | FOR LPAR id = IDENT COLON e = expression RPAR s = statement  { EFor(Var id, e, s) }
  (*| FOR LPAR vm = variableModifiers? t = typ id = IDENT COLON e = expression RPAR s = statement  { EFor(vm, t, Var id, e, s) }*)*)


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
