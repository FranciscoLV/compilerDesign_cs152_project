%{
 #include <stdio.h>
 #include <stdlib.h>
 void yyerror(const char *msg);
 int yylex();
 extern int currentLine;
 extern int currentPosition;
 FILE * yyin;
%}

%union{
  double  nval;
  char  *idval;
}


/* Reserved Word Tokens */
%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY
%token END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO BEGINLOOP
%token ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE
%token RETURN

/* Arithmetic Operators */
%token SUB ADD MULT DIV MOD

/* Comparision Operators */
%token EQ NEQ LT GT LTE GTE

/* Ident and Num */
%token <nval> NUMBER
%token <idval> IDENT

/* Special Symbols */
%token SEMICOLON COLON COMMA

/* Precedence */
%left L_PAREN R_PAREN L_BRACKET R_BRACKET
%left MULT DIV MOD SUB ADD
%left EQ NEQ LT GT LTE GTE AND OR
%right NOT ASSIGN 

%error-verbose
%start program 

%%

program:  | funct_start program {printf("program -> funct_start program");};

funct_start: function identifier semicolon beginparams ds_loop endparams beginlocals ds_loop endlocals beginbody ss_loop endbody
	{printf("funct_start -> function identifier semicolon beginparams ds_loop endparams beginlocals ds_loop endlocals beginbody ss_loop endbody\n");};

ds_loop:  | declaration semicolon ds_loop  {printf("ds_loop -> declaration semicolon ds_loop\n");};

ss_loop:  statement semicolon 
	      | statement semicolon ss_loop {printf("ss_loop -> statement semicolon ss_loop\n");};		 

declaration: i_loop colon integer {printf("declaration -> i_loop colon integer\n");}
		  | i_loop colon array l_bracket number r_bracket of integer {printf("declaration -> i_loop colon array l_bracket r_bracket of integer\n");};

 i_loop: identifier  comma i_loop {printf("i_loop -> identifier comma i_loop\n");}
		  | identifier {printf("i_loop -> identifier\n");};
		 
statement: var assign expression {printf("statement -> var assign expression\n");}
		  | if bool_expr then ss_loop endif {printf("statement -> if bool_expr then ss_loop endif\n");}
		  | if bool_expr then ss_loop else ss_loop endif {printf("statement -> if bool_expr then ss_loop else endif\n");}
		  | while bool_expr beginloop ss_loop endloop {printf("statement -> while bool_expr beginloop ss_loop endloop\n");}
		  | do beginloop ss_loop endloop while bool_expr {printf("statement -> do beginloop ss_loop endloop while bool_expr\n");}
		  | read var_loop {printf("statement -> read var_loop\n");}
		  | write var_loop {printf("statement -> write var_loop\n");}
		  | continue {printf("statement -> continue\n");}
		  | return expression {printf("statement -> return experssion\n");};  
		  
var_loop: var comma var_loop {printf("var_loop -> var comma var_loop\n");}
		  | var {printf("var_loop -> var\n");};
		  
bool_expr: relation_and_expr ro_loop {printf("bool_expr -> relation_and_expr ro_loop\n");};

ro_loop:  | or relation_and_expr ro_loop {printf("ro_loop -> or relation_and_expr ro_loop\n");};

relation_and_expr: relation_expr ra_loop {printf("relation_and_expr -> relation_and_expr ra_loop\n");};

ra_loop:  | and relation_expr ra_loop {printf("ra_loop -> or relation_expr ra_loop\n");};

relation_expr: not n_loop {printf("relation_expr -> not n_loop\n");}
		  | n_loop {printf("relation_expr -> n_loop\n");};

n_loop: expression comp expression {printf("n_loop -> expression comp expression\n");}
		  | true {printf("n_loop -> true\n");}
		  | false {printf("n_loop -> false\n");}
		  | l_paren bool_expr r_paren {printf("n_loop -> l_paren bool_expr r_paren\n");};

comp: 	  eq {printf("comp -> eq\n");}
		  | neq {printf("comp -> neq\n");} 
		  | lt {printf("comp -> lt\n");}
		  | gt {printf("comp -> gt\n");}
		  | lte {printf("comp -> lte\n");}
		  | gte {printf("comp -> gte\n");};
	  
expression: multiplicative_expr as_loop {printf("expression -> multiplicative_expr as_loop\n");};

as_loop:  | add multiplicative_expr as_loop {printf("as_loop -> add multiplicative_expr as_loop\n");}
		  | sub multiplicative_expr as_loop {printf("as_loop -> sub multiplicative_expr as_loop\n");};
 
multiplicative_expr: term mdm_loop {printf("multiplicative_expr -> term mdm_loop\n");};

mdm_loop: | mult term mdm_loop {printf("mdm_loop -> mult term mdm_loop\n");}
		  | div term mdm_loop {printf("mdm_loop -> div term mdm_loop\n");}
		  | mod term mdm_loop {printf("mdm_loop -> mod term mdm_loop\n");}; 
  
term: 	identifier l_paren ec_loop r_paren {printf("term -> identifier l_paren ec_loop r_paren\n");}
		  | sub s_loop {printf("term -> sub s_loop\n");}
		  | s_loop {printf("term -> s_loop\n");};

ec_loop:  | expression comma ec_loop {printf("ec_loop -> expression comma ec_loop\n");}
		  | expression {printf("ec_loop -> expression\n");};

s_loop:	l_paren expression r_paren {printf("s_loop -> l_paren expression r_paren\n");}
		  | number {printf("s_loop -> number\n");}
		  | var {printf("s_loop -> var\n");};
	  
var: identifier l_bracket expression r_bracket {printf("var -> identifier l_bracket expression r_bracket\n");}
		  | identifier {printf("var -> identifier\n");};
		  

function:  FUNCTION {printf("function -> FUNCTION\n");};

identifier: IDENT {printf("identifier -> IDENT %s\n", $1);};

semicolon: SEMICOLON {printf("semicolon -> SEMICOLON\n");};

beginparams: BEGIN_PARAMS {printf("beginparams -> BEGIN_PARAMS\n");};

endparams: END_PARAMS {printf("endparams -> END_PARAMS\n");};

beginlocals: BEGIN_LOCALS {printf("beginlocals -> BEGIN_LOCALS\n");};

endlocals: END_LOCALS {printf("endlocals -> END_LOCALS\n");};

beginbody: BEGIN_BODY {printf("beginbody -> BEGIN_BODY\n");};

endbody: END_BODY {printf("endbody -> END_BODY\n");};

comma: COMMA {printf("comma -> COMMA\n");};

colon: COLON {printf("colon -> COLON\n");};

integer: INTEGER {printf("integer -> INTEGER\n");};

array: ARRAY {printf("array -> ARRAY\n");};

l_bracket: L_BRACKET {printf("l_bracket -> L_BRACKET\n");};

number: NUMBER {printf("number -> NUMBER %f\n", $1);};

r_bracket: R_BRACKET {printf("r_bracket -> R_BRACKET\n");};

of: OF {printf("of -> OF\n");};

assign: ASSIGN {printf("assign -> ASSIGN\n");};

if: IF {printf("if -> IF\n");};

then: THEN {printf("then -> THEN\n");};

else: ELSE {printf("else -> ELSE\n");};

endif: ENDIF {printf("endif -> ENDIF\n");};

while: WHILE {printf("while -> WHILE\n");};

beginloop: BEGINLOOP {printf("beginloop -> BEGINLOOP\n");};

endloop: ENDLOOP {printf("endloop -> ENDLOOP\n");};

do: DO {printf("do -> DO\n");};

read: READ {printf("read -> READ\n");};

write: WRITE {printf("write -> WRITE\n");};

continue: CONTINUE {printf("continue -> CONTINUE\n");};

return: RETURN {printf("return -> RETURN\n");};

or: OR {printf("or -> OR\n");};

and: AND {printf("and -> AND\n");};

not: NOT {printf("not -> NOT\n");};

true: TRUE {printf("true -> TRUE\n");};

false: FALSE {printf("false -> FALSE\n");};

l_paren: L_PAREN {printf("l_paren -> L_PAREN\n");};

r_paren: R_PAREN {printf("r_paren -> R_PAREN\n");};

eq: EQ {printf("eq -> EQ\n");};

neq: NEQ {printf("neq -> NEQ\n");};

lt: LT {printf("lt -> LT\n");};

gt: GT {printf("gt -> GT\n");};

lte: LTE {printf("lte -> LTE\n");};

gte: GTE {printf("gte -> GTE\n");};

add: ADD {printf("add -> ADD\n");};

sub: SUB {printf("sub -> SUB\n");};

mult: MULT {printf("mult -> MULT\n");};

div: DIV {printf("div -> DIV\n");};

mod: MOD {printf("mod -> MOD\n");};

%%
int main(int argc, char ** argv)
{
   if(argc >= 2)
   {
      yyin = fopen(argv[1], "r");
      if(yyin == NULL)
      {
         yyin = stdin;
      }
   }
   else
   {
      yyin = stdin;
   }
   yyparse();
   return 0;
}
void yyerror(const char *msg) {
   printf("** Line %d, position %d: %s\n", currentLine, currentPosition, msg);
}
