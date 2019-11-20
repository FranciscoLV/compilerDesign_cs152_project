

%{
   #include "y.tab.h"
   #include <string.h>
   #include <stdio.h>
   #include <stdlib.h>
	int currentLine = 1;
	int currentPosition = 0;
%}

LETTER			[a-zA-Z]
DIGIT			[0-9]
WHITESPACE		[ \t]
IDENTIFIER		{LETTER}+(_?({LETTER}|{DIGIT})+)*

%%

"function" 		{currentPosition += yyleng; return FUNCTION;}
"beginparams" 	{return BEGIN_PARAMS; currentPosition += yyleng;}
"endparams" 	{return END_PARAMS; currentPosition += yyleng;}
"beginlocals" 	{return BEGIN_LOCALS; currentPosition += yyleng;}
"endlocals" 	{return END_LOCALS; currentPosition += yyleng;}
"beginbody" 	{return BEGIN_BODY; currentPosition += yyleng;}
"endbody" 		{return END_BODY; currentPosition += yyleng;}
"integer" 		{return INTEGER; currentPosition += yyleng;}
"array"			{return ARRAY; currentPosition += yyleng;}
"of"			{return OF; currentPosition += yyleng;}
"if" 			{return IF; currentPosition += yyleng;}
"then"		 	{return THEN; currentPosition += yyleng;}
"endif"		 	{return ENDIF; currentPosition += yyleng;}
"else" 			{return ELSE; currentPosition += yyleng;}
"while" 		{return WHILE; currentPosition += yyleng;}
"do" 			{return DO; currentPosition += yyleng;}
"beginloop"	 	{return BEGINLOOP; currentPosition += yyleng;}
"endloop" 		{return ENDLOOP; currentPosition += yyleng;}
"continue"	 	{return CONTINUE; currentPosition += yyleng;}
"read"		 	{return READ; currentPosition += yyleng;}
"write"		 	{return WRITE; currentPosition += yyleng;}
"and"			{return AND; currentPosition += yyleng;}
"or"			{return OR; currentPosition += yyleng;}
"not"			{return NOT; currentPosition += yyleng;}
"true"		 	{return TRUE; currentPosition += yyleng;}
"false"		 	{return FALSE; currentPosition += yyleng;}
"return" 		{return RETURN; currentPosition += yyleng;}

"-"				{return SUB; currentPosition += yyleng;}
"+"				{return ADD; currentPosition += yyleng;}
"*"				{return MULT; currentPosition += yyleng;}
"/"				{return DIV; currentPosition += yyleng;}
"%"				{return MOD; currentPosition += yyleng;}

"=="			{return EQ; currentPosition += yyleng;}
"<>"			{return NEQ; currentPosition += yyleng;}
"<"				{return LT; currentPosition += yyleng;}
">"				{return GT; currentPosition += yyleng;}
"<="			{return LTE; currentPosition += yyleng;}
">="			{return GTE; currentPosition += yyleng;}


{IDENTIFIER}+	{yylval.idval = strdup(yytext); currentPosition += yyleng;return IDENT;}
{DIGIT}+		{yylval.nval=atoi(yytext); currentPosition += yyleng;return NUMBER;}

";"				{return SEMICOLON; currentPosition += yyleng;}
":"				{return COLON; currentPosition += yyleng;}
","				{return COMMA; currentPosition += yyleng;}
"("				{return L_PAREN; currentPosition += yyleng;}
")"				{return R_PAREN; currentPosition += yyleng;}
":="			{return ASSIGN; currentPosition += yyleng;}
"["				{return L_BRACKET; currentPosition += yyleng;}
"]"				{return R_BRACKET; currentPosition += yyleng;}

"##".*			{currentPosition += yyleng;}
{WHITESPACE}+	{currentPosition += yyleng;}

"\n"			{currentLine += 1; currentPosition = 0;}

({DIGIT}+|"_"+)({DIGIT}|"_"|{LETTER})+	{printf("ERROR at line %d, column %d: identifier \"%s\" must begin with a letter.\n", currentLine, currentPosition, yytext); exit(0);}
{IDENTIFIER}_+				{printf("ERROR at line %d, column %d: identifier \"%s\" cannot end with an underscore.\n", currentLine, currentPosition, yytext); exit(0);}
.					{printf("ERROR at line %d, column %d: unrecognized symbol \"%s\"\n", currentLine, currentPosition, yytext); exit(0);}

%%

