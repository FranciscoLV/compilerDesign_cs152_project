%{
	int currentLine = 1;
	int currentPosition = 0;
%}

LETTER			[a-zA-Z]
DIGIT			[0-9]
WHITESPACE		[ \t]
IDENTIFIER		{LETTER}+(_?({LETTER}|{DIGIT})+)*

%%

"function" 		{printf("FUNCTION\n"); currentPosition += yyleng;}
"beginparams" 	{printf("BEGIN_PARAMS\n"); currentPosition += yyleng;}
"endparams" 	{printf("END_PARAMS\n"); currentPosition += yyleng;}
"beginlocals" 	{printf("BEGIN_LOCALS\n"); currentPosition += yyleng;}
"endlocals" 	{printf("END_LOCALS\n"); currentPosition += yyleng;}
"beginbody" 	{printf("BEGIN_BODY\n"); currentPosition += yyleng;}
"endbody" 		{printf("END_BODY\n"); currentPosition += yyleng;}
"program" 		{printf("PROGRAM\n"); currentPosition += yyleng;}
"beginprogram"	{printf("BEGIN_PROGRAM\n"); currentPosition += yyleng;}
"endprogram"	{printf("END_PROGRAM\n"); currentPosition += yyleng;}
"integer" 		{printf("INTEGER\n"); currentPosition += yyleng;}
"array"			{printf("ARRAY\n"); currentPosition += yyleng;}
"of"			{printf("OF\n"); currentPosition += yyleng;}
"if" 			{printf("IF\n"); currentPosition += yyleng;}
"then"		 	{printf("THEN\n"); currentPosition += yyleng;}
"endif"		 	{printf("ENDIF\n"); currentPosition += yyleng;}
"else" 			{printf("ELSE\n"); currentPosition += yyleng;}
"elseif" 		{printf("ELSEIF\n"); currentPosition += yyleng;}
"while" 		{printf("WHILE\n"); currentPosition += yyleng;}
"do" 			{printf("DO\n"); currentPosition += yyleng;}
"beginloop"	 	{printf("BEGINLOOP\n"); currentPosition += yyleng;}
"endloop" 		{printf("ENDLOOP\n"); currentPosition += yyleng;}
"continue"	 	{printf("CONTINUE\n"); currentPosition += yyleng;}
"read"		 	{printf("READ\n"); currentPosition += yyleng;}
"write"		 	{printf("WRITE\n"); currentPosition += yyleng;}
"and"			{printf("AND\n"); currentPosition += yyleng;}
"or"			{printf("OR\n"); currentPosition += yyleng;}
"not"			{printf("NOT\n"); currentPosition += yyleng;}
"true"		 	{printf("TRUE\n"); currentPosition += yyleng;}
"false"		 	{printf("FALSE\n"); currentPosition += yyleng;}

"-"				{printf("SUB\n"); currentPosition += yyleng;}
"+"				{printf("ADD\n"); currentPosition += yyleng;}
"*"				{printf("MULT\n"); currentPosition += yyleng;}
"/"				{printf("DIV\n"); currentPosition += yyleng;}
"%"				{printf("MOD\n"); currentPosition += yyleng;}

"=="			{printf("EQ\n"); currentPosition += yyleng;}
"<>"			{printf("NEQ\n"); currentPosition += yyleng;}
"<"				{printf("LT\n"); currentPosition += yyleng;}
">"				{printf("GT\n"); currentPosition += yyleng;}
"<="			{printf("LTE\n"); currentPosition += yyleng;}
">="			{printf("GTE\n"); currentPosition += yyleng;}


{IDENTIFIER}+	{printf("IDENT %s\n", yytext); currentPosition += yyleng;}
{DIGIT}+		{printf("NUMBER %s\n", yytext); currentPosition += yyleng;}

";"				{printf("SEMICOLON\n"); currentPosition += yyleng;}
":"				{printf("COLON\n"); currentPosition += yyleng;}
","				{printf("COMMA\n"); currentPosition += yyleng;}
"("				{printf("L_PAREN\n"); currentPosition += yyleng;}
")"				{printf("R_PAREN\n"); currentPosition += yyleng;}
":="			{printf("ASSIGN\n"); currentPosition += yyleng;}
"["				{printf("L_SQUARE_BRACKET\n"); currentPosition += yyleng;}
"]"				{printf("R_SQUARE_BRACKET\n"); currentPosition += yyleng;}

"##".*			{currentPosition += yyleng;}
{WHITESPACE}+	{currentPosition += yyleng;}

"\n"			{currentLine += 1; currentPosition = 0;}

({DIGIT}+|"_"+)({DIGIT}|"_"|{LETTER})+	{printf("ERROR at line %d, column %d: identifier \"%s\" must begin with a letter.\n", currentLine, currentPosition, yytext); exit(0);}
{IDENTIFIER}_+				{printf("ERROR at line %d, column %d: identifier \"%s\" cannot end with an underscore.\n", currentLine, currentPosition, yytext); exit(0);}
.					{printf("ERROR at line %d, column %d: unrecognized symbol \"%s\"\n", currentLine, currentPosition, yytext); exit(0);}

%%

int main(int argc, char** argv){
	if(argc >= 2){
		yyin = fopen(argv[1], "r");
		if(yyin == NULL){
			yyin = stdin;
		}
	}
	else{
		yyin = stdin;
	}

	yylex();

	return 0;
