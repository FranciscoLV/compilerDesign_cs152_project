%{
 #define YY_NO_UNPUT
 #include <stdio.h>
 #include <stdlib.h>
 #include <map>
 #include <string.h>
 #include <vector>
 
 void yyerror(const char *msg);
 int yylex();
 
 extern int currentLine;
 extern int currentPosition;
 extern char* yytext;
 extern char* prog;

 std::string nTemp();
 std::string nLabel();

 const char* empty = "";

 std::map <std::string, int> variables;
 std::map <std::string, int> functions;

 std::vector<std::string> wordsRes = {"FUNCTION", "BEGIN_PARAMS", "END_PARAMS", "BEGIN_LOCALS", "END_LOCALS", "BEGIN_BODY", "END_BODY", "INTEGER",
    "ARRAY", "OF", "IF", "THEN", "ENDIF", "ELSE", "WHILE", "DO", "BEGINLOOP", "ENDLOOP", "CONTINUE", "READ", "WRITE", "AND", "OR", 
    "NOT", "TRUE", "FALSE", "RETURN", "SUB", "ADD", "MULT", "DIV", "MOD", "EQ", "NEQ", "LT", "GT", "LTE", "GTE", "L_PAREN", "R_PAREN", "L_SQUARE_BRACKET",
    "R_SQUARE_BRACKET", "COLON", "SEMICOLON", "COMMA", "ASSIGN", "function", "beginparams", "endparams", "beginlocals", "endlocals", "integer", 
    "beginbody", "endbody", "beginloop", "endloop", "if", "endif", "continue", "while", "else", "read", "do", "write", "Ident",};
%}

%union{
  char  *idval;
  int  nval;
  
  struct Expr{
	char* place;
	char* code;
	bool array;
  }expr;
  
  struct Stat{
	char* code;
  }stat;
}

%type <expr> FIdent i_loop ds_loop declaration var var_loop multiplicative_expr expression
%type <expr> bool_expr term n_loop relation_and_expr ec_loop comp Ident relation_expr
%type <stat> ss_loop statement 

%error-verbose
%start program 

/* Ident and Num */
%token <nval> NUMBER
%token <idval> IDENT

/* Reserved Word Tokens */
%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY
%token END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO BEGINLOOP
%token ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE
%token RETURN

/* Special Symbols */
%token SEMICOLON COLON COMMA

/* Precedence */
%left L_PAREN R_PAREN L_BRACKET R_BRACKET
%left MULT DIV MOD SUB ADD
%left EQ NEQ LT GT LTE GTE AND OR ASSIGN
%right NOT

%% 
/* GRAMMAR */
program: %empty { 
    std::string tempM = "main";
    if(functions.find(tempM) == functions.end()){        //check if main is declared
        char buffer[128];
        snprintf(buffer, 127, "The function main has not been declared");
        yyerror(buffer);
    }
    if(variables.find(std::string (prog)) != variables.end()){ //check if a variable was delcared with the same name as the program
        char buffer[128];
        snprintf(buffer, 127, "The program name has been declared as a variable");
        yyerror(buffer);
    }

} | funct_start program {};

funct_start: FUNCTION FIdent SEMICOLON BEGIN_PARAMS ds_loop END_PARAMS BEGIN_LOCALS ds_loop END_LOCALS BEGIN_BODY ss_loop END_BODY{
    std::string temp = "func ";
    temp.append($2.place);
    temp.append("\n");
    temp.append($2.code);
    temp.append($5.code);

    std::string paramsInit = $5.code;
    int paramNum = 0;
    while(paramsInit.find(".") != std::string::npos){    //while the next "." is not in the last position of the string
        size_t pos = paramsInit.find(".");
        paramsInit.replace(pos, 1, "=");
        std::string param = ", $";
        param.append(std::to_string(paramNum++));  
        param.append("\n");
        paramsInit.replace(paramsInit.find("\n", pos), 1, param);
    }
    temp.append(paramsInit);
    temp.append($8.code);
    
    std::string statements($11.code);
    if(statements.find("continue") != std::string::npos){
        char buffer[128];
        snprintf(buffer, 127, "There is a continue outside the loop in function %s",  $2.place);
        yyerror(buffer);
    }
    temp.append(statements);
    temp.append("endfunc\n\n");

    printf("%s", temp.c_str());   
};

ds_loop: %empty {
    $$.code = strdup(empty);
    $$.place = strdup(empty);

} | declaration SEMICOLON ds_loop {
    std::string temp;
    temp.append($1.code);
    temp.append($3.code);

    $$.code = strdup(temp.c_str());
    $$.place = strdup(empty);
};

ss_loop: statement SEMICOLON ss_loop {
    std::string temp;
    temp.append($1.code);
    temp.append($3.code);

    $$.code = strdup(temp.c_str());

} | statement SEMICOLON {
    std::string temp;
    temp.append($1.code);

    $$.code = strdup(temp.c_str());
};

declaration: i_loop COLON INTEGER  {
    std::string groupVariable($1.place);
    std::string temp;
    std::string variable;
    bool flag = true;
    size_t oldPos = 0;
    size_t currPos = 0;
    bool isWordReserverd = false;
    while(flag){
        currPos = groupVariable.find("|", oldPos);
        if(currPos == std::string::npos){
            temp.append(". ");
            variable =  groupVariable.substr(oldPos, currPos);
            temp.append(variable);
            temp.append("\n");
            flag = false;
        }
        else{
            currPos = groupVariable.find("|", oldPos);
            size_t theVariable = currPos - oldPos;
            temp.append(". ");
            variable = groupVariable.substr(oldPos, theVariable);
            temp.append(variable);
            temp.append("\n");
        }
        for(unsigned int i = 0; i < wordsRes.size(); i++){
            if(wordsRes.at(i) == variable){
                isWordReserverd = true;
            }
        }
        //find has to be at the end so that it is not repeated
        if(variables.find(variable) != variables.end()){
            char buffer[128];
            snprintf(buffer, 127, "The variable %s was redeclared", variable.c_str());
            yyerror(buffer);
        }
        else if(isWordReserverd){
            char buffer[128];
            snprintf(buffer, 127, "The declaration of the word %s is invalid because it is a reserved word", variable.c_str());
            yyerror(buffer);            
        }
        else{
            variables.insert(std::pair<std::string,int>(variable,0));
        }

        oldPos = currPos + 1;
    }
    
    $$.code = strdup(temp.c_str());
    $$.place = strdup(empty);

} | i_loop COLON ARRAY L_BRACKET NUMBER R_BRACKET OF INTEGER {   
    if($5 <= 0){
        char buffer[128];
        snprintf(buffer, 127, "The size of the array cannot be less than 1");
        yyerror(buffer);
    }

    std::string groupVariable($1.place);
    std::string temp;
    std::string variable;
    bool flag = true;

    size_t oldPos = 0;
    size_t currPos = 0;

    while(flag){
        currPos = groupVariable.find("|", oldPos);
        if(currPos == std::string::npos){
            temp.append(".[] ");
            variable = groupVariable.substr(oldPos, currPos);
            temp.append(variable);
            temp.append(", ");
            temp.append(std::to_string($5));
            temp.append("\n");
            flag = false;
        }
        else{
            size_t lastVar = currPos - oldPos;
            temp.append("[] ");
            variable = groupVariable.substr(oldPos, lastVar);
            temp.append(variable);
            temp.append(", ");
            temp.append(std::to_string($5));
            temp.append("\n");
        }

        if(variables.find(variable) != variables.end()){
            char buffer[128];
            snprintf(buffer, 127, "The variable %s was redeclared", variable.c_str());
            yyerror(buffer);
        }
        else{
            variables.insert(std::pair<std::string, int>(variable, $5));
        }

        oldPos = currPos + 1;
    }

    $$.code = strdup(temp.c_str());
    $$.place = strdup(empty);
};

i_loop: Ident {
    
    $$.place = strdup($1.place);
    $$.code = strdup(empty);
} | Ident COMMA i_loop {
    std::string temp;
    temp.append($1.place);
    temp.append("|");
    temp.append($3.place); 
    
    $$.place = strdup(temp.c_str());
    $$.code = strdup(empty);
};
		
		 
statement: var ASSIGN expression {
    std::string temp;
    temp.append($1.code);
    temp.append($3.code);

    if($1.array && $3.array){
        temp.append(". ");
        temp.append($3.place);
        temp.append("\n");
        temp.append("=[]");
        temp.append($3.place);
        temp.append(", ");
        temp.append($3.place);
        temp.append("\n");
        temp.append("[]=");
    }
    else if($1.array){
        temp.append("[]=");
    }
    else if($3.array){
        temp.append("=[]");
    }
    else{
        temp.append("= ");
    }

    temp.append($1.place);
    temp.append(", ");
    temp.append($3.place);
    temp.append("\n");

  $$.code = strdup(temp.c_str());

} | IF bool_expr THEN ss_loop ENDIF {
    std::string ifthen = nLabel();
    std::string after = nLabel();
    std::string temp;

    temp.append($2.code);
    //if
    temp.append("?:= ");
    temp.append(ifthen);
    temp.append(", ");
    temp.append($2.place);
    temp.append("\n");

    temp.append(":= ");
    temp.append(after);
    temp.append("\n");
    temp.append(": ");
    temp.append(ifthen);
    temp.append("\n");

    temp.append($4.code);
    temp.append(": ");
    temp.append(after);
    temp.append("\n");

    $$.code = strdup(temp.c_str());

} | IF bool_expr THEN ss_loop ELSE ss_loop ENDIF {
    std::string ifthen = nLabel();
    std::string after = nLabel();
    std::string temp;

    temp.append($2.code);
    //if
    temp.append("?:= ");
    temp.append(ifthen);
    temp.append(", ");
    temp.append($2.place);
    temp.append("\n");
    //else
    temp.append($6.code);

    temp.append(":= ");
    temp.append(after);
    temp.append("\n");
    temp.append(": ");
    temp.append(ifthen);
    temp.append("\n");

    temp.append($4.code);
    temp.append(": ");
    temp.append(after);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
} | WHILE bool_expr BEGINLOOP ss_loop ENDLOOP {
    std::string temp;
    std::string startLoop = nLabel();
    std::string endLoop = nLabel();
    std::string whileStart = nLabel();

    std::string Statement = $4.code;
    std::string loop;
    loop.append(":= ");
    loop.append(startLoop);
    while(Statement.find("continue") != std::string::npos){
        Statement.replace(Statement.find("continue"), 8, loop);
    } 
    temp.append(": ");
    temp.append(whileStart);
    temp.append("\n");
    temp.append($2.code);
    temp.append("?:= ");
    temp.append(startLoop);
    temp.append(", ");
    temp.append($2.place);
    temp.append("\n");
    temp.append(":= ");
    temp.append(endLoop);
    temp.append("\n");
    temp.append(": ");
    temp.append(startLoop);
    temp.append("\n");   
    temp.append(Statement);
    temp.append(":= ");
    temp.append(whileStart);
    temp.append("\n");
    temp.append(": ");
    temp.append(endLoop);
    temp.append("\n");

    $$.code = strdup(temp.c_str());

} | DO BEGINLOOP ss_loop ENDLOOP WHILE bool_expr {
    std::string temp;
    std::string startLoop = nLabel();
    std::string whileStart = nLabel();
    
    std::string Statement = $3.code;
    std::string loop;
    loop.append(":= ");
    loop.append(whileStart);
    while(Statement.find("continue") != std::string::npos){
        Statement.replace(Statement.find("continue"),8,loop);
    }
    temp.append(": ");
    temp.append(startLoop);
    temp.append("\n");
    temp.append(Statement);
    temp.append(": ");
    temp.append(whileStart);
    temp.append("\n");
    temp.append($6.code);
    temp.append("?:= ");
    temp.append(startLoop);
    temp.append(", ");
    temp.append($6.place);
    temp.append("\n");
    
    $$.code = strdup(temp.c_str());
} | READ var_loop {
    std::string temp = $2.code;
    size_t pos = 0; 
    do{
        pos = temp.find("|", pos);
        if(pos == std::string::npos)
            break;
        temp.replace(pos, 1, "<");
    }while(true);

    $$.code = strdup(temp.c_str());

} | WRITE var_loop {
    std::string temp = $2.code;
    size_t pos = 0;
    do{
        pos = temp.find("|", pos);
        if(pos == std::string::npos)
            break;
        temp.replace(pos, 1, ">");    
    }while(true);

    $$.code = strdup(temp.c_str());
} | CONTINUE {
    std::string temp = "continue\n";
    $$.code = strdup(temp.c_str());
} | RETURN expression {
    std::string temp;
    temp.append($2.code);
    temp.append("ret ");
    temp.append($2.place);
    temp.append("\n");
    $$.code = strdup(temp.c_str());
};  

var_loop: var COMMA var_loop {
    std::string temp;
    temp.append($1.code);
    if($1.array)
        temp.append(".[]| ");
    else
        temp.append(".| ");
    
    temp.append($1.place);
    temp.append("\n");
    temp.append($3.code);

    $$.code = strdup(temp.c_str());
    $$.place = strdup(empty);

} | var {
    std::string temp;
    temp.append($1.code);
    if($1.array)
        temp.append(".[]| ");
    else
        temp.append(".| ");

    temp.append($1.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
    $$.place = strdup(empty);
};
		  
bool_expr: relation_and_expr {
    $$.place = strdup($1.place);
    $$.code = strdup($1.code);

} | relation_and_expr OR bool_expr {
    std::string dest = nTemp();
    std::string temp;
    
    temp.append($1.code);
    temp.append($3.code);
    temp.append(". ");
    temp.append(dest);  
    temp.append("\n");
    temp.append("|| ");
    temp.append(dest);
    temp.append(", ");
    temp.append($1.place);
    temp.append(", ");
    temp.append($3.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
    $$.place = strdup(dest.c_str());
};

relation_and_expr: relation_expr{
    $$.place = strdup($1.place);  
    $$.code = strdup($1.code);

} | relation_expr AND relation_and_expr{
    std::string dest = nTemp();
    std::string temp;

    temp.append($1.code);
    temp.append($3.code);
    temp.append(". ");
    temp.append(dest);
    temp.append("\n");
    temp.append("&& ");
    temp.append(dest);
    temp.append(", ");
    temp.append($1.place);
    temp.append(", ");
    temp.append($3.place);
    temp.append("\n");
    $$.code = strdup(temp.c_str());
    $$.place = strdup(dest.c_str());
};

relation_expr: NOT n_loop {
    std::string dest = nTemp();
    std::string temp;

    temp.append($2.code);
    temp.append(". ");
    temp.append(dest);
    temp.append("\n");
    temp.append("! ");
    temp.append(dest);
    temp.append(", ");
    temp.append($2.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
    $$.place = strdup(dest.c_str());
} | n_loop {
    $$.place = strdup($1.place);   
    $$.code = strdup($1.code);
};

n_loop: expression comp expression {
    std::string dest = nTemp();
    std::string temp;

    temp.append($1.code);
    temp.append($3.code);
    temp.append(". ");
    temp.append(dest);
    temp.append("\n");
    temp.append($2.place);
    temp.append(dest);
    temp.append(", ");
    temp.append($1.place);
    temp.append(", ");
    temp.append($3.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
    $$.place = strdup(dest.c_str());
} | TRUE {
    char temp[2] = "1";
    $$.place = strdup(temp);
    $$.code = strdup(empty);
} | FALSE {
    char temp[2] = "0";
    $$.place = strdup(temp);
    $$.code = strdup(empty);
} | L_PAREN bool_expr R_PAREN {
    $$.place = strdup($2.place);
    $$.code = strdup($2.code);
};

comp:   EQ {
    std::string temp = "== ";
    $$.place = strdup(temp.c_str());
    $$.code = strdup(empty);
} | NEQ {
    std::string temp = "!= ";
    $$.place = strdup(temp.c_str());
    $$.code = strdup(empty);
} | LT {
    std::string temp = "< ";
    $$.place = strdup(temp.c_str());
    $$.code = strdup(empty);
} | GT {
    std::string temp = "> ";
    $$.place = strdup(temp.c_str());
    $$.code = strdup(empty);
} | LTE {
    std::string temp = "<= ";
    $$.place = strdup(temp.c_str());
    $$.code = strdup(empty);    

} | GTE {
    std::string temp = ">= ";
    $$.place = strdup(temp.c_str());
    $$.code = strdup(empty);

};
	  
expression: multiplicative_expr {
    $$.code = strdup($1.code);
    $$.place = strdup($1.place); 

} | multiplicative_expr ADD expression {
    $$.place = strdup(nTemp().c_str());

  std::string temp;
  temp.append($1.code);
  temp.append($3.code);
  temp.append(". ");
  temp.append($$.place);
  temp.append("\n");
  temp.append("+ ");
  temp.append($$.place); 
  temp.append(", ");
  temp.append($1.place);
  temp.append(", ");
  temp.append($3.place);
  temp.append("\n");

   $$.code = strdup(temp.c_str());

} | multiplicative_expr SUB expression {
    $$.place = strdup(nTemp().c_str());

    std::string temp;
    temp.append($1.code);
    temp.append($3.code);
    temp.append(". ");
    temp.append($$.place);
    temp.append("\n");
    temp.append("- ");
    temp.append($$.place);
    temp.append(", ");
    temp.append($1.place);
    temp.append(", ");
    temp.append($3.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
};
 
multiplicative_expr: term {
    $$.code = strdup($1.code); 
    $$.place = strdup($1.place);    

} | term MULT multiplicative_expr {
    $$.place = strdup(nTemp().c_str());
    std::string temp;
    temp.append(". ");
    temp.append($$.place);
    temp.append("\n");
    temp.append($1.code);
    temp.append($3.code);
    temp.append("* ");
    temp.append($$.place);
    temp.append(", ");
    temp.append($1.place);
    temp.append(", ");
    temp.append($3.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());

} | term DIV multiplicative_expr {
    $$.place = strdup(nTemp().c_str());

    std::string temp;
    temp.append($1.code);
    temp.append($3.code);
    temp.append("/ ");
    temp.append($$.place);
    temp.append(", ");
    temp.append($1.place);
    temp.append(", ");
    temp.append($3.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());

} | term MOD multiplicative_expr {
    $$.place = strdup(nTemp().c_str());

    std::string temp;
    temp.append(". ");
    temp.append($$.place);
    temp.append("\n");
    temp.append($1.code);
    temp.append($3.code);
    temp.append("% ");
    temp.append($$.place);
    temp.append(", ");
    temp.append($1.place);
    temp.append(", ");
    temp.append($3.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
};
  
term: var{
    if($$.array == true){
        std::string temp;
        std::string dest = nTemp();
        temp.append($1.code);
        temp.append(". ");
        temp.append(dest);
        temp.append("\n");
        temp.append("=[] ");
        temp.append(dest);
        temp.append(", ");
        temp.append($1.place);
        temp.append("\n");
        $$.code = strdup(temp.c_str());
        $$.place = strdup(dest.c_str());
        $$.array = false;
    }
    else{ 
        std::string temp;
        std::string dest = nTemp();

        temp.append($1.code);
        temp.append(". ");
        temp.append(dest);
        temp.append("\n");
        temp.append("= ");
        temp.append(dest);
        temp.append(", ");
        temp.append($1.place);
        temp.append("\n");
        
        $$.code = strdup(temp.c_str());
        $$.place = strdup(dest.c_str());
    }
} | SUB var {
    $$.place = strdup(nTemp().c_str());
    std::string temp;
    temp.append($2.code);
    temp.append(". ");
    temp.append($$.place);
    temp.append("\n");
    if($2.array){
        temp.append("=[] ");
        temp.append($$.place);
        temp.append(", ");
        temp.append($2.place);
        temp.append("\n");
    }
    else{
        temp.append("= ");
        temp.append($$.place);
        temp.append(", ");
        temp.append($2.place);
        temp.append("\n");
    }
    temp.append("* ");
    temp.append($$.place);
    temp.append(", ");
    temp.append($$.place);
    temp.append(", -1\n");

    $$.code = strdup(temp.c_str());
    $$.array = false;

} | NUMBER { 
    std::string dest = nTemp();
    std::string temp;

    temp.append(". ");
    temp.append(dest);
    temp.append("\n");
    temp.append("= ");
    temp.append(dest);
    temp.append(", ");
    temp.append(std::to_string($1));
    temp.append("\n");
 
    $$.code = strdup(temp.c_str());
    $$.place = strdup(dest.c_str());

} | SUB NUMBER {
    std::string temp;
    temp.append("-");
    temp.append(std::to_string($2));
    $$.code = strdup(empty);
    $$.place = strdup(temp.c_str());

} | L_PAREN expression R_PAREN {
    $$.code = strdup($2.code);
    $$.place = strdup($2.place);

} | SUB L_PAREN expression R_PAREN {
    $$.place = strdup($3.place);
    std::string temp;
    temp.append($3.code);
    temp.append("* ");
    temp.append($3.place);
    temp.append(", ");
    temp.append($3.place);
    temp.append(", -1\n");
    $$.code = strdup(temp.c_str());    

} | Ident L_PAREN ec_loop R_PAREN{
    if(functions.find(std::string($1.place)) == functions.end()){
        char buffer[128];
        snprintf(buffer, 127, "The variable %s is undeclared", $1.place);
        yyerror(buffer);
    }

    $$.place = strdup(nTemp().c_str());

    std::string temp;
    temp.append($3.code);
    temp.append(". ");
    temp.append($$.place);
    temp.append("\n");
    temp.append("call ");
    temp.append($1.place);
    temp.append(", ");
    temp.append($$.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
};


ec_loop: %empty{
    $$.code = strdup(empty);
    $$.place = strdup(empty);

} | expression COMMA ec_loop {
    std::string temp;
    temp.append($1.code);
    temp.append("param ");
    temp.append($1.place);
    temp.append("\n");
    temp.append($3.code);

    $$.code = strdup(temp.c_str());
    $$.place = strdup(empty);
    
} | expression {
    std::string temp;
    temp.append($1.code);
    temp.append("param ");
    temp.append($1.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
    $$.place = strdup(empty);
};
	  
var: Ident L_BRACKET expression R_BRACKET {
    if(variables.find(std::string($1.place)) == variables.end()){
        char buffer[128];
        snprintf(buffer, 127, "The variable %s is undeclared", $1.place);
        yyerror(buffer);
    }
    else if(variables.find(std::string($1.place)) -> second == 0){
        char buffer[128];
        snprintf(buffer, 127, "Indexing a non-array variable %s", $1.place);
        yyerror(buffer);
    }
    std::string temp;
    temp.append($1.place);
    temp.append(", ");
    temp.append($3.place);

    $$.code = strdup($3.code);
    $$.place = strdup(temp.c_str());
    $$.array = true;
    
} | Ident { 
    if(variables.find(std::string($1.place)) == variables.end()){
        char buffer[128];
        snprintf(buffer, 127, "The variable %s is undeclared", $1.place);
        yyerror(buffer); 
    }
    else if (variables.find(std::string($1.place)) -> second > 0){
        char buffer[128];
        snprintf(buffer, 127, "Index for variable %s is missing", $1.place);
        yyerror(buffer);        
    }
    $$.code = strdup(empty);
    $$.place = strdup($1.place);
    $$.array = false;
}; 

FIdent: IDENT{
    if(functions.find(std::string($1)) != functions.end()){
        char buffer[128];
        snprintf(buffer, 127, "The function %s has been redeclared",$1);
        yyerror(buffer);
    }
    else{
        functions.insert(std::pair<std::string,int>($1,0));
    }

    $$.place = strdup($1);
    $$.code = strdup(empty);
};

Ident: IDENT{
    
  $$.place = strdup($1);
  $$.code = strdup(empty);;
};

%%

void yyerror(const char* s) {
   printf("ERROR: %s on line %ld \n", s, currentLine);
}

std::string nTemp() {
  static int number = 0;
  std::string temp = "_temp_" + std::to_string(number++);
  return temp;
}

std::string nLabel() {
  static int number = 0;
  std::string temp = "_label_" + std::to_string(number++);
  return temp;
}