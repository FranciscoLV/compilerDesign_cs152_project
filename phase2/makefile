mini: lex.yy.c y.tab.c
	gcc -g lex.yy.c y.tab.c -o mini -lfl

lex.yy.c: y.tab.c mini_l.lex
	lex mini_l.lex

y.tab.c: mini_l.y
	bison -d mini_l.y

clean: 
	rm -rf lex.yy.c y.tab.c y.tab.h mini mini.dSYM
