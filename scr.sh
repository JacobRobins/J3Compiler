bison -y -d grammar.y
flex token.l
gcc -c   y.tab.c lex.yy.c
gcc  y.tab.o lex.yy.o
alias j3c=./a.out
