LEXERS = lexer/c/lex lexer/python/lex

all: winnow $(LEXERS)

winnow: winnow.c
	gcc winnow.c -o winnow

lexer/%/lex: lexer/%/lex.l lexer/%/symtab.h
	flex -o $@.out.c $@.l
	gcc $@.out.c -o $@ -lfl
