LEXERS = lang/c/lex lang/python/lex lang/java/lex
export STACK_ROOT = ${CURDIR}global_stack

.PHONY: all clean

all: $(LEXERS) bin/lichen

lang/%/lex: lang/%/lex.l lang/%/tokens.h
	flex -o $@.out.c $@.l
	$(CC) $@.out.c -o $@ -lfl

bin/lichen: app/*.hs src/*.hs .deps
	stack install --allow-different-user

clean:
	rm $(LEXERS) -f
	stack clean
	rm bin/lichen

stack:
	stack upgrade --install-ghc
	stack setup --allow-different-user

.deps:
	#wget -qO- https://get.haskellstack.org/ | sh
	stack upgrade --install-ghc
	stack setup --allow-different-user
	touch .deps
