LEXERS = lang/c/lex lang/python/lex lang/java/lex

.PHONY: all clean

all: bin/lichen $(LEXERS)

lang/%/lex: lang/%/lex.l lang/%/tokens.h
	flex -o $@.out.c $@.l
	$(CC) $@.out.c -o $@ -lfl

bin/lichen: app/*.hs src/*.hs .ubuntudeps
	stack install

clean:
	rm $(LEXERS) -f
	stack clean
	rm bin/lichen

.ubuntudeps:
	apt-get install -qq build-essential pkg-config flex
	wget -qO- https://get.haskellstack.org/ | sh
	stack upgrade
	stack setup
	touch .ubuntudeps
