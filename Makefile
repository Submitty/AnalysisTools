SRCS = $(foreach file,$(wildcard src/*.c),$(notdir $(file)))
BUILD_DIR = bin
LIB_DIR = lib_$(CC)
BINARIES = $(addprefix $(BUILD_DIR)/, $(SRCS:.c=))

SPLINT = $(shell command -v splint 2> /dev/null)
PYLINT = $(shell command -v pylint 2> /dev/null)
INDENT = $(shell command -v indent 2> /dev/null)

SCRIPTLINT_PYTHON = bin/plagiarism bin/anonymization bin/anonymize_log

LEXERS = lexer/c/lex lexer/python/lex lexer/java/lex
LANGUAGES = lang/newc

CFLAGS_gcc = -Iinclude -I/usr/local/include -O2 -g -Wall -Werror -D_POSIX_C_SOURCE=200809 -D_DEFAULT_SOURCE -Wno-unused-result
CFLAGS = $(CFLAGS_$(CC))
LINKER_FLAGS_gcc = -lm -lpcre
LINKER_FLAGS = $(LINKER_FLAGS_$(CC))

vpath %.c src

.PHONY: all directories clean indent

all: $(BUILD_DIR)/.lintstate directories $(BINARIES) $(LEXERS)
#	$(MAKE) $(LANGUAGES)
#	python setup.py build

directories: $(BUILD_DIR)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%: %.c
ifdef SPLINT
	splint $< -I include -I /usr/include/x86_64-linux-gnu \
		-compdef -retvalint -nullpass -nullstate -warnposix -formatcode -mayaliasunique -unrecog \
		-unqualifiedtrans -noeffect -mustfreefresh -observertrans -nullassign -onlytrans -statictrans -paramuse \
		-immediatetrans -globstate -nullret -mustfreeonly -branchstate -compdestroy
endif
ifdef INDENT
	indent $< -linux -st | diff - $<
endif
	$(CC) -o $@ $(CFLAGS) $< $(LINKER_FLAGS)

lexer/%/lex: lexer/%/lex.l lexer/%/tokens.h
	flex -o $@.out.c $@.l
	$(CC) $@.out.c -o $@ -lfl

lang/ast_node.o: lang/ast_node.c
	$(CC) -o $@ $(CFLAGS) -c $<

lang/%: lang/%/lex.l lang/%/parse.y lang/ast_node.o
	bison --defines=$@/parser.h --output=$@/parse.out.c $@/parse.y
	$(CC) -Ilang $@/parse.out.c -c -o $@/parse.o
	flex -o $@/lex.out.c $@/lex.l
	$(CC) -Ilang $@/main.c $@/lex.out.c $@/parse.o lang/ast_node.o -o $@/lex -lfl -lpython3.4

clean:
	rm $(BINARIES) -f
	rm $(BUILD_DIR)/.lintstate -f
	rm .analysis_data -rf

$(BUILD_DIR)/.lintstate: $(SCRIPTLINT_PYTHON)
ifdef PYLINT
	pylint --max-line-length=80 $(SCRIPTLINT_PYTHON)
endif
	touch $@

indent:
	indent -linux src/*.c
