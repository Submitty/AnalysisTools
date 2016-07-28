CC = gcc

SRCS = $(foreach file,$(wildcard src/*.c),$(notdir $(file)))
BUILD_DIR = bin
LIB_DIR = lib_$(CC)
BINARIES = $(addprefix $(BUILD_DIR)/, $(SRCS:.c=))

SPLINT = $(shell which splint 2> /dev/null)
PYLINT = $(shell which pylint 2> /dev/null)
INDENT = $(shell which indent 2> /dev/null)

SCRIPTLINT_PYTHON = bin/plagiarism bin/anonymization bin/anonymize_log bin/csa bin/count_token lang/lexer.py

LEXERS = lang/python/lex lang/java/lex
LANGUAGES = lang/c
LANGUAGES_STATE = $(foreach file,$(LANGUAGES),$(file)/.buildstate)

CFLAGS_gcc = -Iinclude -I/usr/local/include -O2 -g -Wall -Werror -D_POSIX_C_SOURCE=200809 -D_DEFAULT_SOURCE -Wno-unused-result
CFLAGS = $(CFLAGS_$(CC))
LINKER_FLAGS_gcc = -lm -lpcre
LINKER_FLAGS = $(LINKER_FLAGS_$(CC))

vpath %.c src

.PHONY: all directories clean indent ubuntudeps permissions

all: directories $(BUILD_DIR)/.lintstate $(BINARIES) $(LEXERS) $(LANGUAGES_STATE)
	chmod -R a+r lang
	chmod a+x lang/python2/parse
	python3 setup.py build

directories: $(BUILD_DIR)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%: %.c
ifneq ($(SPLINT),)
	$(SPLINT) $< -I include -I /usr/include/x86_64-linux-gnu \
		-compdef -retvalint -nullpass -nullstate -warnposix -formatcode -mayaliasunique -unrecog \
		-unqualifiedtrans -noeffect -mustfreefresh -observertrans -nullassign -onlytrans -statictrans -paramuse \
		-immediatetrans -globstate -nullret -mustfreeonly -branchstate -compdestroy
endif
ifneq ($(INDENT),)
	$(INDENT) $< -linux -st | diff - $<
endif
	$(CC) -o $@ $(CFLAGS) $< $(LINKER_FLAGS)

lang/%/lex: lang/%/lex.l lang/%/tokens.h
	flex -o $@.out.c $@.l
	$(CC) $@.out.c -o $@ -lfl

lang/ast_node.o: lang/ast_node.c
	$(CC) -o $@ $(CFLAGS) -c $<

lang/%/.buildstate: lang/% lang/%/lex.l lang/%/parse.y lang/ast_node.o
	bison --defines=$</parser.h --output=$</parse.out.c $</parse.y
	$(CC) -Ilang $</parse.out.c -c -o $</parse.o
	flex -o $</lex.out.c $</lex.l
	$(CC) -Ilang $</main.c $</lex.out.c $</parse.o lang/ast_node.o -o $</lex -lfl `pkg-config --libs python-3.4`
	touch $@

clean:
	rm $(LEXERS) -f
	rm $(LANGUAGES_STATE) -f
	rm $(BINARIES) -f
	rm $(BUILD_DIR)/.lintstate -f
	rm .analysis_data -rf

$(BUILD_DIR)/.lintstate: $(SCRIPTLINT_PYTHON)
ifneq ($(PYLINT),)
	$(PYLINT) --disable=import-error --disable=no-member --disable=wrong-import-position --max-line-length=80 $(SCRIPTLINT_PYTHON)
endif
	touch $@

indent:
	indent -linux src/*.c

ubuntudeps:
	add-apt-repository "http://downloads.skewed.de/apt/trusty universe" -y
	add-apt-repository ppa:ubuntu-toolchain-r/test -y
	apt-get update -qq
	apt-get install -qq build-essential pkg-config flex bison
	apt-get install -qq libpcre3 libpcre3-dev
	apt-get install -qq splint indent
	apt-get install -qq python3 python3-dev libpython3.4 python3-pip
	python3 -m pip install pylint
	apt-get install -qq --force-yes python3-graph-tool
