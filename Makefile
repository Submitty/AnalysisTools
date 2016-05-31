CC = gcc
SRCS = $(foreach file,$(wildcard src/*),$(notdir $(file)))
BUILD_DIR = bin
LIB_DIR = lib_$(CC)
BINARIES = $(addprefix $(BUILD_DIR)/, $(SRCS:.c=))

LEXERS = lexer/c/lex lexer/python/lex lexer/java/lex

CFLAGS_gcc = -Iinclude -std=c99 -O2 -g -Wall -Werror -Wno-unused-result
CFLAGS = $(CFLAGS_$(CC))
LINKER_FLAGS_gcc = -lm
LINKER_FLAGS = $(LINKER_FLAGS_$(CC))

vpath %.c src

.PHONY: all directories clean

all: directories $(BINARIES) $(LEXERS)

directories: $(BUILD_DIR)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%: %.c
	$(CC) -o $@ $(CFLAGS) $(LINKER_FLAGS) $<

lexer/%/lex: lexer/%/lex.l lexer/%/symtab.h
	flex -o $@.out.c $@.l
	$(CC) $@.out.c -o $@ -lfl

clean:
	rm $(BINARIES)
	rm moss_data -r
