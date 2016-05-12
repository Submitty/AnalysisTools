CC = gcc
SRCS = $(foreach file,$(wildcard src/*),$(notdir $(file)))
BUILD_DIR = bin
LIB_DIR = lib_$(CC)
BINARIES = $(addprefix $(BUILD_DIR)/, $(SRCS:.c=))

LEXERS = lexer/c/lex lexer/python/lex

CFLAGS_gcc = -Iinclude -std=c99 -g -Wall -Werror
CFLAGS = $(CFLAGS_$(CC))
LINKER_FLAGS_gcc = -lm
LINKER_FLAGS = $(LINKER_FLAGS_$(CC))

vpath %.c src

.PHONY: all directories

all: directories $(BINARIES) $(LEXERS)

directories: $(BUILD_DIR)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%: %.c
	$(CC) -o $@ $(CFLAGS) $(LINKER_FLAGS) $<

lexer/%/lex: lexer/%/lex.l lexer/%/symtab.h
	flex -o $@.out.c $@.l
	gcc $(CFLAGS) $@.out.c -o $@ -lfl
