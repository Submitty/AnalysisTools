CC = gcc

SRCS = $(foreach file,$(wildcard src/*.c),$(notdir $(file)))
BUILD_DIR = bin
LIB_DIR = lib_$(CC)
BINARIES = $(addprefix $(BUILD_DIR)/, $(SRCS:.c=))

CFLAGS_gcc = -Iinclude -I/usr/local/include -O2 -g -Wall -Werror -D_POSIX_C_SOURCE=200809 -D_DEFAULT_SOURCE -Wno-unused-result
CFLAGS = $(CFLAGS_$(CC))
LINKER_FLAGS_gcc = -lm -lpcre
LINKER_FLAGS = $(LINKER_FLAGS_$(CC))

vpath %.c src

.PHONY: all directories clean

all: directories $(BINARIES)

directories: $(BUILD_DIR)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%: %.c
	$(CC) -o $@ $(CFLAGS) $< $(LINKER_FLAGS)

clean:
	rm $(BINARIES) -f
