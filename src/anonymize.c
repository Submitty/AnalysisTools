#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <pcre.h>

#include "config.h"
#include "utils.h"

typedef struct name_entry {
	char name[1024];
	char new[1024];
	struct name_entry *next;
} name_entry;

name_entry *NAMES = NULL;

char *RIN_PATTERN = "(66[0-9]{7})";

void add_name(char *name, char *new)
{
	name_entry *n = (name_entry *) malloc(sizeof(name_entry));
	strncpy(n->name, name, 1024);
	strncpy(n->new, new, 1024);
	n->next = NAMES;
	NAMES = n;
}

unsigned int hash(char *key)
{
	unsigned int h = 5381;
	for (unsigned int i = 0; i < strlen(key); ++i) {
		h = h * 33 + key[i];
	}
	return abs(h % FINGERPRINT_CACHE_SIZE);
}

void scramble_name(char *name, char *new)
{
	snprintf(new, 1024, "REDACTED_%3u", hash(name));
}

void apply_replace(char *buf, char *str, name_entry *entry)
{
	memset(buf, 0, 1024);
	int len = strlen(str);
	int name_len = strlen(entry->name);
	int new_len = strlen(entry->new);
	for (int i = 0; i < len; ++i) {
		if (strncmp(str + i, entry->name, name_len) == 0) {
			strncat(buf, entry->new, new_len);
			i += name_len - 1;
		} else {
			*(strchr(buf, 0)) = str[i];
		}
	}
}

int main(int argc, char **argv)
{
	bool twocol = false;

	int arg;
	while ((arg = getopt(argc, argv, "t")) != -1) {
		switch (arg) {
			case 't':
				twocol = true;
				break;
		}
	}

	FILE *name_file;
	if (optind < argc) {
		name_file = fopen(argv[optind], "r");
	} else {
		exit(1);
	}
	if (twocol) {
		char name[1024], new[1024];
		while (fscanf(name_file, " %[^,],%s ", name, new) == 2) {
			add_name(name, new);
		}
	} else {
		char name[1024], new[1024];
		while (fscanf(name_file, "  %s  ", name) == 1) {
			scramble_name(name, new);
			add_name(name, new);
		}
	}

	char line[4096];
	const char *error;
	int erroroffset;
	pcre *rin_re = pcre_compile(
			RIN_PATTERN,
			0,
			&error,
			&erroroffset,
			NULL);
	if (rin_re == NULL)
	{
		fprintf(stderr, "PCRE2 compilation failed at offset %d: %s\n", erroroffset, error);
		return 1;
	}
	int ovector[30];
	while (fgets(line, 4096, stdin) != NULL) {
		char buf[4096];
		for (name_entry *n = NAMES; n != NULL; n = n->next) {
			apply_replace(buf, line, n);
			memcpy(line, buf, 4096);
		}
		int rc;
		while ((rc = pcre_exec(rin_re, NULL, line, strlen(line), 0, 0, ovector, 30)) > 0) {
			char *start = line + ovector[0];
			char buffer[10] = {0};
			memcpy(buffer, start, 9);
			snprintf(buffer, 10, "%09d", atoi(buffer) % 523);
			memcpy(start, buffer, 9);
		}
		printf("%s", line);
	}
}
