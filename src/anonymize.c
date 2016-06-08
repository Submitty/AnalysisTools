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

typedef struct regexp_entry {
	pcre *re;
	char sub[1024];
	struct regexp_entry *next;
} regexp_entry;

/*
 * Linked lists containing names and regular expressions to replace.
 */
static name_entry *NAMES = NULL;
static regexp_entry *REGEXPS = NULL;

/*
 * Add a name and corresponding replacement string to NAMES.
 */
static void add_name(char *name, char *new)
{
	name_entry *n = (name_entry *) malloc(sizeof(name_entry));
	strncpy(n->name, name, 1024);
	strncpy(n->new, new, 1024);
	n->next = NAMES;
	NAMES = n;
}

/*
 * Compile a regular expression and add it to REGEXPS alongside its
 * replacement string.
 */
static void add_regexp(char *input)
{
	char regexp[1024];
	char sub[1024];
	sscanf(input, "s/%[^/]/%[^/]/", regexp, sub);
	const char *error;
	int erroroffset;
	pcre *re = pcre_compile(
			regexp,
			0,
			&error,
			&erroroffset,
			NULL);
	if (re == NULL) {
		fprintf(stderr,
				"Regular expression compilation failed at offset %d: %s\n",
				erroroffset, error);
		exit(1);
	} else {
		regexp_entry *r = (regexp_entry *) malloc(sizeof(regexp_entry));
		r->re = re;
		strncpy(r->sub, sub, 1024);
		r->next = REGEXPS;
		REGEXPS = r;
	}
}

static unsigned int hash(char *key)
{
	unsigned int h = 5381;
	for (unsigned int i = 0; i < strlen(key); ++i) {
		h = h * 33 + key[i];
	}
	return abs(h % FINGERPRINT_CACHE_SIZE);
}

static void scramble_name(char *name, char *new)
{
	// TODO: This should probably be user provided.
	snprintf(new, 1024, "REDACTED_%03u", hash(name));
}

static void apply_replace(char *buf, char *str, name_entry *entry)
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

void read_names(char *path, bool twocol)
{
	FILE *name_file = fopen(path, "r");
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
}

int main(int argc, char **argv)
{
	int arg;
	while ((arg = getopt(argc, argv, "t:n:r:")) != -1) {
		switch (arg) {
			case 'n':
				read_names(optarg, false);
				break;
			case 't':
				read_names(optarg, true);
				break;
			case 'r':

				add_regexp(optarg);
				break;
		}
	}

	char line[4096];
	int ovector[30];
	while (fgets(line, 4096, stdin) != NULL) {
		char buf[4096];
		for (name_entry *n = NAMES; n != NULL; n = n->next) {
			apply_replace(buf, line, n);
			memcpy(line, buf, 4096);
		}
		for (regexp_entry *r = REGEXPS; r != NULL; r = r->next) {
			int rc;
			while ((rc = pcre_exec(r->re, NULL, line, strlen(line), 0, 0, ovector, 30)) > 0) {
				memset(buf, 0, 4096);
				memcpy(buf, line, ovector[0]);
				int sublen = strlen(r->sub);
				memcpy(buf + ovector[0], r->sub, sublen);
				memcpy(buf + ovector[0] + sublen, line + ovector[1], 4095 - (ovector[0] + sublen));
				memcpy(line, buf, 4096);
			}
		}
		printf("%s", line);
	}
}
