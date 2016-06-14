#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <pcre.h>

#include <ctype.h>

#include "config.h"
#include "utils.h"

typedef struct name_entry {
	char name[128];
	char new[128];
	struct name_entry *next;
} name_entry;

typedef struct regexp_entry {
	pcre *re;
	char sub[128];
	struct regexp_entry *next;
} regexp_entry;

/*
 * Hash table containing names to replace.
 */
static name_entry *NAMES[256] = {NULL};

/*
 * Linked list containing regular expressions to replace.
 */
static regexp_entry *REGEXPS = NULL;

static unsigned int hash(char *key)
{
	unsigned int h = 5381;
	for (unsigned int i = 0; i < strlen(key); ++i) {
		h = h * 33 + key[i];
	}
	return abs(h % FINGERPRINT_CACHE_SIZE);
}

/*
 * Add a name and corresponding replacement string to NAMES.
 */
static void add_name(char *name, char *new)
{
	name_entry *n = (name_entry *) malloc(sizeof(name_entry));
	strncpy(n->name, name, 128);
	strncpy(n->new, new, 128);
	unsigned int h = hash(n->name);
	n->next = NAMES[h];
	NAMES[h] = n;
}

/*
 * Compile a regular expression and add it to REGEXPS alongside its
 * replacement string.
 */
static void add_regexp(char *input)
{
	char regexp[128];
	char sub[128];
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
		strncpy(r->sub, sub, 128);
		r->next = REGEXPS;
		REGEXPS = r;
	}
}

static void scramble_name(char *name, char *new)
{
	snprintf(new, 128, "REDACTED_%03u", hash(name));
}

static void apply_replace(char *buf, char *str, name_entry *entry)
{
	memset(buf, 0, 128);
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

static inline void make_lowercase(char *b)
{
	while (*b) { *b = tolower(*b); b++; }
}

static void read_names(char *path, bool twocol)
{
	FILE *name_file = fopen(path, "r");
	if (twocol) {
		char name[128], new[128];
		while (fscanf(name_file, " %[^,],%s ", name, new) == 2) {
			make_lowercase(name);
			add_name(name, new);
		}
	} else {
		char name[128], new[128];
		while (fscanf(name_file, "  %s  ", name) == 1) {
			make_lowercase(name);
			scramble_name(name, new);
			add_name(name, new);
		}
	}
}

static char next_word(char *buf, int size)
{
	int c = getchar();
	while (c != EOF && isalnum(c) && size > 0) {
		*(buf++) = c;
		--size;
		c = getchar();
	}
	*buf = 0;
	return c == EOF ? 0 : c;
}

int main(int argc, char **argv)
{
	int arg;
	while ((arg = getopt(argc, argv, "t:n:r:l:")) != -1) {
		switch (arg) {
			case 't':
				read_names(optarg, true);
				break;
			case 'n':
				read_names(optarg, false);
				break;
			case 'r':
				add_regexp(optarg);
				break;
			case 'l':
				break;
		}
	}

	char word[128];
	char buf[128];
	char lower[128];
	int ovector[30];
	char delim;
	while ((delim = next_word(word, 128)) != 0) {
		memcpy(lower, word, 128);
		make_lowercase(lower);
		for (name_entry *n = NAMES[hash(lower)]; n != NULL; n = n->next) {
			apply_replace(buf, lower, n);
			memcpy(word, buf, 128);
		}
		for (regexp_entry *r = REGEXPS; r != NULL; r = r->next) {
			int rc;
			while ((rc = pcre_exec(r->re, NULL, word, strlen(word), 0, 0, ovector, 30)) > 0) {
				memset(buf, 0, 128);
				memcpy(buf, word, ovector[0]);
				int sublen = strlen(r->sub);
				memcpy(buf + ovector[0], r->sub, sublen);
				memcpy(buf + ovector[0] + sublen, word + ovector[1], 127 - (ovector[0] + sublen));
				memcpy(word, buf, 128);
			}
		}
		printf("%s%c", word, delim);
	}
}
