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
	string name;
	string new;
	struct name_entry *next;
} name_entry;

typedef struct regexp_entry {
	pcre *re;
	string sub;
	struct regexp_entry *next;
} regexp_entry;

/*
 * Hash table containing names to replace.
 */
static name_entry *NAMES[256];

/*
 * Linked list containing regular expressions to replace.
 */
static regexp_entry *REGEXPS = NULL;

static unsigned int hash(const char *key)
{
	unsigned int h = 5381;
	unsigned int i;
	for (i = 0; i < (unsigned int)strlen(key); ++i) {
		h = h * 33 + (unsigned int)key[i];
	}
	return h % 256;
}

/*
 * Add a name and corresponding replacement string to NAMES.
 */
static void add_name(const char *name, const char *new)
{
	unsigned int h;
	name_entry *n = (name_entry *) malloc(sizeof(name_entry));

	if (n == NULL)
		exit(EXIT_FAILURE);
	strncpy(n->name, name, STRING_LENGTH);
	strncpy(n->new, new, STRING_LENGTH);
	h = hash(n->name);
	n->next = NAMES[h];
	NAMES[h] = n;
}

/*
 * Compile a regular expression and add it to REGEXPS alongside its
 * replacement string.
 */
static void add_regexp(const char *input)
{
	string regexp;
	string sub;
	const char *error = NULL;
	int erroroffset = 0;
	pcre *re;

	sscanf(input, "s/%[^/]/%[^/]/", regexp, sub);
	re = pcre_compile(regexp, 0, &error, &erroroffset, NULL);
	if (re == NULL) {
		fprintf(stderr,
			"--- Regular expression compilation failed at offset %d: %s\n",
			erroroffset, error);
		exit(EXIT_FAILURE);
	} else {
		regexp_entry *r = (regexp_entry *) malloc(sizeof(regexp_entry));
		if (r == NULL)
			exit(EXIT_FAILURE);
		r->re = re;
		strncpy(r->sub, sub, STRING_LENGTH);
		r->next = REGEXPS;
		REGEXPS = r;
	}
}

static void scramble_name(const char *name, char *new)
{
	snprintf(new, STRING_LENGTH, REDACTION_PATTERN, hash(name));
}

static void apply_replace(char *buf, const char *str, name_entry * entry)
{
	unsigned int len = (unsigned int)strlen(str);
	unsigned int name_len = (unsigned int)strlen(entry->name);
	unsigned int new_len = (unsigned int)strlen(entry->new);
	unsigned int i;

	memset(buf, 0, STRING_LENGTH);
	for (i = 0; i < len; ++i) {
		if (strncmp(str + i, entry->name, (size_t) name_len) == 0) {
			strncat(buf, entry->new, (size_t) new_len);
			i += name_len - 1;
		} else {
			char *pos = strchr(buf, 0);
			if (pos != NULL) {
				*pos = str[i];
			}
		}
	}
}

static inline void make_lowercase(char *b)
{
	while (*b != (char)0x0) {
		*b = tolower(*b);
		b++;
	}
}

static void read_names(const char *path, bool twocol)
{
	FILE *name_file = fopen(path, "r");
	if (twocol) {
		string name, new;
		while (fscanf(name_file, " %[^,],%s ", name, new) == 2) {
			make_lowercase(name);
			add_name(name, new);
		}
	} else {
		string name, new;
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
		*(buf++) = (char)c;
		--size;
		c = getchar();
	}
	*buf = (char)0x0;
	return (char)(c == EOF ? 0x0 : c);
}

int main(int argc, char **argv)
{
	static int arg;
	static string word, buf, lower;
	static int ovector[30];
	static char delim;

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

	while ((delim = next_word(word, STRING_LENGTH)) != (char)0x0) {
		name_entry *n;
		regexp_entry *r;

		memcpy(lower, word, STRING_LENGTH);
		make_lowercase(lower);

		for (n = NAMES[hash(lower)]; n != NULL; n = n->next) {
			if (strncmp(lower, n->name, STRING_LENGTH) == 0) {
				fprintf(stderr, "Replaced %s with %s\n", word,
					n->new);
				apply_replace(buf, lower, n);
				memcpy(word, buf, STRING_LENGTH);
			}
		}
		for (r = REGEXPS; r != NULL; r = r->next) {
			int rc;
			while ((rc =
				pcre_exec(r->re, NULL, word, (int)strlen(word),
					  0, 0, ovector, 30)) > 0) {
				unsigned int sublen =
				    (unsigned int)strlen(r->sub);

				memset(buf, 0, STRING_LENGTH);
				memcpy(buf, word, (size_t) ovector[0]);
				memcpy(buf + ovector[0], r->sub,
				       (size_t) sublen);
				memcpy(buf + ovector[0] + sublen,
				       word + ovector[1],
				       (size_t) (STRING_LENGTH - 1 -
						 (ovector[0] + sublen)));
				memcpy(word, buf, STRING_LENGTH);
			}
		}
		printf("%s%c", word, delim);
	}

	return 0;
}
