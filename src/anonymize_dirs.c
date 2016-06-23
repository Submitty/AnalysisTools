#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <errno.h>

#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <ctype.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "config.h"
#include "utils.h"


typedef struct name_entry {
	string name;
	string new;
	struct name_entry *next;
} name_entry;

/*@only@*/ /*@null@*/ static char **COMMAND = NULL;

static string DIR_STACK[64];
static unsigned int DIR_STACK_INDEX = 0;
static unsigned int REPLACE_LEVEL = 0;

static name_entry *NAMES[256];

static unsigned int hash(const char *key)
{
	unsigned int h = 5381;
	unsigned int i;
	for (i = 0; i < (unsigned int) strlen(key); ++i) {
		h = h * 33 + (unsigned int) key[i];
	}
	return h % FINGERPRINT_CACHE_SIZE;
}

static void add_name(const char *name, const char *new)
{
	name_entry *n;
	unsigned int h;

	n = (name_entry *) malloc(sizeof(name_entry));
	if (n == NULL) exit(EXIT_FAILURE);

	strncpy(n->name, name, STRING_LENGTH);
	strncpy(n->new, new, STRING_LENGTH);
	h = hash(n->name);
	n->next = NAMES[h];
	NAMES[h] = n;
}

static inline void make_lowercase(char *b)
{
	while (*b != (char) 0x0) { *b = tolower(*b); b++; }
}

static void read_names(const char *path)
{
	FILE *name_file = fopen(path, "r");
	string name, new;
	while (fscanf(name_file, " %[^,],%s ", name, new) == 2) {
		make_lowercase(name);
		add_name(name, new);
	}
}

static void construct_path(char *buf, unsigned int bufsize, bool sub)
{
	string component;
	unsigned int i;
	memset(buf, 0, (size_t) bufsize);
	for (i = 0; i < DIR_STACK_INDEX; ++i) {
		if (sub && i+1 == REPLACE_LEVEL) {
			bool rep = false;
			name_entry *n;

			strncpy(component, DIR_STACK[i], STRING_LENGTH);
			make_lowercase(component);

			for (n = NAMES[hash(component)]; n != NULL; n = n->next) {
				if (strncmp(n->name, component, STRING_LENGTH) == 0) {
					strncat(buf, n->new, (size_t) bufsize);
					rep = true;
					break;
				}
			}
			if (!rep) strncat(buf, DIR_STACK[i], STRING_LENGTH);
		} else {
			strncat(buf, DIR_STACK[i], (size_t) bufsize);
		}
		strncat(buf, "/", (size_t) bufsize);
	}
}

static void walk(const char *path, int (*cb)(const char *, bool))
{
	const char *last = strrchr(path, '/');
	string complete_path, stat_path;
	DIR *dir;
	struct dirent *ent;
	struct stat statbuf;

	strncpy(DIR_STACK[DIR_STACK_INDEX++], (last == NULL) ? path : last, STRING_LENGTH);
	cb(NULL, false);

	construct_path(complete_path, STRING_LENGTH, false);
	//printf("+%s | %s\n", last, complete_path);

	dir = opendir(complete_path);
	while ((ent = readdir(dir))) {
		snprintf(stat_path, STRING_LENGTH, "%s%s", complete_path, ent->d_name);
		stat(stat_path, &statbuf);
		if (S_ISDIR(statbuf.st_mode) && ent->d_name[0] != '.') {
			walk(ent->d_name, cb);
		} else if (S_ISREG(statbuf.st_mode)) {
			cb(ent->d_name, true);
		}
	}

	--DIR_STACK_INDEX;
	closedir(dir);
}

/*
 * For the given path, if that path is a directory, create a corresponding
 * directory in WORKING_DIR/anonymized.  If it is a regular file, pipe it
 * to ./bin/anonymize INPUT_PATH (where INPUT_PATH is a CLI argument
 * containing names to replace). If TWOCOL is set (again a CLI flag), "-t"
 * is also passed as a flag, telling ./bin/anonymize to parse a two-column
 * CSV file rather than a simple list of names.
 */
static int walk_fn(const char *path, bool isreg)
{
	string fake_path, real_path, buf, new_name;
	if (isreg) { /* If this is a regular file: */
		bool rep = false;
		char *base;
		name_entry *n;
		int input, output;

		strncpy(new_name, path, STRING_LENGTH);
		strncpy(buf, path, STRING_LENGTH);
		base = strtok(buf, ".");
		make_lowercase(base);
		fprintf(stderr, "%s %s\n", base, new_name);
		for (n = NAMES[hash(base)]; n != NULL; n = n->next) {
			if (strncmp(n->name, base, STRING_LENGTH) == 0) {
				strncpy(new_name, n->new, STRING_LENGTH);
				strcat(new_name, ".");
				strcat(new_name, buf + strlen(base) + 1);
				rep = true;
				break;
			}
		}
		if (!rep) strncpy(new_name, path, STRING_LENGTH);
		fprintf(stderr, "%s\n", new_name);

		construct_path(buf, STRING_LENGTH, true);
		snprintf(fake_path, STRING_LENGTH, WORKING_DIR "/anonymized/%s%s", buf, new_name);
		construct_path(buf, STRING_LENGTH, false);
		snprintf(real_path, STRING_LENGTH, "%s%s", buf, path);
		/* Open the input file */
		input = open(real_path, O_RDONLY);
		if (input < 0) { perror(real_path); exit(EXIT_FAILURE); }

		/* Open the output file in WORKING_DIR/anonymized */
		output = creat(fake_path, 0644);
		if (output < 0) { perror(fake_path); exit(EXIT_FAILURE); }

		/* Run ./bin/anonymize with the appropriate arguments */
		execute(input, output, -1, COMMAND);

		/* Clean child processes */
		while (wait(NULL) > 0);
	} else { /* If this is a directory, create it */
		construct_path(buf, STRING_LENGTH, true);
		snprintf(fake_path, STRING_LENGTH, WORKING_DIR "/anonymized/%s", buf);
		mkdir(fake_path, 0777);
	}
	return 0;
}

int main(int argc, char **argv)
	/*@globals killed undef COMMAND;@*/
{
	int arg;
	string buf = WORKING_DIR "/anonymized";
	string swap;
	const char *d;

	COMMAND = (char **) malloc(argc * sizeof(char *));
	if (COMMAND == NULL) exit(EXIT_FAILURE);
	memcpy(COMMAND+1, argv+2, (argc - 2) * sizeof(char *));
	COMMAND[0] = "./bin/anonymize";
	COMMAND[argc-1] = NULL;

	while ((arg = getopt(argc-1, COMMAND, "t:n:r:l:")) != -1) {
		switch (arg) {
			case 't':
				read_names(optarg);
				break;
			case 'n':
			case 'r':
				break;
			case 'l':
				REPLACE_LEVEL = (unsigned int) atoi(optarg) + 1;
				break;
		}
	}
	mkdir(WORKING_DIR, 0777);
	mkdir(WORKING_DIR "/anonymized", 0777);

	d = strtok(argv[1], "/");
	do {
		snprintf(swap, STRING_LENGTH, "%s/%s", buf, d);
		mkdir(swap, 0777);
		strncpy(buf, swap, STRING_LENGTH);
	} while ((d = strtok(NULL, "/")) != NULL);

	walk(argv[1], walk_fn);
	free(COMMAND);

	return 0;
}
