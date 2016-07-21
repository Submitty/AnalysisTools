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

typedef struct ignore_entry {
	string name;
	struct ignore_entry *next;
} ignore_entry;

static char **COMMAND = NULL;
static int ARGC;

static string DIR_STACK[64];
static unsigned int DIR_STACK_INDEX = 0;
static bool ABSOLUTE_PATH = false;
static unsigned int REPLACE_LEVEL = 0;
static unsigned int STARTING_LEVEL;

static name_entry *NAMES[1024];
static ignore_entry *IGNORED[1024];

static unsigned int hash(const char *key)
{
	unsigned int h = 5381;
	unsigned int i;
	for (i = 0; i < (unsigned int)strlen(key); ++i) {
		h = h * 33 + (unsigned int)key[i];
	}
	return h % 1024;
}

static void add_name(const char *name, const char *new)
{
	name_entry *n;
	unsigned int h;

	n = (name_entry *) malloc(sizeof(name_entry));
	if (n == NULL)
		exit(EXIT_FAILURE);

	strncpy(n->name, name, STRING_LENGTH);
	strncpy(n->new, new, STRING_LENGTH);
	h = hash(n->name);
	n->next = NAMES[h];
	NAMES[h] = n;
}

static void add_ignored(const char *name)
{
	ignore_entry *n;
	unsigned int h;

	n = (ignore_entry *) malloc(sizeof(ignore_entry));
	if (n == NULL)
		exit(EXIT_FAILURE);

	strncpy(n->name, name, STRING_LENGTH);

	h = hash(n->name);
	n->next = IGNORED[h];
	IGNORED[h] = n;
}

static bool should_ignore(const char *name)
{
	ignore_entry *n;
	for (n = IGNORED[hash(name)]; n != NULL; n = n->next) {
		if (strncmp(n->name, name, STRING_LENGTH) == 0) {
			return true;
		}
	}
	return false;
}

static inline void make_lowercase(char *b)
{
	while (*b != (char)0x0) {
		*b = tolower(*b);
		b++;
	}
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
	bool anyrep = false;
	memset(buf, 0, (size_t) bufsize);
	if (!sub && ABSOLUTE_PATH)
		buf[0] = '/';
	for (i = 0; i < DIR_STACK_INDEX; ++i) {
		if (sub && strncmp(DIR_STACK[i], "..", STRING_LENGTH) == 0)
			continue;

		if (sub && i == (REPLACE_LEVEL + STARTING_LEVEL)) {
			bool rep = false;
			name_entry *n;

			strncpy(component, DIR_STACK[i], STRING_LENGTH);
			make_lowercase(component);

			for (n = NAMES[hash(component)]; n != NULL; n = n->next) {
				if (strncmp(n->name, component, STRING_LENGTH)
				    == 0) {
					strncat(buf, n->new, (size_t) bufsize);
					rep = true;
					anyrep = true;
					break;
				}
			}
			if (!rep)
				strncat(buf, DIR_STACK[i], STRING_LENGTH);
		} else {
			strncat(buf, DIR_STACK[i], (size_t) bufsize);
		}
		strncat(buf, "/", (size_t) bufsize);
	}
	if (anyrep)
		fprintf(stderr, "Applied substitution in path %s\n", buf);
}

static void apply_filename_replace(char *buf, char *str)
{
	char *tok = strtok(str, "_");
	bool first = true;
	memset(buf, 0, STRING_LENGTH);
	do {
		string lowercase;
		name_entry *n;
		bool rep = false;

		if (first) {
			first = false;
		} else {
			strcat(buf, "_");
		}

		strncpy(lowercase, tok, STRING_LENGTH);
		make_lowercase(lowercase);
		for (n = NAMES[hash(lowercase)]; n != NULL; n = n->next) {
			if (strncmp(n->name, lowercase, STRING_LENGTH) == 0) {
				fprintf(stderr,
					"Applied substitution in filename: %s for %s\n",
					n->new, lowercase);
				strcat(buf, n->new);
				rep = true;
				break;
			}
		}
		if (!rep)
			strcat(buf, tok);
	} while ((tok = strtok(NULL, "_")) != NULL);
}

static void walk(const char *path, void (*cb) (const char *, bool))
{
	const char *last = strrchr(path, '/');
	string complete_path = "", stat_path = "";
	DIR *dir;
	struct dirent *ent;
	struct stat statbuf;
	memset(&statbuf, 0, sizeof(statbuf));

	strncpy(DIR_STACK[DIR_STACK_INDEX++], (last == NULL) ? path : last,
		STRING_LENGTH);
	cb(NULL, false);

	construct_path(complete_path, STRING_LENGTH, false);

	if (should_ignore(complete_path)) {
		fprintf(stderr, "Ignored file %s\n", complete_path);
		return;
	}

	dir = opendir(complete_path);
	while ((ent = readdir(dir))) {
		snprintf(stat_path, STRING_LENGTH, "%s%s", complete_path,
			 ent->d_name);
		stat(stat_path, &statbuf);
		if (S_ISDIR(statbuf.st_mode) && (strcmp(ent->d_name, ".") != 0)
		    && (strcmp(ent->d_name, "..") != 0)) {
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
static void walk_fn(const char *path, bool isreg)
{
	string fake_path = "", real_path = "", buf = "", new_name =
	    "", strhash = "";
	if (isreg) {		/* If this is a regular file: */
		char *base, *ext;
		int input, output;

		construct_path(buf, STRING_LENGTH, false);
		snprintf(real_path, STRING_LENGTH, "%s%s", buf, path);

		if (should_ignore(real_path)) {
			fprintf(stderr, "Ignored file %s\n", real_path);
			return;
		}

		strncpy(new_name, path, STRING_LENGTH);
		strncpy(buf, path, STRING_LENGTH);
		base = strtok(buf, ".");
		ext = buf + strlen(base) + 1;
		apply_filename_replace(new_name, base);
		if (strlen(ext) != 0) {
			strcat(new_name, ".");
			strcat(new_name, ext);
		}

		construct_path(buf, STRING_LENGTH, true);
		snprintf(fake_path, STRING_LENGTH,
			 WORKING_DIR "/anonymized/%s%s", buf, new_name);

		/* Open the input file */
		input = open(real_path, O_RDONLY);
		if (input < 0) {
			perror(real_path);
			exit(EXIT_FAILURE);
		}

		/* Open the output file in WORKING_DIR/anonymized */
		output = creat(fake_path, 0644);
		if (output < 0) {
			perror(fake_path);
			exit(EXIT_FAILURE);
		}

		if (DIR_STACK_INDEX > (REPLACE_LEVEL + STARTING_LEVEL)) {
			COMMAND[ARGC - 1] = "-h";
			construct_path(buf, STRING_LENGTH, false);
			snprintf(strhash, STRING_LENGTH, "%u", hash(buf));
			COMMAND[ARGC] = strhash;
			COMMAND[ARGC + 1] = NULL;
		} else {
			COMMAND[ARGC - 1] = NULL;
		}

		/* Run ./bin/anonymize with the appropriate arguments */
		execute(input, output, -1, COMMAND);

		/* Clean child processes */
		while (wait(NULL) > 0) ;
	} else {		/* If this is a directory, create it */
		construct_path(buf, STRING_LENGTH, true);
		snprintf(fake_path, STRING_LENGTH, WORKING_DIR "/anonymized/%s",
			 buf);
		mkdir(fake_path, 0777);
	}
	return;
}

int main(int argc, char **argv)
{
	int arg;
	string buf = WORKING_DIR "/anonymized";
	string swap;
	const char *d;

	COMMAND = (char **)malloc((argc + 2) * sizeof(char *));
	if (COMMAND == NULL)
		exit(EXIT_FAILURE);
	memcpy(COMMAND + 1, argv + 2, (argc - 2) * sizeof(char *));
	COMMAND[0] = "./bin/anonymize";
	COMMAND[argc - 1] = NULL;
	ARGC = argc;

	while ((arg = getopt(argc - 1, COMMAND, "t:n:r:a:l:")) != -1) {
		switch (arg) {
		case 't':
			read_names(optarg);
			break;
		case 'n':
		case 'r':
		case 'a':
			break;
		case 'l':
			REPLACE_LEVEL = (unsigned int)atoi(optarg) + 1;
			break;
		}
	}

	for (++optind; optind < argc; ++optind) {
		add_ignored(argv[optind]);
	}

	mkdir(WORKING_DIR, 0777);
	mkdir(WORKING_DIR "/anonymized", 0777);

	if (argv[1][0] == '/')
		ABSOLUTE_PATH = true;

	d = strtok(argv[1], "/");
	do {
		if (strncmp(d, "..", STRING_LENGTH) != 0) {
			snprintf(swap, STRING_LENGTH, "%s/%s", buf, d);
			mkdir(swap, 0777);
			strncpy(buf, swap, STRING_LENGTH);
		}
		strncpy(DIR_STACK[DIR_STACK_INDEX++], d, STRING_LENGTH);
	} while ((d = strtok(NULL, "/")) != NULL);
	DIR_STACK_INDEX--;

	STARTING_LEVEL = DIR_STACK_INDEX;

	walk(DIR_STACK[DIR_STACK_INDEX], walk_fn);
	free(COMMAND);

	return 0;
}
