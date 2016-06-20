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
	char name[1024];
	char new[1024];
	struct name_entry *next;
} name_entry;

static char **COMMAND;

static char DIR_STACK[64][1024];
static unsigned int DIR_STACK_INDEX = 0;
static int REPLACE_LEVEL = 0;

static name_entry *NAMES[256] = {NULL};

static unsigned int hash(char *key)
{
	unsigned int h = 5381;
	for (unsigned int i = 0; i < strlen(key); ++i) {
		h = h * 33 + key[i];
	}
	return abs(h % FINGERPRINT_CACHE_SIZE);
}

static void add_name(char *name, char *new)
{
	name_entry *n = (name_entry *) malloc(sizeof(name_entry));
	strncpy(n->name, name, 128);
	strncpy(n->new, new, 128);
	unsigned int h = hash(n->name);
	n->next = NAMES[h];
	NAMES[h] = n;
}

static inline void make_lowercase(char *b)
{
	while (*b) { *b = tolower(*b); b++; }
}

static void read_names(char *path)
{
	FILE *name_file = fopen(path, "r");
	char name[128], new[128];
	while (fscanf(name_file, " %[^,],%s ", name, new) == 2) {
		make_lowercase(name);
		add_name(name, new);
	}
}

static void construct_path(char *buf, unsigned int bufsize, bool sub)
{
	char component[1024];
	memset(buf, 0, bufsize);
	for (int i = 0; i < DIR_STACK_INDEX; ++i) {
		if (sub && i+1 == REPLACE_LEVEL) {
			strncpy(component, DIR_STACK[i], 1024);
			make_lowercase(component);
			bool rep = false;
			for (name_entry *n = NAMES[hash(component)]; n != NULL; n = n->next) {
				if (strncmp(n->name, component, 1024) == 0) {
					strncat(buf, n->new, bufsize);
					rep = true;
					break;
				}
			}
			if (!rep) strncat(buf, DIR_STACK[i], 1024);
		} else {
			strncat(buf, DIR_STACK[i], bufsize);
		}
		strncat(buf, "/", bufsize);
	}
}

static void walk(const char *path, int (*cb)(const char *, bool))
{
	char *last = strrchr(path, '/');
	strncpy(DIR_STACK[DIR_STACK_INDEX++], (last == NULL) ? path : last, 1024);
	cb(NULL, false);

	char complete_path[1024];
	construct_path(complete_path, 1024, false);
	//printf("+%s | %s\n", last, complete_path);

	DIR *dir = opendir(complete_path);
	struct dirent *ent;
	struct stat statbuf;
	char stat_path[1024];
	while ((ent = readdir(dir))) {
		snprintf(stat_path, 1024, "%s%s", complete_path, ent->d_name);
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
	char fake_path[1024], real_path[1024], buf[1024];
	if (isreg) { /* If this is a regular file: */
		construct_path(buf, 1024, true);
		snprintf(fake_path, 1024, WORKING_DIR "/anonymized/%s%s", buf, path);
		construct_path(buf, 1024, false);
		snprintf(real_path, 1024, "%s%s", buf, path);
		/* Open the input file */
		int input = open(real_path, O_RDONLY);
		if (input < 0) { perror(real_path); exit(1); }

		/* Open the output file in WORKING_DIR/anonymized */
		int output = creat(fake_path, 0644);
		if (output < 0) { perror(fake_path); exit(1); }

		/* Run ./bin/anonymize with the appropriate arguments */
		execute(input, output, -1, COMMAND);

		/* Clean child processes */
		while (wait(NULL) > 0);
	} else { /* If this is a directory, create it */
		construct_path(buf, 1024, true);
		snprintf(fake_path, 1024, WORKING_DIR "/anonymized/%s", buf);
		mkdir(fake_path, 0777);
	}
	return 0;
}

int main(int argc, char **argv)
{

	COMMAND = (char **) malloc(argc * sizeof(char *));
	memcpy(COMMAND+1, argv+2, (argc - 2) * sizeof(char *));
	COMMAND[0] = "./bin/anonymize";
	COMMAND[argc-1] = NULL;

	int arg;
	while ((arg = getopt(argc-1, COMMAND, "t:n:r:l:")) != -1) {
		switch (arg) {
			case 't':
				read_names(optarg);
				break;
			case 'n':
			case 'r':
				break;
			case 'l':
				REPLACE_LEVEL = atoi(optarg) + 1;
				break;
		}
	}

	mkdir(WORKING_DIR, 0777);
	mkdir(WORKING_DIR "/anonymized", 0777);
	walk(argv[1], walk_fn);
	free(COMMAND);
}
