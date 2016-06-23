#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <ftw.h>

#include <sys/stat.h>
#include <sys/types.h>

#include "config.h"
#include "utils.h"

/*
 * Structure used to represent a node in the linked list containing the
 * paths to every file in the provided directory tree.
 */
typedef struct path_node {
	string path; /* The file's path */
	/*@null@*/ struct path_node *next; /* The node for the next file (NULL if last) */
} path_node;

/*@only@*/ /*@null@*/ static path_node *PATHS = NULL;

/*
 * Construct a node given a path.
 */
/*@only@*/ static /*@special@*/ path_node *make_node(const char *path)
	/*@releases result->next@*/
{
	path_node *ret = (path_node *) malloc(sizeof(path_node));
	if (ret == NULL) exit(EXIT_FAILURE);
	strncpy(ret->path, path, STRING_LENGTH);
	ret->next = NULL;
	return ret;
}

/*
 * Construct a node and push it to the front of PATHS given a path.
 */
static void push_path(const char *path)
{
	path_node *temp = make_node(path);
	temp->next = PATHS;
	PATHS = temp;
}

/*
 * Callback given to ftw. If the file being traversed is not a directory,
 * push its path to PATHS.
 */
static int walk_fn(const char *path, const struct stat *sb, /*@unused@*/ int typeflag)
{
	if (S_ISREG(sb->st_mode)) {
		push_path(path);
	}
	return 0;
}

int main(int argc, char **argv)
	/*@globals killed PATHS@*/
{
	string buffer;
	path_node *n, *temp;

	if (argc < 2) {
		fprintf(stderr, "Usage: %s <timestamp>\n", argv[0]);
		exit(EXIT_FAILURE);
	}

	snprintf(buffer, STRING_LENGTH, "%s/%s", WORKING_DIR, argv[1]);
	ftw(buffer, walk_fn, 20);
	snprintf(buffer, STRING_LENGTH, "%s/%s/%s", WORKING_DIR, argv[1], GLOBAL_FILE_NAME);
	for (n = PATHS; n != NULL; n = temp) {
		path_node *p;
		for (p = PATHS; p != NULL; p = p->next) {
			if (strcmp(n->path, p->path) != 0
					&& strcmp(n->path, buffer) != 0
					&& strcmp(p->path, buffer) != 0) {
				printf("%s\n%s\n", n->path, p->path);
			}
		}
		temp = n->next;
		free(PATHS);
		PATHS = temp;
	}

	if (PATHS != NULL) free(PATHS);

	return 0;
}
