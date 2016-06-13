#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <ftw.h>

#include <sys/stat.h>
#include <sys/types.h>

#include "config.h"

/*
 * Structure used to represent a node in the linked list containing the
 * paths to every file in the provided directory tree.
 */
typedef struct path_node {
	char path[1024]; /* The file's path */
	struct path_node *next; /* The node for the next file (NULL if last) */
} path_node;

static path_node *PATHS = NULL;

/*
 * Construct a node given a path.
 */
path_node *make_node(const char *path)
{
	path_node *ret = (path_node *) malloc(sizeof(path_node));
	strncpy(ret->path, path, 1024);
	ret->next = NULL;
	return ret;
}

/*
 * Construct a node and push it to the front of PATHS given a path.
 */
void push_path(const char *path)
{
	path_node *temp = make_node(path);
	temp->next = PATHS;
	PATHS = temp;
}

/*
 * Callback given to ftw. If the file being traversed is not a directory,
 * push its path to PATHS.
 */
int walk_fn(const char *path, const struct stat *sb, int typeflag)
{
	if (S_ISREG(sb->st_mode)) {
		push_path(path);
	}
	return 0;
}

int main(int argc, char **argv)
{
	if (argc < 2) {
		fprintf(stderr, "Usage: %s <timestamp>\n", argv[0]);
		exit(1);
	}

	char buffer[1024];
	snprintf(buffer, 1024, "%s/%s", WORKING_DIR, argv[1]);
	ftw(buffer, walk_fn, 20);
	snprintf(buffer, 1024, "%s/%s/%s", WORKING_DIR, argv[1], GLOBAL_FILE_NAME);
	for (path_node *n = PATHS; n != NULL; n = n->next) {
		for (path_node *p = PATHS; p != NULL; p = p->next) {
			if (strcmp(n->path, p->path)
					&& strcmp(n->path, buffer)
					&& strcmp(p->path, buffer)) {
				printf("%s\n%s\n", n->path, p->path);
			}
		}
		free(PATHS);
		PATHS = n->next;
	}
}
