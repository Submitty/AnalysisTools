#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <ftw.h>

#include <sys/stat.h>
#include <sys/types.h>

#include "config.h"

typedef struct path_node {
	char path[1024];
	struct path_node *next;
} path_node;

static path_node *PATHS = NULL;

path_node *make_node(const char *path)
{
	path_node *ret = (path_node *) malloc(sizeof(path_node));
	strncpy(ret->path, path, 1024);
	ret->next = NULL;
	return ret;
}

void push_path(const char *path)
{
	path_node *temp = make_node(path);
	temp->next = PATHS;
	PATHS = temp;
}

int walk_fn(const char *path, const struct stat *sb, int typeflag)
{
	if (S_ISREG(sb->st_mode)) {
		push_path(path);
	}
	return 0;
}

int main(int argc, char **argv)
{
	ftw(argv[1], walk_fn, 8);
	for (path_node *n = PATHS; n != NULL; n = n->next) {
		for (path_node *p = PATHS; p != NULL; p = p->next) {
			if (strcmp(n->path, p->path)) {
				printf("%s\n%s\n", n->path, p->path);
			}
		}
		free(PATHS);
		PATHS = n->next;
	}
}
