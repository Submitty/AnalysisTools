#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#include <string.h>

#include <ftw.h>
#include <time.h>

#include <sys/stat.h>
#include <sys/types.h>

#include "config.h"

static char SPRINTF_BUFFER[1024];
int TIMESTAMP;

typedef enum filetype {
	FT_UNKNOWN = 0,
	FT_C,
	FT_PYTHON
} filetype;

char *LEXERS[] = {
	[FT_C] = "c",
	[FT_PYTHON] = "python",
};

char *get_extension(const char *path)
{
	char *ext = strrchr(path, '.');
	if (!ext) return NULL;
	else return ext + 1;
}

filetype determine_type(char *ext)
{
	if (strcmp(ext, "c") == 0
			|| strcmp(ext, "cpp") == 0
			|| strcmp(ext, "cc") == 0
			|| strcmp(ext, "C") == 0
			|| strcmp(ext, "h") == 0
			|| strcmp(ext, "hpp") == 0
			|| strcmp(ext, "H") == 0) {
		return FT_C;
	} else if (strcmp(ext, "py") == 0) {
		return FT_PYTHON;
	} else {
		return FT_UNKNOWN;
	}
}

int walk_fn(const char *path, const struct stat *sb, int typeflag)
{
	if (S_ISREG(sb->st_mode)) {
		filetype f = determine_type(get_extension(path));
		if (f != FT_UNKNOWN) {
			snprintf(SPRINTF_BUFFER, 1024, "cat '%s' | ./lexer/%s/lex | ./bin/winnow '%d' '%d' > " WORKING_DIR "/%d/%s",
					path, LEXERS[f], UPPER_BOUND, LOWER_BOUND, TIMESTAMP, path);
			system(SPRINTF_BUFFER);
		}
	} else {
		snprintf(SPRINTF_BUFFER, 1024, WORKING_DIR "/%d/%s", TIMESTAMP, path);
		mkdir(SPRINTF_BUFFER, 0777);
	}
	return 0;
}

int main(int argc, char **argv)
{
	TIMESTAMP = time(NULL);
	printf("%d\n", TIMESTAMP);
	mkdir(WORKING_DIR, 0777);
	snprintf(SPRINTF_BUFFER, 1024, WORKING_DIR "/%d", TIMESTAMP);
	mkdir(SPRINTF_BUFFER, 0777);
	ftw(argv[1], walk_fn, 8);
}
