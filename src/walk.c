#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <errno.h>

#include <unistd.h>
#include <fcntl.h>
#include <ftw.h>
#include <time.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "config.h"
#include "utils.h"

typedef enum filetype {
	FT_UNKNOWN = 0,
	FT_C,
	FT_PYTHON,
	FT_JAVA,
} filetype;

/*
 * Array associating internal filetypes with names. These are the same
 * names used as directory names in the lexer directory.
 */
static char *LEXERS[] = {
	[FT_C] = "c",
	[FT_PYTHON] = "python",
	[FT_JAVA] = "java",
};

static char SPRINTF_BUFFER[1024];
static unsigned int TIMESTAMP;

static unsigned int GLOBAL_FINGERPRINTS[HASH_BOUND] = {0};
static unsigned int LOCAL_FINGERPRINTS[HASH_BOUND] = {0};
static unsigned int NUM_FILES = 0;

/*
 * Give a path, obtain the file extension.
 */
char *get_extension(const char *path)
{
	char *ext = strrchr(path, '.');
	if (!ext) return NULL;
	else return ext + 1;
}

/*
 * Given a path, determine its filetype. Currently this is done by simply
 * checking the extension; a more robust is desirable.
 */
filetype determine_type(const char *path)
{
	char *ext = get_extension(path);
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
	} else if (strcmp(ext, "java") == 0) {
		return FT_JAVA;
	} else {
		return FT_UNKNOWN;
	}
}

/*
 * Callback given to ftw. For each file in the directory tree, run the
 * appropriate lexer based on its type, run the winnowing algorithm upon
 * the lexer's results, and save the output to WORKING_DIR/TIMESTAMP/path.
 * Cross-file metadata is also updated here: for every unique fingerprint
 * output by the winnowing step, the entry in GLOBAL_FINGERPRINTS is
 * incremented by one.
 */
int walk_fn(const char *path, const struct stat *sb, int typeflag)
{
	if (S_ISREG(sb->st_mode)) {
		filetype f = determine_type(path);
		if (f != FT_UNKNOWN) {
			int file = open(path, O_RDONLY);
			snprintf(SPRINTF_BUFFER, 1024, "./lexer/%s/lex", LEXERS[f]);
			int lexer_o = ex_pi(file, SPRINTF_BUFFER, NULL);
			int winnow_o = ex_pi(lexer_o, "./bin/winnow", NULL);
			FILE *in = fdopen(winnow_o, "r");

			snprintf(SPRINTF_BUFFER, 1024, WORKING_DIR "/%u/%s", TIMESTAMP, path);
			FILE *out = fopen(SPRINTF_BUFFER, "w+");

			memset(LOCAL_FINGERPRINTS, 0, sizeof(LOCAL_FINGERPRINTS));
			char *hash;
			unsigned int lineno;
			while (fscanf(in, "%ms %u ", &hash, &lineno) == 2) {
				unsigned int index = hexstring_to_int(hash);
				LOCAL_FINGERPRINTS[index] += 1;
				if (LOCAL_FINGERPRINTS[index] == 1) GLOBAL_FINGERPRINTS[index] += 1;
				fprintf(out, "%s %u ", hash, lineno);
				free(hash);
			}
			NUM_FILES += 1;

			fclose(in);
			fclose(out);

			while (wait(NULL) > 0);
		}
	} else {
		snprintf(SPRINTF_BUFFER, 1024, WORKING_DIR "/%u/%s", TIMESTAMP, path);
		mkdir(SPRINTF_BUFFER, 0777);
	}
	return 0;
}

int main(int argc, char **argv)
{
	TIMESTAMP = time(NULL);
	mkdir(WORKING_DIR, 0777);
	snprintf(SPRINTF_BUFFER, 1024, WORKING_DIR "/%u", TIMESTAMP);
	mkdir(SPRINTF_BUFFER, 0777);
	ftw(argv[1], walk_fn, 8);

	snprintf(SPRINTF_BUFFER, 1024, WORKING_DIR "/%u/" GLOBAL_FILE_NAME, TIMESTAMP);
	FILE *global_output = fopen(SPRINTF_BUFFER, "w+");
	for (unsigned int i = 0; i < HASH_BOUND; ++i) {
		if (GLOBAL_FINGERPRINTS[i] && (double) GLOBAL_FINGERPRINTS[i] / (double) NUM_FILES > SHARED_THRESHOLD) {
			fprintf(global_output, "%04x ", i);
		}
	}
	fclose(global_output);

	printf("%u\n", TIMESTAMP);

	return 0;
}
