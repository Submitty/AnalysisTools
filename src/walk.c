#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <getopt.h>

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

static char SPRINTF_BUFFER[1024];
static char FILETYPE[64] = "";
static char UPPER_BOUND[64];
static char LOWER_BOUND[64];
static unsigned int TIMESTAMP;

static unsigned int GLOBAL_FINGERPRINTS[HASH_BOUND] = {0};
static unsigned int LOCAL_FINGERPRINTS[HASH_BOUND] = {0};
static unsigned int NUM_FILES = 0;


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
		int file = open(path, O_RDONLY);
		snprintf(SPRINTF_BUFFER, 1024, "./lexer/%s/lex", FILETYPE);
		int lexer_o = ex_pi(file, SPRINTF_BUFFER, NULL);
		int winnow_o = ex_pi(lexer_o, "./bin/winnow", "-u", UPPER_BOUND, "-l", LOWER_BOUND, NULL);
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
	} else {
		snprintf(SPRINTF_BUFFER, 1024, WORKING_DIR "/%u/%s", TIMESTAMP, path);
		mkdir(SPRINTF_BUFFER, 0777);
	}
	return 0;
}

int main(int argc, char **argv)
{
	unsigned int arg;
	snprintf(UPPER_BOUND, 64, "%u", DEFAULT_UPPER_BOUND);
	snprintf(LOWER_BOUND, 64, "%d", DEFAULT_LOWER_BOUND);
	while ((arg = getopt(argc, argv, "u:l:")) != -1) {
		switch (arg) {
			case 'u':
				strncpy(UPPER_BOUND, optarg, 64);
				break;
			case 'l':
				strncpy(LOWER_BOUND, optarg, 64);
				break;
		}
	}

	if (argc - optind < 2) {
		fprintf(stderr, "Usage: %s <filetype> <directory>\n", argv[0]);
		exit(1);
	}

	strncpy(FILETYPE, argv[optind], 64);

	TIMESTAMP = time(NULL);
	mkdir(WORKING_DIR, 0777);
	snprintf(SPRINTF_BUFFER, 1024, WORKING_DIR "/%u", TIMESTAMP);
	mkdir(SPRINTF_BUFFER, 0777);
	ftw(argv[optind + 1], walk_fn, 8);

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
