#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include "config.h"
#include "utils.h"

/*
 * Structure used to represent an entry in the hash table used as a cache
 * for the fingerprint data of each compared file.
 */
typedef struct file_fingerprints {
	string path; /* The file's path (hash key) */
	unsigned int matchcount[HASH_BOUND]; /* The number of occurrences of each fingerprint */
	unsigned int lineno[HASH_BOUND]; /* The last line number corresponding to each fingerprint */
	struct file_fingerprints *next; /* The next file_fingerprints in this bucket */
} file_fingerprints;

file_fingerprints FINGERPRINT_CACHE[FINGERPRINT_CACHE_SIZE];

/*
 * Array storing cumulative data for all files (read in from
 * GLOBAL_FILE_NAME). GLOBAL_FINGERPRINTS[i] is true if fingerprint i is a
 * shared fingerprint that should be ignored, false otherwise.
 */
bool GLOBAL_FINGERPRINTS[HASH_BOUND] = {false};

/*
 * Compute a hash on a string using the standard djb2 hashing algorithm.
 */
static unsigned int hash(const char *key)
{
	unsigned int h = 5381;
	for (unsigned int i = 0; i < strlen(key); ++i) {
		h = h * 33 + key[i];
	}
	return h % FINGERPRINT_CACHE_SIZE;
}

/*
 * Read whitespace-separated pairs of hexadecimal strings and integers from
 * the file at path, updating the match count and line number data in buf.
 */
static void read_fingerprints(file_fingerprints *buf, const char *path)
{
	FILE *f = fopen(path, "r");
	char *hash;
	unsigned int lineno;

	while (fscanf(f, "%ms %d ", &hash, &lineno) == 2) {
		unsigned int index = hexstring_to_int(hash);
		buf->matchcount[index] += 1;
		buf->lineno[index] = lineno;
		free(hash);
	}
	fclose(f);
}

/*
 * Given a path, return a file_fingerprints structure containing the
 * fingerprint data associated with that path, either loading the existing
 * entry stored in the cache or by reading the file (and making a new cache
 * entry).
 */
static file_fingerprints *get_fingerprints(const char *path)
{
	unsigned int hashed = hash(path);
	file_fingerprints *entry = &FINGERPRINT_CACHE[hashed];

	while (strncmp(path, entry->path, STRING_LENGTH) != 0) {
		if (entry->next == NULL) {
			entry->next = (file_fingerprints *) malloc(sizeof(file_fingerprints));
			strncpy(entry->next->path, path, STRING_LENGTH);
			read_fingerprints(entry->next, path);
			entry->next->next = NULL;
		}
		entry = entry->next;
	}
	return entry;
}

int main(int argc, char **argv)
{
	if (argc < 2) {
		fprintf(stderr, "Usage: %s <timestamp>\n", argv[0]);
		exit(1);
	}

	string firstpath, secondpath, prefix, format;
	snprintf(prefix, STRING_LENGTH, "%s/%s/", WORKING_DIR, argv[1]);
	snprintf(format, STRING_LENGTH, "%s%%s", prefix);

	snprintf(firstpath, STRING_LENGTH, "%s%s", prefix, GLOBAL_FILE_NAME);
	FILE *f = fopen(firstpath, "r");
	char *hash;

	while (fscanf(f, "%ms ", &hash) == 1) {
		unsigned int index = hexstring_to_int(hash);
		GLOBAL_FINGERPRINTS[index] = true;
		free(hash);
	}
	fclose(f);

	while(fscanf(stdin, "%s %s ", firstpath, secondpath) == 2) {

		file_fingerprints *first = get_fingerprints(firstpath);
		file_fingerprints *second = get_fingerprints(secondpath);

		unsigned int match = 0;
		unsigned int total = 0;
		string lines = "";
		string swap;
		for (unsigned int i = 0; i < HASH_BOUND; ++i) {
			if (!GLOBAL_FINGERPRINTS[i]) {
				if (first->matchcount[i] && second->matchcount[i]) {
					match += 1;
					memcpy(swap, lines, STRING_LENGTH);
					snprintf(lines, STRING_LENGTH, "%s(%u %u):", swap, first->lineno[i], second->lineno[i]);
				}
				if (first->matchcount[i] || second->matchcount[i]) total += 1;
			}
		}
		sscanf(firstpath, format, firstpath);
		sscanf(secondpath, format, secondpath);
		printf("%9.5f,%s,%s,%s\n", ((float) match)/((float) total) * 100.0, firstpath, secondpath, lines);
	}

	return 0;
}
