#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include "config.h"
#include "utils.h"

typedef struct file_fingerprints {
	char path[1024];
	unsigned int matchcount[HASH_BOUND];
	unsigned int lineno[HASH_BOUND];
	struct file_fingerprints *next;
} file_fingerprints;

file_fingerprints FINGERPRINT_CACHE[FINGERPRINT_CACHE_SIZE];
unsigned int GLOBAL_FINGERPRINTS[HASH_BOUND] = {0};

unsigned int hash(char *key)
{
	unsigned int h = 5381;
	for (unsigned int i = 0; i < strlen(key); ++i) {
		h = h * 33 + key[i];
	}
	return abs(h % FINGERPRINT_CACHE_SIZE);
}

void read_fingerprints(file_fingerprints *buf, char *path)
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

file_fingerprints *get_fingerprints(char *path)
{
	unsigned int hashed = hash(path);
	file_fingerprints *entry = &FINGERPRINT_CACHE[hashed];

	while (strncmp(path, entry->path, 1024) != 0) {
		if (entry->next == NULL) {
			entry->next = (file_fingerprints *) malloc(sizeof(file_fingerprints));
			strncpy(entry->next->path, path, 1024);
			read_fingerprints(entry->next, path);
			entry->next->next = NULL;
		}
		entry = entry->next;
	}
	return entry;
}

int main(int argc, char **argv)
{
	char firstpath[1024], secondpath[1024];

	snprintf(firstpath, 1024, "%s/%s/%s", WORKING_DIR, argv[1], GLOBAL_FILE_NAME);
	FILE *f = fopen(firstpath, "r");
	char *hash;
	double ratio;

	while (fscanf(f, "%ms %lf ", &hash, &ratio) == 2) {
		unsigned int index = hexstring_to_int(hash);
		GLOBAL_FINGERPRINTS[index] = ratio;
		printf("%lf", ratio);
		free(hash);
	}
	fclose(f);

	while(fscanf(stdin, "%s %s ", firstpath, secondpath) == 2) {

		file_fingerprints *first = get_fingerprints(firstpath);
		file_fingerprints *second = get_fingerprints(secondpath);

		unsigned int match = 0;
		unsigned int total = 0;
		for (unsigned int i = 0; i < HASH_BOUND; ++i) {
			if (GLOBAL_FINGERPRINTS[i] <= SHARED_THRESHOLD) {
				if (first->matchcount[i] && second->matchcount[i]) match += 1;
				if (first->matchcount[i] || second->matchcount[i]) total += 1;
			}
		}
		printf("%9.5f | %s | %s |", ((float) match)/((float) total) * 100.0, firstpath, secondpath);
		for (unsigned int i = 0; i < HASH_BOUND; ++i) {
			if (first->matchcount[i] && second->matchcount[i]) printf(" %u %u,", first->lineno[i], second->lineno[i]);
		}
		puts("");
	}

	return 0;
}
