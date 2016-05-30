#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#include <string.h>

#include <ctype.h>

#include <math.h>

#include "config.h"

unsigned int hexstring_to_int(char *str)
{
	int len = strlen(str);
	unsigned int total = 0;
	for (int i = 0; i < len; ++i) {
		total *= 16;
		if (str[i] >= '0' && str[i] <= '9') {
			total += (str[i] - '0');
		} else if (str[i] >= 'a' && str[i] <= 'f') {
			total += (str[i] - 'a' + 0xa);
		} else if (str[i] >= 'A' && str[i] <= 'F') {
			total += (str[i] - 'A' + 0xa);
		}
	}
	return total;
}

int main(int argc, char **argv)
{
	char firstpath[1024], secondpath[1024];

	while(fscanf(stdin, "%s", firstpath) == 1) {
		fscanf(stdin, "%s", secondpath);
		FILE *first = fopen(firstpath, "r");
		FILE *second = fopen(secondpath, "r");

		unsigned int first_matchcount[HASH_BOUND] = {0};
		unsigned int second_matchcount[HASH_BOUND] = {0};
		int first_lineno[HASH_BOUND] = {0};
		int second_lineno[HASH_BOUND] = {0};

		char *hash;
		int result;
		int lineno;

		while (1) {
			result = fscanf(first, "%ms %d ", &hash, &lineno);
			if (result != 2) break;
			int index = hexstring_to_int(hash);
			first_matchcount[index] += 1;
			first_lineno[index] = lineno;
			free(hash);
		}
		fclose(first);

		while (1) {
			result = fscanf(second, "%ms %d ", &hash, &lineno);
			if (result != 2) break;
			int index = hexstring_to_int(hash);
			second_matchcount[index] += 1;
			second_lineno[index] = lineno;
			free(hash);
		}
		fclose(second);

		int match = 0;
		int total = 0;
		for (int i = 0; i < HASH_BOUND; ++i) {
			if (first_matchcount[i] && second_matchcount[i]) match += 1;
			if (first_matchcount[i] || second_matchcount[i]) total += 1;
		}
		printf("%f : %s | %s |", ((float) match)/((float) total) * 100.0, firstpath, secondpath);
		for (int i = 0; i < HASH_BOUND; ++i) if (first_matchcount[i] && second_matchcount[i]) printf("%d %d, ", first_lineno[i], second_lineno[i]);
		puts("");
	}

	return 0;
}
