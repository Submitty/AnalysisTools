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

		char *hash;
		int result;
		while (1) {
			result = fscanf(first, "%ms", &hash);
			if (result != 1) break;
			first_matchcount[hexstring_to_int(hash)] += 1;
			free(hash);
		}
		while (1) {
			result = fscanf(second, "%ms", &hash);
			if (result != 1) break;
			second_matchcount[hexstring_to_int(hash)] += 1;
			free(hash);
		}

		int match = 0;
		int total = 0;
		for (int i = 0; i < HASH_BOUND; ++i) {
			if (first_matchcount[i] && second_matchcount[i]) match += 1;
			if (first_matchcount[i] || second_matchcount[i]) total += 1;
		}
		printf("%f : %s | %s\n", ((float) match)/((float) total) * 100.0, firstpath, secondpath);
		fclose(first);
		fclose(second);
	}

	return 0;
}
