#ifndef FINGERPRINTS_H
#define FINGERPRINTS_H

typedef struct file_fingerprints {
	char path[1024];
	unsigned int matchcount[HASH_BOUND];
	unsigned int lineno[HASH_BOUND];
	struct file_fingerprints *next;
} file_fingerprints;

unsigned int hexstring_to_int(char *str)
{
	//int len = strlen(str);
	int len = 4;
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

#endif
