#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <getopt.h>

#include "config.h"

/*
 * Compute a hash on an array of unsigned integers using the standard djb2
 * hashing algorithm.
 */
static unsigned int hash(unsigned int *key, unsigned int len)
{
	unsigned int h = 5381;
	unsigned int i;
	for (i = 0; i < len; ++i) {
		h = h * 33 + key[i];
	}
	return h % HASH_BOUND;
}

/*
 * Given a sequence produced a tokenizer, generate the fingerprints of the
 * sequence. In this context, fingerprints are sequences of hashed k-grams,
 * where k is a user-provided value.
 *
 * Example:
 *
 * fingerprint(&buf, "helloworld", 5)
 *    -> buf = {hash("hello"), hash("ellow"), hash("llowo"), hash("lowor"), hash("oworl"), hash("world")}
 *
 */
static void fingerprint(unsigned int *buf, unsigned int *data,
			unsigned int data_size, unsigned int k)
{
	unsigned int data_index;
	for (data_index = 0; data_index <= data_size - k; ++data_index) {
		buf[data_index] = hash(&data[data_index], k);
	}
}

/*
 * Given the fingerprints of some data, selectively prune those fingerprints
 * to improve efficiency. This process is detailed in
 *     http://theory.stanford.edu/~aiken/publications/papers/sigmod03.pdf,
 * but as a brief summary:
 *
 * The algorithm is reliant on two user-provided values: t, or the guarantee
 * threshold, and k (previously used in the fingerprint function), the noise
 * threshold. k marks the character (or, in more complex cases, token) length
 * below which matches are considered noise. t marks the character length
 * above which we want to definitely detect all matches. In other words, to
 * quote the previously listed paper:
 *     1) If there is a substring match at least as long as the guarantee
 *     threshold, t, then this match is detected, and
 *     2) We do not detect any matches shorter than the noise threshold, k
 * In this example, a value of k of about 5 is used, which prevents matches
 * on many common english words, for example "the", "there", "a", "and", that
 * do not hold much significance relating to actual content that might be
 * plagiarized. As a more practical example, in a system used to detect
 * plagiarism in C++ programs, a value of k greater than 3 might be used to
 * ensure the system does not match the common sequence
 *     ) ; }
 * which occurs at the end of any block where the last statement is a function
 * call.
 *
 * Using t and k, a window size is determined to be t - k + 1. This value is
 * used to move a sliding window over the fingerprints, a process mirroring
 * that used to break the initial data into k-grams. Within each window, the
 * minimum value is taken and added to a result set, if this particular instance
 * is not already in the result set. For example, take two windows
 *     [2 1 3 4] and [1 3 4 5],
 * which are windows over the sequence
 *     [2 1 3 4 5].
 * In this situation, the hash 1 is added to the result set when examining the
 * first window. When the second window is examined, the minimum value is 1, but
 * it is the same one that was already added to the result set, so it is ignored
 * and no value is added. However, say the windows are
 *     [2 1 3 4] and [1 3 4 1],
 * over the sequence
 *     [2 1 3 4 1].
 * Here, 1 is added once again when processing the first window, but in the second
 * window another 1 is added to the result set, since there is a minimum value
 * present that was not already processed.
 *
 * Since the algorithm is already concerned with the positions of specific hashes
 * within the original fingerprint, it would be trivial to allow a tool for
 * plagiarism detection to report specific locations of matches within each file.
 */
static unsigned int winnow(unsigned int *buf,
			   unsigned int *lineno_buf, unsigned int *fingerprints,
			   unsigned int *lineno,
			   unsigned int fingerprints_size, unsigned int t,
			   unsigned int k)
{
	unsigned int window_size = t - k + 1;
	unsigned int selected_index = 0;
	unsigned int fingerprints_index;
	unsigned int i;
	for (fingerprints_index = 0;
	     fingerprints_index < fingerprints_size - window_size;
	     ++fingerprints_index) {
		unsigned int min = HASH_BOUND;
		unsigned int min_index = (unsigned int)-1;
		unsigned int window_index;
		bool do_add = true;
		for (window_index = 0; window_index < window_size;
		     ++window_index) {
			if (fingerprints[fingerprints_index + window_index] <
			    min) {
				min =
				    fingerprints[fingerprints_index +
						 window_index];
				min_index = fingerprints_index + window_index;
			}
		}
		for (i = 0; i < selected_index; ++i) {
			if (buf[i] == min_index) {
				do_add = false;
				break;
			}
		}
		if (do_add)
			buf[selected_index++] = min_index;
	}
	for (i = 0; i < selected_index; ++i) {
		lineno_buf[i] = lineno[buf[i]];
		buf[i] = fingerprints[buf[i]];
	}
	return selected_index;
}

int main(int argc, char **argv)
{
	static unsigned int t = DEFAULT_UPPER_BOUND;
	static unsigned int k = DEFAULT_LOWER_BOUND;

	static int arg;

	static unsigned int data[4096];
	static unsigned int lineno[4096];
	static unsigned int data_size = 0;

	static unsigned int hashes[4096];

	static unsigned int fingerprints[4096];
	static unsigned int lineno_new[4096];
	static unsigned int len;
	static unsigned int i;

	while ((arg = getopt(argc, argv, "u:l:")) != -1) {
		switch (arg) {
		case 'u':
			t = (unsigned int)atoi(optarg);
			break;
		case 'l':
			k = (unsigned int)atoi(optarg);
			break;
		}
	}
	for (;
	     fscanf(stdin, "%u %u ", &data[data_size], &lineno[data_size]) == 2
	     && data_size < 4096; ++data_size) ;
	fingerprint(hashes, data, data_size, k);
	len =
	    winnow(fingerprints, lineno_new, hashes, lineno, data_size - k, t,
		   k);
	for (i = 0; i < len; ++i) {
		printf("%04x %u ", hash(&fingerprints[i], 1), lineno_new[i]);
	}
	puts("");

	return 0;
}
