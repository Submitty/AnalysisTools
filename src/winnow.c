#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#include <string.h>

#include <ctype.h>

#include "config.h"

/*
 * Compute a hash on a string using the standard djb2 hashing algorithm.
 */
int hash(char *key, int len)
{
	int h = 5381;
	for (int i = 0; i < len; ++i) {
		h = h * 33 + key[i];
	}
	return abs(h % HASH_BOUND);
}

/*
 * Remove all "unnecessary" data from data, storing the result in buf as a
 * standard null-terminated C string.  Of course, what is unneccessary differs
 * from application to application.  Here, we simply remove symbols, numbers,
 * and whitespace while also converting all letters to lowercase.
 *
 * Examples:
 *
 * filter(&buf, "Hello, world!");
 *    -> buf = "helloworld
 *
 * filter(&buf, "123456789");
 *    -> buf = ""
 *
 * In analyzing the similarity of programs, it is likely better to use a
 * language-specific system for token generation, which is easily implemented
 * and requires only syntactic understanding of the language. It also has the
 * advantage of stopping some obvious plagiarism tactics, such as changing
 * variable names: if all tokens representing, for example, identifiers are
 * treated to be equivalent, no trivial change will modify the resulting
 * fingerprint.
 */
void filter(char *buf, char *data)
{
	int buf_index = 0;
	int len = strlen(data);
	for (int data_index = 0; data_index < len; ++data_index) {
		if (isalpha(data[data_index])) {
			buf[buf_index++] = tolower(data[data_index]);
		}
	}
	buf[buf_index] = 0;
}

/*
 * Given a sequence like that produced by filter or another tokenizer,
 * generate the fingerprints of the sequence. In this context, fingerprints
 * are sequences of hashed k-grams, where k is a user-provided value.
 *
 * Example:
 *
 * fingerprint(&buf, "helloworld", 5)
 *    -> buf = {hash("hello"), hash("ellow"), hash("llowo"), hash("lowor"), hash("oworl"), hash("world")}
 *
 */
void fingerprint(int *buf, char *data, int k)
{
	int len = strlen(data);
	for (int data_index = 0; data_index <= len - k; ++data_index) {
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
int winnow(int *buf, int *fingerprints, int fingerprints_size, int t, int k)
{
	int window_size = t - k + 1;
	int selected_index = 0;
	for (int fingerprints_index = 0; fingerprints_index < fingerprints_size - window_size; ++fingerprints_index) {
		int min = HASH_BOUND;
		int min_index = -1;
		for (int window_index = 0; window_index < window_size; ++window_index) {
			if (fingerprints[fingerprints_index + window_index] < min) {
				min = fingerprints[fingerprints_index + window_index];
				min_index = fingerprints_index + window_index;
			}
		}
		bool do_add = true;
		for (int i = 0; i < selected_index; ++i) {
			if (buf[i] == min_index) {do_add = false; break;}
		}
		if (do_add) buf[selected_index++] = min_index;
	}
	for (int i = 0; i < selected_index; ++i) {
		buf[i] = fingerprints[buf[i]];
	}
	return selected_index;
}

int main(int argc, char **argv)
{
	int t = atoi(argv[1]);
	int k = atoi(argv[2]);

	char data[2048];
	for (int i = 0; fscanf(stdin, "%d", (int *) &data[i++]) == 1 && i < 2048;);

	char buf[2048];
	filter(buf, data);

	int hashes[2048];
	fingerprint(hashes, buf, k);

	int fingerprints[2058];
	int len = winnow(fingerprints, hashes, strlen(buf) - k, t, k);
	for (int i = 0; i < len; ++i) {
		printf("%04x ", fingerprints[i]);
	}
	puts("");
}
