#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>

#include <errno.h>

#include <unistd.h>

#include <sys/stat.h>
#include <sys/types.h>

#include "config.h"

typedef char string[1024];

/*
 * Convert a string containing hexadecimal digits to an integer.
 */
unsigned int hexstring_to_int(const char *str)
{
	unsigned int len = (unsigned int)strlen(str);
	unsigned int total = 0;
	unsigned int i;
	for (i = 0; i < len; ++i) {
		total *= 16;
		if (str[i] >= '0' && str[i] <= '9') {
			total += (unsigned int)(str[i] - '0');
		} else if (str[i] >= 'a' && str[i] <= 'f') {
			total += (unsigned int)(str[i] - 'a' + (char)0xa);
		} else if (str[i] >= 'A' && str[i] <= 'F') {
			total += (unsigned int)(str[i] - 'A' + (char)0xa);
		}
	}
	return total;
}

unsigned int pair_id(const char *first, const char *second)
{
	unsigned int size = (unsigned int)strlen(first)
	    + (unsigned int)strlen(second);
	char *buf = (char *)malloc((size_t) size + 1);
	unsigned int h = 5381;
	unsigned int i;
	if (buf == NULL)
		exit(EXIT_FAILURE);
	snprintf(buf, (size_t) size + 1, "%s%s", first, second);
	for (i = 0; i < size; ++i) {
		h = h * 33 + (unsigned int)buf[i];
	}
	free(buf);
	return h;
}

/*
 * Given file descriptors input, output, and toclose and complete
 * arguments args, run the executable at path args[0], providing args as
 * arguments and using input and output as standard input and output
 * respectively. This function does not call wait; the process will run in
 * the background and the caller is responsible for taking appropriate
 * action to ensure it is cleaned.
 */
static void execute(int input, int output, int toclose, char **args)
{
	int pid = fork();
	if (pid == 0) {
		int res;

		if (toclose != -1)
			close(toclose);
		if (input != -1) {
			if (dup2(input, STDIN_FILENO) == -1) {
				fprintf(stderr, "Error duplicating input.\n");
				exit(EXIT_FAILURE);
			}
			close(input);
		}
		if (output != -1) {
			if (dup2(output, STDOUT_FILENO) == -1) {
				fprintf(stderr, "Error duplicating output.\n");
				exit(EXIT_FAILURE);
			}
			close(output);
		}
		res = execve(args[0], args, NULL);
		fprintf(stderr, "Error: %s\n", strerror(errno));
		exit(res);
	} else if (pid > 0) {
		if (input != -1)
			close(input);
		if (output != -1)
			close(output);
	} else {
		if (input != -1)
			close(input);
		if (output != -1)
			close(output);
	}
}

/*
 * Runs the executable at path with args arg1, arg2, etc. using input as
 * standard input. Returns a pipe for the standard output of the
 * executable.
 */
int ex_pi(int input, char *path, ...)
{				/* char *arg1, char *arg2, ... */
	char *args[32];
	va_list al;
	char *cur;
	int i;
	int stdout_fds[2] = {-1, -1};

	va_start(al, path);
	cur = path;

	for (i = 0; cur != NULL && i < 32; cur = va_arg(al, char *), ++i) {
		args[i] = cur;
	}
	args[i] = NULL;

	if (pipe(stdout_fds) < 0) {
		printf("Error allocating pipe.\n");
		return -1;
	}
	execute(input, stdout_fds[1], stdout_fds[0], args);
	close(stdout_fds[1]);
	return stdout_fds[0];
}

/*
 * Runs the executable at path with args arg1, arg2, etc. using output as
 * standard output. Returns a pipe for the standard input of the
 * executable.
 */
int ex_po(int output, char *path, ...)
{				/* char *arg1, char *arg2, ... */
	char *args[32];
	va_list al;
	char *cur;
	int i;
	int stdin_fds[2] = {-1, -1};

	va_start(al, path);
	cur = path;

	for (i = 0; cur != NULL && i < 32; cur = va_arg(al, char *), ++i) {
		args[i] = cur;
	}
	args[i] = NULL;

	if (pipe(stdin_fds) < 0) {
		printf("Error allocating pipe.\n");
		return -1;
	}
	execute(stdin_fds[0], output, stdin_fds[1], args);
	close(stdin_fds[0]);
	return stdin_fds[1];
}

/*
 * Runs the executable at path with args arg1, arg2, etc. using input as
 * standard input and output as standard output.
 */
void ex_io(int input, int output, char *path, ...)
{				/* char *arg1, char *arg2, ... */
	char *args[32];
	va_list al;
	char *cur;
	int i;

	va_start(al, path);
	cur = path;

	for (i = 0; cur != NULL && i < 32; cur = va_arg(al, char *), ++i) {
		args[i] = cur;
	}
	args[i] = NULL;
	execute(input, output, -1, args);
}

#endif
