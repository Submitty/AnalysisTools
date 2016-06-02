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

void execute(int input, int output, int toclose, char **args)
{
	int pid = fork();
	if (pid == 0) {
		if (toclose != -1) close(toclose);
		if (input != -1) {
			if (dup2(input, STDIN_FILENO) == -1) {
				fprintf(stderr, "Error duplicating input.\n");
				exit(1);
			}
			close(input);
		}
		if (output != -1) {
			if (dup2(output, STDOUT_FILENO) == -1) {
				fprintf(stderr, "Error duplicating output.\n");
				exit(1);
			}
			close(output);
		}

		int res = execve(args[0], args, NULL);
		fprintf(stderr, "Error: %s\n", strerror(errno));
		exit(res);
	} else if (pid > 0) {
		if (input != -1) close(input);
		if (output != -1) close(output);
	} else {
		if (input != -1) close(input);
		if (output != -1) close(output);
	}
}

int ex_pi(int input, char *path, ...)
{
	char *args[32];
	va_list al;
	va_start(al, path);
	char *cur = path;
	int i;
	for (i = 0; cur != NULL && i < 32; cur = va_arg(al, char *), ++i) {
		args[i] = cur;
	}
	args[i] = NULL;

	int stdout_fds[2];
	if (pipe(stdout_fds) < 0) {
		printf("Error allocating pipe.\n");
		return -1;
	}
	execute(input, stdout_fds[1], stdout_fds[0], args);
	close(stdout_fds[1]);
	return stdout_fds[0];
}

int ex_po(int output, char *path, ...)
{
	char *args[32];
	va_list al;
	va_start(al, path);
	char *cur = path;
	int i;
	for (i = 0; cur != NULL && i < 32; cur = va_arg(al, char *), ++i) {
		args[i] = cur;
	}
	args[i] = NULL;

	int stdin_fds[2];
	if (pipe(stdin_fds) < 0) {
		printf("Error allocating pipe.\n");
		return -1;
	}
	execute(stdin_fds[0], output, stdin_fds[1], args);
	close(stdin_fds[0]);
	return stdin_fds[1];
}

void ex_io(int input, int output, char *path, ...)
{
	char *args[32];
	va_list al;
	va_start(al, path);
	char *cur = path;
	int i;
	for (i = 0; cur != NULL && i < 32; cur = va_arg(al, char *), ++i) {
		args[i] = cur;
	}
	args[i] = NULL;
	execute(input, output, -1, args);
}

#endif
