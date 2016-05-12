#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#include <string.h>
#include <errno.h>

#include <unistd.h>
#include <sys/wait.h>

void execute_bg(char *path, char **args)
{
	int pid = fork();
	if (pid == 0) {
		int res = execve(path, args, NULL);
		if (res == -1) {
			printf("Error: %s\n", strerror(errno));
			exit(1);
		} else exit(0);
	}
}

int main(int argc, char **argv)
{
	char **withsentinel = (char **) malloc(sizeof(char *) * argc);
	for (int i = 2; i < argc; ++i) {
		withsentinel[i - 2] = argv[i];
	}
	withsentinel[argc] = NULL;
	int count = atoi(argv[1]);
	for (int i = 0; i < count; ++i) {
		execute_bg(withsentinel[0], withsentinel);
	}
	while (wait(NULL) > 0);

	return 0;
}
