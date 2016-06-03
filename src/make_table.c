#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include "config.h"
#include "utils.h"

int main(int argc, char **argv)
{
	puts("<table>");
	char line[4096];
	while (fgets(line, 4096, stdin) != NULL) {
		puts("<tr>");
		char *cellval = strtok(line, "|");
		do {
			printf("<td>%s</td>", cellval);
		} while ((cellval = strtok(NULL, "|")) != NULL);
		puts("</tr>");
	}
	puts("</table>");
}
