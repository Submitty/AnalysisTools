#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <sys/stat.h>
#include <sys/types.h>

#include "config.h"
#include "utils.h"

void write_diff(char *path, int count, int *a, int *b)
{
	FILE *f = fopen(path, "w");
	fputs("{", f);
	fputs("\"differences\" : [", f);
	for (int i = 0; i < count; ++i) {
		fputs("{", f);
		fputs("\"first\" : {", f);
		fprintf(f, "\"start\" : %d,\n", b[i] - 1);
		fprintf(f, "\"line\" : [{\"line_number\" : %d}]\n", b[i] - 1);
		fputs("},", f);
		fputs("\"second\" : {", f);
		fprintf(f, "\"start\" : %d,\n", a[i] - 1);
		fprintf(f, "\"line\" : [{\"line_number\" : %d}]\n", a[i] - 1);
		fputs("}", f);
		if (i + 1 == count) fputs("}", f);
		else fputs("},", f);
	}
	fputs("]", f);
	fputs("}", f);
	fclose(f);
}

void write_php(char *first, char *second)
{
	unsigned int id = pair_id(first, second);
	char buf[256];
	snprintf(buf, 256, "html_summary/%u.php", id);
	FILE *f = fopen(buf, "w");
	snprintf(buf, 256, "html_summary/%u.json", id);
	fprintf(f, "\
<?php\n\
require_once(\"php/lib/DiffViewer.php\");\n\
use lib\\DiffViewer;\n\
$diff = new DiffViewer(\"first\", \"second\");\n\
$diff->load(\"%s\",\n\
            \"%s\",\n\
            \"%s\");\n\
print <<<HTML\n\
<html>\n\
    <head>\n\
	<title>Plagiarism Detection</title>\n\
	<link href=\"../php/toolbox/include/diff-viewer/diff.css\" rel=\"stylesheet\"></link>\n\
	</head>\n\
	<body>\n\
	<div style=\"float: left; width: 50%%\">\n\
	{$diff->getDisplayActual()}\n\
	</div>\n\
	<div style=\"float: left; width: 50%%\">\n\
	{$diff->getDisplayExpected()}\n\
	</div>\n\
	</body>\n\
</html>\n\
HTML;\n\
?>", first, second, buf);
	fclose(f);
}

int parse_pairs(char *pairs, int *a, int *b)
{
	char *cur = pairs;
	int count = 0;
	while (sscanf(cur, " ( %d %d ) ", &a[count], &b[count]) == 2) {
		++count;
		cur = strchr(cur, ':');
		if (cur == NULL) break;
		else ++cur;
	}
	return count;
}

int smudge_pairs(int *a2, int *b2, int *a, int *b, int count)
{
	memcpy(a2, a, count * sizeof(int));
	memcpy(b2, b, count * sizeof(int));
	unsigned int amax = 0, bmax = 0;
	unsigned int amin = -1, bmin = -1;
	int index = count;
	for (int i = 0; i < count; ++i) {
		if (a[i] > amax) amax = a[i];
		if (b[i] > bmax) bmax = b[i];
		if (a[i] < amin) amin = a[i];
		if (b[i] < bmin) bmin = b[i];
	}
	for (int i = amin; i < amax; ++i) {
		bool abovemet = false, belowmet = false;
		for (int j = 0; j < count; ++j) {
			if (a[j] == i-1 || a[j] == i-2) abovemet = true;
			if (a[j] == i+1 || a[j] == i+2) belowmet = true;
		}
		if (abovemet && belowmet) {
			a2[index] = i;
			b2[index++] = b[0];
		}
	}
	for (int i = bmin; i < bmax; ++i) {
		bool abovemet = false, belowmet = false;
		for (int j = 0; j < count; ++j) {
			if (b[j] == i-1 || b[j] == i-2) abovemet = true;
			if (b[j] == i+1 || b[j] == i+2) belowmet = true;
		}
		if (abovemet && belowmet) {
			b2[index] = i;
			a2[index++] = a[0];
		}
	}
	return index;
}

int main(int argc, char **argv)
{
	mkdir("html_summary", 0777);
	char line[4096];
	int count, a[4096], b[4096], a2[4096], b2[4096];
	puts("<table>");
	while (fgets(line, 4096, stdin) != NULL) {
		char *percent_match = strtok(line, ",");
		char *first = strtok(NULL, ",");
		char *second = strtok(NULL, ",");
		char *pairs = strtok(NULL, ",");
		printf("<tr><td>%s</td><td><a href=html_summary/%u.html>view</a></td><td>%s</td><td>%s</td></tr>\n",
				percent_match, pair_id(first, second), first, second);
		count = parse_pairs(pairs, a, b);
		count = smudge_pairs(a2, b2, a, b, count);
		char buf[256];
		snprintf(buf, 256, "html_summary/%u.json", pair_id(first, second));
		write_diff(buf, count, a2, b2);
		write_php(first, second);
	}
	puts("</table>");
}
