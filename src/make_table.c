#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include "config.h"
#include "utils.h"

static void write_diff(const char *path, unsigned int count, unsigned int *a, unsigned int *b)
{
	FILE *f = fopen(path, "w");
	unsigned int i;

	fputs("{", f);
	fputs("\"differences\" : [", f);
	for (i = 0; i < count; ++i) {
		fputs("{", f);
		fputs("\"first\" : {", f);
		fprintf(f, "\"start\" : %u,\n", b[i] - 1);
		fprintf(f, "\"line\" : [{\"line_number\" : %u}]\n", b[i] - 1);
		fputs("},", f);
		fputs("\"second\" : {", f);
		fprintf(f, "\"start\" : %u,\n", a[i] - 1);
		fprintf(f, "\"line\" : [{\"line_number\" : %u}]\n", a[i] - 1);
		fputs("}", f);
		if (i + 1 == count) fputs("}", f);
		else fputs("},", f);
	}
	fputs("]", f);
	fputs("}", f);
	fclose(f);
}

static void write_php(const char *first, const char *second)
{
	unsigned int id = pair_id(first, second);
	FILE *f;
	string buf;
	snprintf(buf, STRING_LENGTH, "html_summary/%u.php", id);
	f = fopen(buf, "w");
	snprintf(buf, STRING_LENGTH, "html_summary/%u.json", id);
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

static unsigned int parse_pairs(const char *pairs, unsigned int *a, unsigned int *b)
{
	const char *cur = pairs;
	unsigned int count = 0;
	while (sscanf(cur, " ( %u %u ) ", &a[count], &b[count]) == 2) {
		++count;
		cur = strchr(cur, ':');
		if (cur == NULL) break;
		else ++cur;
	}
	return count;
}

static unsigned int smudge_pairs(unsigned int *a2, unsigned int *b2,
		unsigned int *a, unsigned int *b, unsigned int count)
{
	unsigned int amax = 0, bmax = 0;
	unsigned int amin = (unsigned int) -1, bmin = (unsigned int) -1;
	unsigned int index = count, i;

	memcpy(a2, a, count * sizeof(int));
	memcpy(b2, b, count * sizeof(int));

	for (i = 0; i < count; ++i) {
		if (a[i] > amax) amax = a[i];
		if (b[i] > bmax) bmax = b[i];
		if (a[i] < amin) amin = a[i];
		if (b[i] < bmin) bmin = b[i];
	}
	for (i = amin; i < amax; ++i) {
		bool abovemet = false, belowmet = false;
		unsigned int j;
		for (j = 0; j < count; ++j) {
			if (a[j] == i-1 || a[j] == i-2) abovemet = true;
			if (a[j] == i+1 || a[j] == i+2) belowmet = true;
		}
		if (abovemet && belowmet) {
			a2[index] = i;
			b2[index++] = b[0];
		}
	}
	for (i = bmin; i < bmax; ++i) {
		bool abovemet = false, belowmet = false;
		unsigned int j;
		for (j = 0; j < count; ++j) {
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

int main(/*@unused@*/ int argc, /*@unused@*/ char **argv)
{
	char line[4096];
	unsigned int count, a[4096], b[4096], a2[4096], b2[4096];

	mkdir("html_summary", 0777);
	puts("<table>");
	while (fgets(line, 4096, stdin) != NULL) {
		const char *percent_match = strtok(line, ",");
		const char *first = strtok(NULL, ",");
		const char *second = strtok(NULL, ",");
		const char *pairs = strtok(NULL, ",");
		string buf;

		printf("<tr><td>%s</td><td><a href=html_summary/%u.html>view</a></td><td>%s</td><td>%s</td></tr>\n",
				percent_match, pair_id(first, second), first, second);
		count = parse_pairs(pairs, a, b);
		count = smudge_pairs(a2, b2, a, b, count);
		snprintf(buf, STRING_LENGTH, "html_summary/%u.json", pair_id(first, second));
		write_diff(buf, count, a2, b2);
		write_php(first, second);
	}
	puts("</table>");

	return 0;
}
