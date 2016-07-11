# Analysis Tools [![Build Status](https://travis-ci.org/Submitty/AnalysisTools.svg?branch=master)](https://travis-ci.org/Submitty/AnalysisTools)
This repository contains a variety of tools used for source code analysis.

## Dependencies
* C compiler with Posix standard libraries (tested on GCC 4.9.3)
* Flex (tested on version 2.5.39)
* GNU Bison (tested on version 3.0.4)
* PCRE (tested on version 8.38)
* Python (tested on version 3.43)
* Eclipse JDT Library
* python-graph-tool

Optionally, if GNU Indent, Splint, and/or Pylint are installed, they will be incorporated into the build process to check for common stylistic and logical errors. It is recommended to install these tools if you wish to make a large contribution.

## Building
Simply run `make` in the repository root. This will build a number of executables, described below, to the `bin/` directory. In the interest of providing examples that fully express the usage of the tool, each example uses the complete set of available flags. However, *every* flag is optional, as a rule. Default values can be set at three different levels: at the lowest level of precedence, build-wide values can be set in `include/config.h`. Single values in `include/config.h` can be overwritten for a specific executable by setting `CFLAGS` when calling `make`. Finally, language-specific values and some other runtime configuration used by `bin/plagiarism` can be modified in `config/plagiarism.json` by default (or by passing the `-c config_file.json` flag to `bin/plagiarism`).

### `bin/anonymize`
Usage example: `cat file.c | ./bin/anonymize -n name_list.txt -t to_replace.csv -r 's/foo/bar/' -a 10`

Anonymizes standard input, writing to standard output. In the example above, case-insensitively redacts all words found in the whitespace-delimited list in `name_list.txt`, replaces any word found in the first column of the two-column CSV file `to_replace.csv` with the corresponding word in the second column, and replaces any string matching regular expression `foo` with the string `bar`. If the `-a <n>` flag is provided, replace only in the first `n` lines of the input.

Multiple instances of each flag may be passed when appropriate. For example, `./bin/anonymize -n name_list.txt -n name_list_2.txt` will replace words found in either list.

### `bin/anonymization`
Usage example: `./bin/anonymization source_dir -n first_names.txt -n last_names.txt -t rcs_ids.txt -r 's/66[0-9]{7}/RIN/' -a 5 -l 3`.

Anonymizes all files in the given directory tree. The `-n`, `-t`, `-a`, and `-r` flags have the same meaning as those passed to `bin/anonymize`. One additional flag is also accepted: `-l`, which allows the user to specify a level in the directory tree (as an integer). Any directories at this level will also have their names anonymized according to the CSV files passed using the `-t` flag. Note that only those CSV files will be used for the replacement: simple name lists passed with `-n` will not be used for directory replacement.

Capture the STDOUT of this run to see the stats of the # of replacements in directory names, file names, and file contents.

### `bin/plagiarism`
Usage example: `./bin/plagiarism java source_dir`

Checks every source file in the given language contained in `source_dir` (or a subdirectory) against every other source file for possible plagiarism, writing sorted (by percent match) CSV data to standard output.

### `bin/winnow`
Usage example: `cat tokens | ./bin/winnow -u 30 -l 15`

Applys the winnowing algorithm to a sequence of whitespace-separated integer-valued tokens read from standard input using upper bound `30` and lower bound `15`. Writes the selected fingerprints to standard output.

### `bin/walk`
Usage example: `./bin/walk -u 30 -l 15 python source_dir`

Tokenizes and winnows each source file in `source_dir`, using upper bound `30` and lower bound `15`. Winnowing output is written in a parallel directory tree as a subdirectory of `.analysis_data/<timestamp>`, where timestamp is a unique integer-valued timestamp. The timestamp used is written to standard output.

### `bin/genpairs`
Usage example: `./bin/genpairs <timestamp>`

Generates all pairs of files in the directory `.analysis_data/<timestamp>`, writing those pairs to standard output.

### `bin/compare_fingerprints`
Usage example: `cat pairs | ./bin/genpairs <timestamp>`

Reads pairs from standard input, reads fingeprint data from `.analysis_data/<timestamp>`, and computes match data, writing to standard output.

### Java Parser
Arguments: \[javafile ...\] \[ -t for XML ast \|  -u for class hierarchical data\] -o filename

### Class Structure Analysis
Usage example: `python CSA.py student\_structure\_file subgraph\_structure\_file`
