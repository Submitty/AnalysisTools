# Analysis Tools
This repository contains a variety of tools used for source code analysis.

## Building
Simply run `make` in the repository root. This will build a number of executables, described below, to the `bin/` directory.

### `bin/anonymize`
Usage: `cat file.c | ./bin/anonymize -n name_list.txt -t to_replace.csv -r 's/foo/bar/'`

Anonymizes standard input, writing to standard output. In the example above, case-insensitively redacts all words found in the whitespace-delimited list in `name_list.txt`, replaces any word found in the first column of the two-column CSV file `to_replace.csv` with the corresponding word in the second column, and replaces any string matching regular expression `foo` with the string `bar`.

Multiple instances of each flag may be passed. For example, `./bin/anonymize -n name_list.txt -n name_list_2.txt` will replace words found in either list.

### `bin/anonymize_dirs`
Usage: `./bin/anonymize_dirs source_dir -t first_names.csv -t last_names.csv -n rcs_ids.txt -r 's/66[0-9]{7}/RIN/' -l 3`.

Anonymizes all files in the given directory tree. The `-n`, `-t`, and `-r` flags have the same meaning as those passed to `bin/anonymize`. One additional flag is also accepted: `-l`, which allows the user to specify a level in the directory tree (as an integer). Any directories at this level will also have their names anonymized according to the CSV files passed using the `-t` flag. Note that only those CSV files will be used for the replacement: simple name lists passed with `-n` will not be used for directory replacement.

## identity tool
To make source files anonymous, run `./bin/anonymize_dirs` upon a directory. This tool accepts three flags (each with one argument), allowing the user to specify what should be replaced. Output files are placed in `moss_data/anonymized`.

`-n <f>` case-insensitively replaces all strings that occur in the whitespace-delimited file `f`.

`-t <f>` case-insensitively replaces all strings that occur in the first column of the CSV file `f` with the values in the second column. For example, if `f` contains `foo,bar`, all instances of `foo` will be replaced with `bar`.

`-r <substitution>` takes a regular expression substitution in `s/pattern/replace/` form, where pattern is a Perl-style regular expression and `replace` is a string. Currently, `replace` is inserted verbatim, but in the future it might be extended to perform some standard substitutions if necessary.


## plagiarism detection
To run the plagiarism detection system, call the provided `moss` script with a directory containing source files as an argument: `./moss source_dir`.
By default, the script displays an output summary using `less`, but an optional `-o` argument can be used to write output to a file: `./moss source_dir -o results.out`. To output an HTML table rather than plain text, pass the `-h` flag.
